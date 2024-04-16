(use jw32/_winbase)
(use jw32/_winuser)
(use jw32/_processthreadsapi)
(use jw32/_securitybaseapi)
(use jw32/_combaseapi)
(use jw32/_winnt)
(use jw32/_handleapi)
(use jw32/_uiautomation)
(use jw32/_dwmapi)
(use jw32/_util)

(import ./uia)
(import ./log)


######### Generic tree node #########

(defn tree-node-activate [self]
  (var child self)
  (var parent (in self :parent))
  (while parent
    (put parent :current-child child)
    (set child parent)
    (set parent (in parent :parent)))
  self)


(defn tree-node-get-next-child [self child]
  (let [all-children (in self :children)
        child-count (length all-children)]
    (if-let [idx (find-index |(= $ child) all-children)]
      (let [next-idx (% (+ idx 1) child-count)]
        (in all-children next-idx))
      (error "inconsistent states for frame tree"))))


(defn tree-node-get-prev-child [self child]
  (let [all-children (in self :children)
        child-count (length all-children)]
    (if-let [idx (find-index |(= $ child) all-children)]
      (let [prev-idx (% (+ child-count idx -1) child-count)]
        (in all-children prev-idx))
      (error "inconsistent states for frame tree"))))


(defn tree-node-get-next-sibling [self]
  (cond
    (nil? (in self :parent))
    nil

    true
    (tree-node-get-next-child (in self :parent) self)))


(defn tree-node-get-prev-sibling [self]
  (cond
    (nil? (in self :parent))
    nil

    true
    (tree-node-get-prev-child (in self :parent) self)))


(def- tree-node-proto
  @{:activate tree-node-activate
    :get-next-child tree-node-get-next-child
    :get-prev-child tree-node-get-prev-child
    :get-next-sibling tree-node-get-next-sibling
    :get-prev-sibling tree-node-get-prev-sibling})


(defn tree-node [parent children &keys extra-fields]
  (let [node (table/setproto
              @{:parent parent
                :children children}
              tree-node-proto)]
    (when children
      (each c children
        (put c :parent node)))
    (eachp [k v] extra-fields
      (put node k v))
    node))


######### Window object #########

(defn window-alive? [self]
  (and (not= FALSE (IsWindow (in self :hwnd)))
       (not= FALSE (IsWindowVisible (in self :hwnd)))))


(def- window-proto
  (table/setproto
   @{:alive? window-alive?}
   tree-node-proto))


(defn window [hwnd &opt parent]
  (let [node (tree-node parent nil
                        :type :window
                        :hwnd hwnd
                        :tags @{})]
    (table/setproto node window-proto)))


######### Frame object #########

# Forward declarations
(var frame nil)
(var frame-proto nil)
(var vertical-frame-proto nil)
(var horizontal-frame-proto nil)


(defn frame-remove-child [self child]
  (def current-child (in self :current-child))
  (def next-child
    (when (= child current-child)
      (:get-next-child self child)))
  (put self :children
     (filter |(not= $ child) (in self :children)))
  (when (= child current-child)
    (if (= child next-child)
      (put self :current-child nil) # There's no other child
      (put self :current-child next-child)))
  self)


(defn frame-add-child [self child]
  (let [children (in self :children)
        old-parent (in child :parent)]
    (cond
      (empty? children)
      (do
        (put child :parent self)
        (array/push children child)
        # This is the only child, activate it to avoid inconsistent states
        (put self :current-child child))

      (= (in child :type) :window)
      (if (= (get-in children [0 :type]) :window)
        (do
          (put child :parent self)
          (array/push children child))
        (error "cannot mix frames and windows"))

      (= (in child :type) :frame)
      (if (= (get-in children [0 :type]) :frame)
        (do
          (put child :parent self)
          (array/push children child))
        (error "cannot mix frames and windows")))

    # Do the removal after a successful insertion, so
    # that we won't end up in an inconsistent state
    (when old-parent
      (frame-remove-child old-parent child)))
  self)


(defn frame-split [self direction &opt n ratios]
  (default n 2)
  (default ratios [0.5])
  (if (and (not (empty? (in self :children)))
           (= (get-in self [:children 0 :type]) :frame))
    (error "frame is already split"))
  (if (<= n 1)
    (error "invalid number of sub-frames"))
  (if (< (length ratios) (- n 1))
    (error "not enough ratios provided"))
  (def full-ratios
    (if (> (length ratios) (- n 1))
      (slice ratios 0 (- n 1))
      ratios))

  (let [rect (in self :rect)
        top (in rect :top)
        bottom (in rect :bottom)
        left (in rect :left)
        right (in rect :right)
        height (- bottom top)
        width (- right left)]

    (def new-rects
      (case direction
        :horizontal
        (do
          (table/setproto self horizontal-frame-proto)
          (:calculate-sub-rects self
                                (fn [_ i]
                                  (math/floor (* width (in full-ratios i))))
                                n))

        :vertical
        (do
          (table/setproto self vertical-frame-proto)
          (:calculate-sub-rects self
                                (fn [_ i]
                                  (math/floor (* height (in full-ratios i))))
                                n))))

    (def new-frames (map |(frame $ self) new-rects))

    (def old-children (in self :children))
    (def old-active-child (in self :current-child))
    (put self :children new-frames)
    (def first-sub-frame (in new-frames 0))
    # XXX: Always activate the first sub-frame by default
    (put self :current-child first-sub-frame)
    # XXX: Move all window children to the first sub-frame by default
    (each win old-children
      (:add-child first-sub-frame win))
    (put first-sub-frame :current-child old-active-child))

  self)


(defn frame-get-all-windows [self]
  (cond
    (empty? (in self :children))
    []

    (= :window (get-in self [:children 0 :type]))
    (slice (in self :children))

    (= :frame (get-in self [:children 0 :type]))
    (let [offsprings @[]]
      (each fr (in self :children)
        (array/push offsprings ;(frame-get-all-windows fr)))
      offsprings)))


(defn frame-get-current-window [self]
  (var parent self)
  (var cur-child (in parent :current-child))
  (while (and cur-child
              (not= :window (in cur-child :type)))
    (set parent cur-child)
    (set cur-child (in parent :current-child)))
  cur-child)


(defn frame-flatten [self]
  (def cur-window (frame-get-current-window self))
  (def all-windows (frame-get-all-windows self))
  (each w all-windows
    (put w :parent self))
  (put self :children all-windows)
  # XXX: Default to the active window before flattening,
  # or simply the first child
  (put self :current-child
     (cond
       (not (nil? cur-window))
       cur-window

       (empty? all-windows)
       nil

       true
       (in all-windows 0)))
  (table/setproto self frame-proto) # Clear vertical/horizontal settings
  self)


(defn frame-find-window [self hwnd]
  (var found nil)
  (each child (in self :children)
    (cond
      (and (= :window (in child :type))
           (= hwnd (in child :hwnd)))
      (do
        (set found child)
        (break))

      (= :frame (in child :type))
      (do
        (set found (frame-find-window child hwnd))
        (if found (break)))))
  found)


(defn frame-find-frame-for-window [self win]
  (cond
    (empty? (in self :children))
    self

    (= :window (get-in self [:children 0 :type]))
    self

    (in self :current-child)
    (frame-find-frame-for-window (in self :current-child) win)

    true
    (error "inconsistent states for frame tree")))


(defn frame-get-current-frame [self]
  (frame-find-frame-for-window self nil))


(defn frame-purge-windows [self]
  (cond
    (empty? (in self :children))
    @[]

    (= :frame (get-in self [:children 0 :type]))
    (let [dead @[]]
      (each f (in self :children)
        (array/push dead ;(frame-purge-windows f)))
      dead)

    (= :window (get-in self [:children 0 :type]))
    (let [[alive dead] 
          (reduce
            (fn [[a d] w]
              (if (:alive? w)
                (array/push a w)
                (array/push d w))
              [a d])
            [@[] @[]]
            (in self :children))]
      (put self :children alive)
      (cond
        (empty? alive)
        (put self :current-child nil)

        (in self :current-child)
        (each dw dead
          (when (= dw (in self :current-child))
            # The previous active child is dead, fill in a new one
            (put self :current-child (in alive 0))
            (break)))

        true # There are children, but none of them is active
        (error "inconsistent states for frame tree"))
      dead)))


(defn frame-get-first-frame [self]
  (var cur-fr self)
  (while true
    (cond
      (empty? (in cur-fr :children))
      (break)

      (= :window (get-in cur-fr [:children 0 :type]))
      (break))
    (set cur-fr (get-in cur-fr [:children 0])))
  cur-fr)


(defn frame-get-last-frame [self]
  (var cur-fr self)
  (while true
    (cond
      (empty? (in cur-fr :children))
      (break)

      (= :window (get-in cur-fr [:children 0 :type]))
      (break))
    (def child-count (length (in cur-fr :children)))
    (set cur-fr (get-in cur-fr [:children (- child-count 1)])))
  cur-fr)


(defn frame-transform [self new-rect]
  (def old-rect (in self :rect))
  (put self :rect new-rect)

  (def all-children (in self :children))
  (cond
    (empty? all-children)
    (break self)

    (= :window (get-in all-children [0 :type]))
    # Do not actually resize the windows until next retile
    (break self)

    (= :frame (get-in all-children [0 :type]))
    (let [dx (- (in new-rect :left) (in old-rect :left))
          dy (- (in new-rect :top) (in old-rect :top))
          dw (+ (- dx)
                (- (in new-rect :right)
                   (in old-rect :right)))
          dh (+ (- dy)
                (- (in new-rect :bottom)
                   (in old-rect :bottom)))
          old-width (- (in old-rect :right) (in old-rect :left))
          old-height (- (in old-rect :bottom) (in old-rect :top))]
      (def calc-fn
        (cond
          (= horizontal-frame-proto (table/getproto self))
          (fn [sub-fr _i]
            (let [sub-rect (in sub-fr :rect)
                  w (- (in sub-rect :right) (in sub-rect :left))
                  wr (/ w old-width)
                  sub-dw (math/floor (* wr dw))]
              (+ w sub-dw)))

          (= vertical-frame-proto (table/getproto self))
          (fn [sub-fr _i]
            (let [sub-rect (in sub-fr :rect)
                  h (- (in sub-rect :bottom) (in sub-rect :top))
                  hr (/ h old-height)
                  sub-dh (math/floor (* hr dh))]
              (+ h sub-dh)))))
      (def new-rects (:calculate-sub-rects self calc-fn))
      (map (fn [sub-fr rect]
             (frame-transform sub-fr rect))
           all-children
           new-rects)
      self)))


(set frame-proto
     (table/setproto
      @{:add-child frame-add-child
        :remove-child frame-remove-child
        :split frame-split
        :flatten frame-flatten
        :transform frame-transform
        :find-window frame-find-window
        :find-frame-for-window frame-find-frame-for-window
        :purge-windows frame-purge-windows
        :get-current-window frame-get-current-window
        :get-all-windows frame-get-all-windows
        :get-current-frame frame-get-current-frame
        :get-first-frame frame-get-first-frame
        :get-last-frame frame-get-last-frame}
      tree-node-proto))


(varfn frame [rect &opt parent children]
  (default children @[])
  (let [node (tree-node parent children
                        :type :frame
                        :rect rect
                        :current-child (if-not (empty? children)
                                         (in children 0)))]
    (table/setproto node frame-proto)))


(defn vertical-frame-calculate-sub-rects [self h-fn &opt count]
  (def rect (in self :rect))
  (var cur-y (in rect :top))
  (var bottom (in rect :bottom))
  (def left (in rect :left))
  (def right (in rect :right))
  (def children-type (get-in self [:children 0 :type]))
  (def all-children
    (cond
      # This can be used to do calculation for a frame that contains windows.
      # We only consider sub-frames when calculating.
      (= :window children-type) []
      (= :frame children-type) (in self :children)))
  (def child-count (if (nil? count)
                     (length all-children)
                     # The caller can override child-count to calculate rects for new children
                     count))
  (def new-rects @[])
  (for i 0 child-count
    (def is-last (>= i (- child-count 1)))
    (def new-h (if is-last
                 (- bottom cur-y) # To avoid rounding error
                 (h-fn (get all-children i) i)))
    (when (<= new-h 0)
      (error "cannot create zero-height frames"))
    (array/push new-rects {:left left
                           :right right
                           :top cur-y
                           :bottom (+ cur-y new-h)})
    (+= cur-y new-h))
  new-rects)


(set vertical-frame-proto
  (table/setproto
   @{:calculate-sub-rects vertical-frame-calculate-sub-rects}
   frame-proto))


(defn horizontal-frame-calculate-sub-rects [self w-fn &opt count]
  (def rect (in self :rect))
  (var cur-x (in rect :left))
  (var right (in rect :right))
  (def top (in rect :top))
  (def bottom (in rect :bottom))
  (def children-type (get-in self [:children 0 :type]))
  (def all-children
    (cond
      # This can be used to do calculation for a frame that contains windows.
      # We only consider sub-frames when calculating.
      (= :window children-type) []
      (= :frame children-type) (in self :children)))
  (def child-count (if (nil? count)
                     (length all-children)
                     # The caller can override child-count to calculate rects for new children
                     count))
  (def new-rects @[])
  (for i 0 child-count
    (def is-last (>= i (- child-count 1)))
    (def new-w (if is-last
                 (- right cur-x) # To avoid rounding error
                 (w-fn (get all-children i) i)))
    (when (<= new-w 0)
      (error "cannot create zero-width frames"))
    (array/push new-rects {:left cur-x
                           :right (+ cur-x new-w)
                           :top top
                           :bottom bottom})
    (+= cur-x new-w))
  new-rects)


(set horizontal-frame-proto
  (table/setproto
   @{:calculate-sub-rects horizontal-frame-calculate-sub-rects}
   frame-proto))


(defn layout-enumerate-frame [self node dir]
  (let [all-siblings (get-in node [:parent :children])
        sibling-count (length all-siblings)
        fr-idx (if-let [idx (find-index |(= $ node) all-siblings)]
                 idx
                 (error "inconsistent states for frame tree"))
        idx-to-check (case dir
                       :next
                       (if (= self (in node :parent)) # Toplevel frames wrap around
                         (% (+ fr-idx 1) sibling-count)
                         (+ fr-idx 1))
                       :prev
                       (if (= self (in node :parent))
                         (% (+ fr-idx sibling-count -1) sibling-count)
                         (- fr-idx 1)))]
    (cond
      (or (< idx-to-check 0)
          (>= idx-to-check sibling-count))
      # We reached the end of the sub-frame list, go up
      (layout-enumerate-frame self (in node :parent) dir)

      (= dir :next)
      (:get-first-frame (in all-siblings idx-to-check))

      (= dir :prev)
      (:get-last-frame (in all-siblings idx-to-check)))))


(defn layout-get-adjacent-frame [self node dir]
  (let [all-siblings (get-in node [:parent :children])
        fr-idx (if-let [idx (find-index |(= $ node) all-siblings)]
                 idx
                 (error "inconsistent states for frame tree"))]
    (var adj-fr nil)
    (case dir
      :left
      (loop [i :down-to [(- fr-idx 1) 0]]
        (def sibling (in all-siblings i))
        (when (< (get-in sibling [:rect :left]) (get-in node [:rect :left]))
          (set adj-fr sibling)
          (break)))
      :right
      (loop [i :range [(+ fr-idx 1) (length all-siblings)]]
        (def sibling (in all-siblings i))
        (when (> (get-in sibling [:rect :left]) (get-in node [:rect :left]))
          (set adj-fr sibling)
          (break)))
      :up
      (loop [i :down-to [(- fr-idx 1) 0]]
        (def sibling (in all-siblings i))
        (when (< (get-in sibling [:rect :top]) (get-in node [:rect :top]))
          (set adj-fr sibling)
          (break)))
      :down
      (loop [i :range [(+ fr-idx 1) (length all-siblings)]]
        (def sibling (in all-siblings i))
        (when (> (get-in sibling [:rect :top]) (get-in node [:rect :top]))
          (set adj-fr sibling)
          (break))))

    (if adj-fr
      (:get-current-frame adj-fr)
      (if (= self (in node :parent))
        nil
        (layout-get-adjacent-frame self (in node :parent) dir)))))


(defn layout-balance-frames [self &opt fr recursive]
  (default recursive false)
  (cond
    (nil? fr)
    (each toplevel-fr (in self :children)
      (layout-balance-frames self toplevel-fr recursive))

    (empty? (in fr :children))
    nil

    (not= :frame (get-in fr [:children 0 :type]))
    nil

    true
    (let [all-children (in fr :children)
          child-count (length all-children)
          fr-rect (in fr :rect)
          fr-width (- (in fr-rect :right) (in fr-rect :left))
          fr-height (- (in fr-rect :bottom) (in fr-rect :top))
          balanced-len (math/floor (/ (cond
                                        (= vertical-frame-proto (table/getproto fr)) fr-height
                                        (= horizontal-frame-proto (table/getproto fr)) fr-width)
                                      child-count))]
      (def new-rects (:calculate-sub-rects fr (fn [_sub-fr _i] balanced-len)))
      (if recursive
        (map (fn [sub-fr rect]
               (put sub-fr :rect rect)
               (layout-balance-frames self sub-fr recursive))
             all-children
             new-rects)
        (map (fn [sub-fr rect]
               (:transform sub-fr rect))
             all-children
             new-rects)))))


(defn layout-resize-frame [self fr new-rect]
  (when (= self (in fr :parent))
    # This is a toplevel frame, which tracks the monitor
    # geometries and cannot be resized
    (break self))

  (let [parent (in fr :parent)
        all-siblings (in parent :children)
        parent-rect (in parent :rect)
        old-rect (in fr :rect)
        old-width (- (in old-rect :right) (in old-rect :left))
        old-height (- (in old-rect :bottom) (in old-rect :top))
        new-width (- (in new-rect :right) (in new-rect :left))
        new-height (- (in new-rect :bottom) (in new-rect :top))
        parent-width (- (in parent-rect :right) (in parent-rect :left))
        parent-height (- (in parent-rect :bottom) (in parent-rect :top))
        dw (- new-width old-width)
        dh (- new-height old-height)
        avail-h (- parent-height old-height)
        avail-w (- parent-width old-width)]
    (cond
      (= vertical-frame-proto (table/getproto parent))
      (let [new-rects (:calculate-sub-rects
                         parent
                         (fn [sib-fr _i]
                           (def sib-rect (in sib-fr :rect))
                           (def sib-height (- (in sib-rect :bottom) (in sib-rect :top)))
                           (def sib-dh
                             (if (= sib-fr fr)
                               dh
                               (math/floor (* (- dh) (/ sib-height avail-h)))))
                           (+ sib-height sib-dh)))]
        (map (fn [sib-fr rect]
               (:transform sib-fr rect))
             all-siblings
             new-rects)
        (when (not= dw 0)
          (layout-resize-frame self
                               parent
                               {:left (in parent-rect :left)
                                :top (in parent-rect :top)
                                :right (+ dw (in parent-rect :right))
                                :bottom (in parent-rect :bottom)})))
      

      (= horizontal-frame-proto (table/getproto parent))
      (let [new-rects (:calculate-sub-rects
                         parent
                         (fn [sib-fr _i]
                           (def sib-rect (in sib-fr :rect))
                           (def sib-width (- (in sib-rect :right) (in sib-rect :left)))
                           (def sib-dw
                             (if (= sib-fr fr)
                               dw
                               (math/floor (* (- dw) (/ sib-width avail-w)))))
                           (+ sib-width sib-dw)))]
        (map (fn [sib-fr rect]
               (:transform sib-fr rect))
             all-siblings
             new-rects)
        (when (not= dh 0)
          (layout-resize-frame self
                               parent
                               {:left (in parent-rect :left)
                                :top (in parent-rect :top)
                                :right (in parent-rect :right)
                                :bottom (+ dh (in parent-rect :bottom))}))))))


(def- layout-proto
  (table/setproto
   @{:split (fn [&] (error "unsupported operation"))
     :flatten (fn [&] (error "unsupported operation"))
     :transform (fn [&] (error "unsupported operation"))
     :enumerate-frame layout-enumerate-frame
     :get-adjacent-frame layout-get-adjacent-frame
     :resize-frame layout-resize-frame
     :balance-frames layout-balance-frames}
   frame-proto))


(defn layout [&opt children]
  (def layout-obj (frame nil nil children))
  (put layout-obj :type :layout)
  (table/setproto layout-obj layout-proto))


######### Window manager object #########

(defn- calc-centered-coords [win-rect fr-rect fit]
  (def fr-width (- (in fr-rect :right) (in fr-rect :left)))
  (def fr-height (- (in fr-rect :bottom) (in fr-rect :top)))
  (def win-width (- (in win-rect :right) (in win-rect :left)))
  (def win-height (- (in win-rect :bottom) (in win-rect :top)))

  (def x
    (math/floor
     (+ (in fr-rect :left)
        (/ fr-width 2)
        (/ win-width -2))))
  (def y
    (math/floor
     (+ (in fr-rect :top)
        (/ fr-height 2)
        (/ win-height -2))))

  (if fit
    (let [[fitted-x fitted-width] (if (< x (in fr-rect :left))
                                    [(in fr-rect :left) fr-width]
                                    [x win-width])
          [fitted-y fitted-height] (if (< y (in fr-rect :top))
                                     [(in fr-rect :top) fr-height]
                                     [y win-height])]
      [fitted-x fitted-y fitted-width fitted-height])
    [x y win-width win-height]))

(defn wm-transform-window [self win fr]
  (let [uia (in self :uia)
        uia-com (in uia :com)
        hwnd (in win :hwnd)
        rect (in fr :rect)]
    (log/debug "transforming window: %n, rect = %n" hwnd rect)
    (try
      (uia/with-uia [cr (:CreateCacheRequest uia-com)]
        (:AddPattern cr UIA_TransformPatternId)
        (:AddPattern cr UIA_WindowPatternId)
        (:AddProperty cr UIA_TransformCanMovePropertyId)
        (:AddProperty cr UIA_TransformCanResizePropertyId)
        (:AddProperty cr UIA_BoundingRectanglePropertyId)

        (uia/with-uia [uia-win (:ElementFromHandleBuildCache uia-com hwnd cr)]
          (uia/with-uia [pat (:GetCachedPatternAs uia-win UIA_TransformPatternId IUIAutomationTransformPattern)]
            # TODO: restore maximized windows first
            (when (and pat
                       (not= 0 (:get_CachedCanMove pat)))
              (def tags (in win :tags))

              (def no-resize (in tags :no-resize))
              (def no-expand (in tags :no-expand))

              (cond
                (= 0 (:get_CachedCanResize pat))
                (when-let [win-rect (:get_CachedBoundingRectangle uia-win)]
                  # Move the window to the frame's center
                  (def [x y _w _h]
                    (calc-centered-coords win-rect rect false))
                  (:Move pat x y))

                no-resize
                (when-let [win-rect (:get_CachedBoundingRectangle uia-win)]
                  # Move the window to the frame's center
                  (def [x y _w _h]
                    (calc-centered-coords win-rect rect false))
                  (:Move pat x y))

                no-expand
                (when-let [win-rect (:get_CachedBoundingRectangle uia-win)]
                  # Move the window to the frame's center
                  (def [x y w h]
                    (calc-centered-coords win-rect rect true))
                  (:Move pat x y)
                  (:Resize pat w h))

                true
                (do
                  (:Move pat (in rect :left) (in rect :top))
                  (:Resize pat
                           (- (in rect :right) (in rect :left))
                           (- (in rect :bottom) (in rect :top)))))))))
      ((err fib)
       # XXX: Don't manage a window which cannot be transformed?
       (log/error "window transformation failed for %n: %n" (in win :hwnd) err)))))


(defn wm-is-window-process-elevated? [self hwnd]
  (def [_tid pid] (GetWindowThreadProcessId hwnd))
  (when (= (int/u64 0) pid)
    (break false))

  (with [proc
         (OpenProcess PROCESS_QUERY_LIMITED_INFORMATION false pid)
         CloseHandle]
    (with [[ret token]
           (OpenProcessToken proc TOKEN_QUERY)
           (fn [[_ token]] (CloseHandle token))]
      (when (= 0 ret)
        (break false))
      (def [_gti-ret elevated] (GetTokenInformation token TokenElevation))
      elevated)))


(defn wm-is-jwno-process-elevated? [self]
  (with [[ret token]
         (OpenProcessToken (GetCurrentProcess) TOKEN_QUERY)
         (fn [[_ token]] (CloseHandle token))]
    (when (= 0 ret)
      (break false))
    (def [_gti-ret elevated] (GetTokenInformation token TokenElevation))
    elevated))


(defn wm-get-process-path [self pid]
  (with [proc
         (OpenProcess (bor PROCESS_QUERY_INFORMATION
                           PROCESS_VM_READ)
                      true
                      pid)
         CloseHandle]
    (log/debug "proc = %n" proc)
    (QueryFullProcessImageName proc 0)))


(defn wm-get-window-process-path [self hwnd]
  (def [_tid pid] (GetWindowThreadProcessId hwnd))
  (when (= (int/u64 0) pid)
    (break nil))

  (wm-get-process-path self pid))


(defn wm-should-manage? [self hwnd? &opt uia-win?]
  (def uia (in self :uia))
  (def [hwnd uia-win]
    (cond
      (nil? uia-win?)
      (if (or (nil? hwnd?) (null? hwnd?))
        (error "invalid hwnd and uia-win")
        [hwnd? (try
                 (:ElementFromHandleBuildCache (in uia :com) hwnd? (in uia :focus-cr))
                 ((err fib)
                  (log/debug "ElementFromHandleBuildCache failed: %n" err)
                  nil))])

      (or (nil? hwnd?) (null? hwnd?))
      [(try
         (:get_CachedNativeWindowHandle uia-win?)
         ((err fib)
          (log/debug "get_CachedNativeWindowHandle failed: %n" err)
          nil))
       uia-win?]

      true
      [hwnd? uia-win?]))

  (cond
    (nil? uia-win)
    false

    (nil? hwnd)
    (do
      (when (nil? uia-win?)
        (:Release uia-win))
      false)

    true
    (let [result (:call-filter-hook (in self :hook-manager) :filter-window hwnd uia-win)]
      (when (nil? uia-win?)
        (:Release uia-win))
      result)))


(defn wm-add-window [self hwnd]
  (log/debug "new window: %n" hwnd)
  (def new-win (window hwnd))
  (def frame-found (:find-frame-for-window (in self :layout) new-win))
  (:add-child frame-found new-win)
  (:call-hook (in self :hook-manager) :new-window new-win)
  (wm-transform-window self new-win frame-found)
  new-win)


(defn wm-focus-changed [self]
  # XXX: If the focus change is caused by a closing window, that
  # window may still be alive, so it won't be purged immediately.
  # Maybe I shoud check the hwnds everytime a window is manipulated?
  (def dead (:purge-windows (in self :layout)))
  (log/debug "purged %n dead windows" (length dead))

  (def uia (in self :uia))
  (def hwnd-to-manage
    (uia/with-uia [uia-win (:get-focused-window uia)]
      (when (nil? uia-win)
        (log/debug "No focused window")
        (break nil))
      
      (def hwnd (:get_CachedNativeWindowHandle uia-win))

      (when-let [win (:find-window (in self :layout) hwnd)]
        #Already managed
        (:activate win)
        (break nil))

      (when (not (wm-should-manage? self hwnd uia-win))
        (log/debug "Ignoring window: %n" hwnd)
        (break nil))

      hwnd))

  (when hwnd-to-manage
    # TODO: window close events
    (let [new-win (wm-add-window self hwnd-to-manage)]
      (:activate new-win)))
  self)


(defn wm-window-opened [self hwnd]
  (when-let [win (:find-window (in self :layout) hwnd)]
    (log/debug "window-opened event for managed window: %n" hwnd)
    (break self))

  (when (not (wm-should-manage? self hwnd))
    (log/debug "Ignoring window: %n" hwnd)
    (break self))

  (wm-add-window self hwnd)
  self)


(defn wm-activate [self node]
  (when node
    (:activate node))

  (def uia (in self :uia))
  (def root (in uia :root))
  (def root-hwnd (:get_CachedNativeWindowHandle root))

  (def hwnd
    (cond
      (nil? node)
      root-hwnd

      (= :window (in node :type))
      (in node :hwnd)

      (= :frame (in node :type))
      (if-let [cur-win (:get-current-window node)]
        (in cur-win :hwnd)
        root-hwnd)))

  (log/debug "setting focus to window: %n" hwnd)
  (if (= hwnd root-hwnd)
    (SetForegroundWindow hwnd) # The UIA SetFocus method doesn't work for the desktop window
    (:set-focus-to-window uia hwnd))

  self)


(defn wm-retile [self &opt fr]
  (cond
    (nil? fr)
    # Retile the whole tree
    (wm-retile self (in self :layout))

    true
    (cond
      (empty? (in fr :children))
      nil

      (= :window (get-in fr [:children 0 :type]))
      (each w (in fr :children)
        (wm-transform-window self w fr))

      (= :frame (get-in fr [:children 0 :type]))
      (each f (in fr :children)
        (wm-retile self f))))

  self)


(defn wm-enumerate-monitors [self]
  (def work-areas @[])
  (def monitor-info (MONITORINFOEX))
  (var main-idx nil)
  (var idx 0)
  (def enum-ret
    (EnumDisplayMonitors
     nil nil
     (fn [hmon hmdc rect]
       (def ret (GetMonitorInfo hmon monitor-info))
       (if (= FALSE ret)
         (error (string/format "GetMonitorInfo failed for monitor %n" hmon)))
       (array/push work-areas (in monitor-info :rcWork))
       (if (> (band (in monitor-info :dwFlags) MONITORINFOF_PRIMARY) (int/u64 0))
         (set main-idx idx))
       (+= idx 1)
       TRUE)))
  (if (= FALSE enum-ret)
    (error "EnumDisplayMonitors failed"))
  (log/debug "work-areas = %n" work-areas)
  (log/debug "main-idx = %n" main-idx)
  (when (empty? work-areas)
    (error "no monitor found"))
  [work-areas main-idx])


(def- wm-proto
  @{:focus-changed wm-focus-changed
    :window-opened wm-window-opened

    :enumerate-monitors wm-enumerate-monitors
    :should-manage? wm-should-manage?
    :add-window wm-add-window
    :transform-window wm-transform-window
    :retile wm-retile
    :activate wm-activate
    :is-window-process-elevated? wm-is-window-process-elevated?
    :is-jwno-process-elevated? wm-is-jwno-process-elevated?
    :get-process-path wm-get-process-path
    :get-window-process-path wm-get-window-process-path})


(defn init-window-tags [win wm]
  (when-let [win-info (:get-window-info (in wm :uia)
                                        (in win :hwnd))]
    (def {:name name
          :class-name class-name}
      win-info)
    (cond
      # TODO: Generic window rules?
      (= class-name "#32770")
      (put (in win :tags) :no-expand true))))


(defn check-uncloaked-window [hwnd]
  (def cloaked-value
    (try
      (DwmGetWindowAttribute hwnd DWMWA_CLOAKED)
      ((err fib)
       (log/debug "DwmGetWindowAttribute failed: %n" err)
       0)))
  (= 0 cloaked-value))


(defn check-unelevated-window [hwnd wm]
  (or (:is-jwno-process-elevated? wm)
      (not (:is-window-process-elevated? wm hwnd))))


(defn check-valid-uia-window [uia-win]
  (uia/is-valid-uia-window? uia-win))


(defn check-not-pseudo-console-window [uia-win]
  (not= "PseudoConsoleWindow" (:get_CachedClassName uia-win)))


(defn window-manager [uia hook-man]
  (def wm-obj
    (table/setproto
     @{:uia uia
       :hook-manager hook-man}
     wm-proto))

  (def [work-areas main-idx] (:enumerate-monitors wm-obj))
  (put wm-obj :layout
     (layout (map |(frame $) work-areas)))
  (if main-idx
    (:activate (get-in wm-obj [:layout :children main-idx]))
    (:activate (get-in wm-obj [:layout :children 0])))

  (:add-hook hook-man :new-window
     (fn [_hook-name win]
       (init-window-tags win wm-obj)))

  (:add-hook hook-man :filter-window
     (fn [_hook-name hwnd _uia-win]
       (check-uncloaked-window hwnd)))
  (:add-hook hook-man :filter-window
     (fn [_hook-name hwnd _uia-win]
       (check-unelevated-window hwnd wm-obj)))
  (:add-hook hook-man :filter-window
     (fn [_hook-name _hwnd uia-win]
       (check-valid-uia-window uia-win)))
  # TODO: Generic window rules?
  (:add-hook hook-man :filter-window
     (fn [_hook-name _hwnd uia-win]
       (check-not-pseudo-console-window uia-win)))

  wm-obj)
