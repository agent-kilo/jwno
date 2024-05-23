(use jw32/_winbase)
(use jw32/_winuser)
(use jw32/_processthreadsapi)
(use jw32/_securitybaseapi)
(use jw32/_combaseapi)
(use jw32/_shobjidl_core)
(use jw32/_winnt)
(use jw32/_handleapi)
(use jw32/_uiautomation)
(use jw32/_dwmapi)
(use jw32/_errhandlingapi)
(use jw32/_util)

(use ./uia)
(use ./util)

(import ./log)


(defmacro rect-width [rect]
  ~(- (in ,rect :right) (in ,rect :left)))


(defmacro rect-height [rect]
  ~(- (in ,rect :bottom) (in ,rect :top)))


(defmacro rect-size [rect]
  ~[(- (in ,rect :right) (in ,rect :left))
    (- (in ,rect :bottom) (in ,rect :top))])


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


(defn tree-node-add-child [self child]
  (let [children (in self :children)
        old-parent (in child :parent)]
    (cond
      (empty? children)
      (do
        (put child :parent self)
        (array/push children child)
        # This is the only child, activate it to avoid inconsistent states
        (put self :current-child child))

      (not= (in child :type) (in (first children) :type))
      (error "cannot mix different types of children")

      true
      (do
        (put child :parent self)
        (array/push children child)))

    # Do the removal after a successful insertion, so
    # that we won't end up in an inconsistent state
    (when old-parent
      (:remove-child old-parent child))))


(defn tree-node-remove-child [self child]
  (def current-child (in self :current-child))
  (when (= child current-child)
    (def next-child (:get-next-child self child))
    (if (= child next-child)
      (put self :current-child nil) # There's no other child
      (put self :current-child next-child)))
  (put self :children
     (filter |(not= $ child) (in self :children))))


(defn tree-node-get-all-windows [self]
  (def children (in self :children))
  (cond
    (or (nil? children) (empty? children))
    []

    (= :window (in (first children) :type))
    (slice (in self :children))

    true # children are other container nodes
    (let [offsprings @[]]
      (each c children
        (array/push offsprings ;(:get-all-windows c)))
      offsprings)))


(defn tree-node-get-current-window [self]
  (cond
    (= :window (in self :type))
    self

    (nil? (in self :current-child))
    nil

    true
    (:get-current-window (in self :current-child))))


(defn tree-node-get-current-frame [self]
  (def children (in self :children))
  (def current-child (in self :current-child))

  (cond
    (= :window (in self :type))
    (error "invalid operation")

    (empty? children)
    self

    (= :window (in (first children) :type))
    self

    (not (nil? current-child))
    (:get-current-frame current-child)

    true
    # There are children, but no current-child
    (error "inconsistent states for frame tree")))


(defn tree-node-get-first-frame [self]
  (def children (in self :children))

  (cond
    (= :window (in self :type))
    (error "invalid operation")

    (empty? children)
    self

    (= :window (in (first children) :type))
    self

    true
    (:get-first-frame (first children))))


(defn tree-node-get-last-frame [self]
  (def children (in self :children))

  (cond
    (= :window (in self :type))
    (error "invalid operation")

    (empty? children)
    self

    (= :window (in (first children) :type))
    self

    true
    (:get-last-frame (last children))))


(defn tree-node-find-hwnd [self hwnd]
  (cond
    (= :window (in self :type))
    (if (= hwnd (in self :hwnd))
      self
      nil)

    true
    (do
      (var found nil)
      (each c (in self :children)
        (when-let [win-found (:find-hwnd c hwnd)]
          (set found win-found)
          (break)))
      found)))


(defn tree-node-purge-windows [self pred]
  (def children (in self :children))

  (cond
    (= :window (in self :type))
    (error "invalid operation")

    (empty? children)
    @[]

    (= :window (in (first children) :type))
    (let [[alive dead] (reduce
                         (fn [[a d] w]
                           (if (pred w)
                             (array/push d w)
                             (array/push a w))
                           [a d])
                         [@[] @[]]
                         children)]
      (put self :children alive)
      (cond
        (empty? alive)
        (put self :current-child nil)

        (in self :current-child)
        (let [current-child (in self :current-child)]
          (each dw dead
            (when (= dw current-child)
              # The previous active child is dead, fill in a new one
              (put self :current-child (first alive))
              (break))))

        true # There are children, but none of them is active
        (error "inconsistent states for frame tree"))
      dead)

    true # children are other container nodes
    (let [dead @[]]
      (each c children
        (array/push dead ;(:purge-windows c pred)))
      dead)))


(defn tree-node-get-layout [self]
  (if (or (nil? self) (= :layout (in self :type)))
    self
    (tree-node-get-layout (in self :parent))))


(def- tree-node-proto
  @{:activate tree-node-activate
    :get-next-child tree-node-get-next-child
    :get-prev-child tree-node-get-prev-child
    :get-next-sibling tree-node-get-next-sibling
    :get-prev-sibling tree-node-get-prev-sibling
    :add-child tree-node-add-child
    :remove-child tree-node-remove-child
    :get-all-windows tree-node-get-all-windows
    :get-current-window tree-node-get-current-window
    :get-current-frame tree-node-get-current-frame
    :get-first-frame tree-node-get-first-frame
    :get-last-frame tree-node-get-last-frame
    :find-hwnd tree-node-find-hwnd
    :purge-windows tree-node-purge-windows
    :get-layout tree-node-get-layout})


(defn tree-node [node-type &opt parent children &keys extra-fields]
  (let [node (table/setproto
              @{:parent parent
                :children @[]
                :type node-type}
              tree-node-proto)]
    (when children
      (each c children
        (:add-child node c)
        (put c :parent node)))
    (eachp [k v] extra-fields
      (put node k v))
    node))


######### Window object #########

(def- window-proto
  (table/setproto
   @{}
   tree-node-proto))


(defn window [hwnd &opt parent]
  (let [node (tree-node :window parent nil
                        :hwnd hwnd
                        :tags @{})]
    (table/setproto node window-proto)))


######### Frame object #########

# Forward declarations
(var frame nil)
(var frame-proto nil)
(var vertical-frame-proto nil)
(var horizontal-frame-proto nil)


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

  (let [[width height] (rect-size (in self :rect))]
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


(defn frame-flatten [self]
  (def cur-window (:get-current-window self))
  (def all-windows (:get-all-windows self))
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
          [old-width old-height] (rect-size old-rect)]
      (def calc-fn
        (cond
          (= horizontal-frame-proto (table/getproto self))
          (fn [sub-fr _i]
            (let [w (rect-width (in sub-fr :rect))
                  wr (/ w old-width)
                  sub-dw (math/floor (* wr dw))]
              (+ w sub-dw)))

          (= vertical-frame-proto (table/getproto self))
          (fn [sub-fr _i]
            (let [h (rect-height (in sub-fr :rect))
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
      @{:split frame-split
        :flatten frame-flatten
        :transform frame-transform}
      tree-node-proto))


(varfn frame [rect &opt parent children]
  (default children @[])
  (let [node (tree-node :frame parent children
                        :rect rect)]
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


(defn- find-closest-child [children reference rect-key]
  (var found (first children))
  (var min-dist (math/abs (- (get-in found [:rect rect-key]) reference)))
  (each child (slice children 1)
    (def dist (math/abs (- (get-in child [:rect rect-key]) reference)))
    (when (< dist min-dist)
      (set min-dist dist)
      (set found child)))
  found)


(defn layout-get-adjacent-frame-impl-descent [self orig-node node dir]
  (def children (in node :children))
  (def {:top orig-top :left orig-left} (in orig-node :rect))

  (cond
    (empty? children)
    node

    (= :window (get-in children [0 :type]))
    node

    (= :frame (get-in children [0 :type]))
    (layout-get-adjacent-frame-impl-descent self
                                            orig-node
                                            (case [dir (table/getproto node)]
                                              [:left horizontal-frame-proto]
                                              (last children)

                                              [:left vertical-frame-proto]
                                              (find-closest-child children orig-top :top)

                                              [:right horizontal-frame-proto]
                                              (first children)

                                              [:right vertical-frame-proto]
                                              (find-closest-child children orig-top :top)

                                              [:up horizontal-frame-proto]
                                              (find-closest-child children orig-left :left)

                                              [:up vertical-frame-proto]
                                              (last children)

                                              [:down horizontal-frame-proto]
                                              (find-closest-child children orig-left :left)

                                              [:down vertical-frame-proto]
                                              (first children))
                                            dir)

    true
    (error "inconsistent states for frame tree")))


(defn layout-get-adjacent-frame-impl-ascent [self orig-node node dir]
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
      (layout-get-adjacent-frame-impl-descent self orig-node adj-fr dir)
      (if (= self (in node :parent))
        nil
        (layout-get-adjacent-frame-impl-ascent self orig-node (in node :parent) dir)))))


(defn layout-get-adjacent-frame [self node dir]
  (layout-get-adjacent-frame-impl-ascent self node node dir))


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
          [fr-width fr-height] (rect-size (in fr :rect))
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
        [old-width old-height] (rect-size (in fr :rect))
        [new-width new-height] (rect-size new-rect)
        [parent-width parent-height] (rect-size parent-rect)
        dw (- new-width old-width)
        dh (- new-height old-height)
        avail-h (- parent-height old-height)
        avail-w (- parent-width old-width)]
    (cond
      (= vertical-frame-proto (table/getproto parent))
      (let [new-rects (:calculate-sub-rects
                         parent
                         (fn [sib-fr _i]
                           (def sib-height (rect-height (in sib-fr :rect)))
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
                           (def sib-width (rect-width (in sib-fr :rect)))
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


(defn layout-close-frame [self fr]
  (cond
    (= self (in fr :parent))
    # This is a toplevel frame, which tracks the monitor
    # geometries and cannot be closed
    nil

    (or (empty? (in fr :children))
        (= :window (get-in fr [:children 0 :type])))
    (let [parent (in fr :parent)
          all-children (in fr :children)
          all-siblings (in parent :children)
          [fr-width fr-height] (rect-size (in fr :rect))
          [parent-width parent-height] (rect-size (in parent :rect))]
      (if (> (length all-siblings) 2)
        (do
          (:remove-child parent fr)

          (def calc-fn
            (cond
              (= horizontal-frame-proto (table/getproto parent))
              (let [rest-width (- parent-width fr-width)]
                (fn [sib-fr _]
                  (let [sib-width (rect-width (in sib-fr :rect))
                        ratio (/ sib-width rest-width)]
                    (math/floor (* ratio parent-width)))))

              (= vertical-frame-proto (table/getproto parent))
              (let [rest-height (- parent-height fr-height)]
                (fn [sib-fr _]
                  (let [sib-height (rect-height (in sib-fr :rect))
                        ratio (/ sib-height rest-height)]
                    (math/floor (* ratio parent-height)))))))
          (def new-rects (:calculate-sub-rects parent calc-fn))
          (map (fn [sib-fr rect]
                 (:transform sib-fr rect))
               (in parent :children)
               new-rects)

          (def cur-frame (:get-current-frame parent))
          (each child all-children
            (put child :parent nil)
            (:add-child cur-frame child)))

        (do
          # When fr is closed, the parent will only have a single child.
          # Remove that child too, and move all children to the parent,
          # so that the tree stays consistent.
          (def sibling (:get-next-sibling fr))
          (def sibling-rect (in sibling :rect))
          (def [sibling-width sibling-height] (rect-size sibling-rect))

          (def to-frame (:get-current-frame sibling))
          (each child all-children
            (put child :parent nil)
            (:add-child to-frame child))

          (put parent :children @[])
          (table/setproto parent (table/getproto sibling)) # reset horizontal/vertical split states
          (each child (in sibling :children)
            (put child :parent nil)
            (:add-child parent child))
          (put parent :current-child (in sibling :current-child))

          (cond
            (empty? (in parent :children))
            nil

            (= :window (get-in parent [:children 0 :type]))
            # Wait for retile to actually resize the windows
            nil

            (= :frame (get-in parent [:children 0 :type]))
            (do
              (def calc-fn
                (cond
                  (= horizontal-frame-proto (table/getproto parent))
                  (fn [sub-fr _]
                    (let [sub-width (rect-width (in sub-fr :rect))
                          ratio (/ sub-width sibling-width)]
                      (math/floor (* ratio parent-width))))

                  (= vertical-frame-proto (table/getproto parent))
                  (fn [sub-fr _]
                    (let [sub-height (rect-height (in sub-fr :rect))
                          ratio (/ sub-height sibling-height)]
                      (math/floor (* ratio parent-height))))))

              (def new-rects (:calculate-sub-rects parent calc-fn))
              (map (fn [sib-fr rect]
                     (:transform sib-fr rect))
                   (in parent :children)
                   new-rects))))))

    (= :frame (get-in fr [:children 0 :type]))
    (error "cannot close frames containing sub-frames"))

  self)


(def- layout-proto
  (table/setproto
   @{:enumerate-frame layout-enumerate-frame
     :get-adjacent-frame layout-get-adjacent-frame
     :resize-frame layout-resize-frame
     :balance-frames layout-balance-frames
     :close-frame layout-close-frame}
   tree-node-proto))


(defn layout [id &opt parent children]
  (default children @[])
  (def layout-obj (tree-node :layout parent children
                             :id id))
  (table/setproto layout-obj layout-proto))


######### Virtual desktop container object #########

(defn vdc-find-frame-for-window [self win]
  (def wm (in self :window-manager))
  (def vd (:get-hwnd-virtual-desktop wm (in win :hwnd)))
  (unless vd
    (break nil))

  (var layout-found nil)
  (each lo (in self :children)
    (when (= (in lo :id) vd)
      (set layout-found lo)
      (break)))

  (def lo
    (if layout-found
      layout-found
      (let [new-layout (:new-layout wm vd)]
        (:add-child self new-layout)
        new-layout)))
  (:get-current-frame lo))


(def- virtual-desktop-container-proto
  (table/setproto
   @{:find-frame-for-window vdc-find-frame-for-window}
   tree-node-proto))


(defn virtual-desktop-container [wm &opt children]
  (default children @[])
  (def vdc-obj (tree-node :virtual-desktop-container nil children
                          :window-manager wm))
  (table/setproto vdc-obj virtual-desktop-container-proto))


######### Window manager object #########

(defn- calc-centered-coords [win-rect fr-rect fit]
  (def [fr-width fr-height] (rect-size fr-rect))
  (def [win-width win-height] (rect-size win-rect))

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
  (let [uia-man (in self :uia-manager)
        uia-com (in uia-man :com)
        hwnd (in win :hwnd)
        rect (in fr :rect)]
    (log/debug "transforming window: %n, rect = %n" hwnd rect)
    (log/debug "fr = %n" fr)
    (try
      (with-uia [cr (:CreateCacheRequest uia-com)]
        (:AddPattern cr UIA_TransformPatternId)
        (:AddPattern cr UIA_WindowPatternId)
        (:AddProperty cr UIA_TransformCanMovePropertyId)
        (:AddProperty cr UIA_TransformCanResizePropertyId)
        (:AddProperty cr UIA_BoundingRectanglePropertyId)

        (with-uia [uia-win (:ElementFromHandleBuildCache uia-com hwnd cr)]
          (with-uia [pat (:GetCachedPatternAs uia-win UIA_TransformPatternId IUIAutomationTransformPattern)]
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
                  (:Resize pat ;(rect-size rect))))))))
      ((err fib)
       # XXX: Don't manage a window which cannot be transformed?
       (log/error "window transformation failed for %n: %n\n%s"
                  (in win :hwnd)
                  err
                  (get-stack-trace fib))))))


(defn wm-set-hwnd-alpha [self hwnd alpha]
  (def ex-style (GetWindowLong hwnd GWL_EXSTYLE))
  (SetWindowLong hwnd GWL_EXSTYLE (unsigned-to-signed-32 (bor ex-style WS_EX_LAYERED)))
  (SetLayeredWindowAttributes hwnd 0 alpha LWA_ALPHA))


(defn wm-hwnd-process-elevated? [self hwnd]
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


(defn wm-jwno-process-elevated? [self]
  (with [[ret token]
         (OpenProcessToken (GetCurrentProcess) TOKEN_QUERY)
         (fn [[_ token]] (CloseHandle token))]
    (when (= 0 ret)
      (break false))
    (def [_gti-ret elevated] (GetTokenInformation token TokenElevation))
    elevated))


(defn wm-get-pid-path [self pid]
  (with [proc
         (OpenProcess (bor PROCESS_QUERY_INFORMATION
                           PROCESS_VM_READ)
                      true
                      pid)
         CloseHandle]
    (log/debug "proc = %n" proc)
    (QueryFullProcessImageName proc 0)))


(defn wm-get-hwnd-path [self hwnd]
  (def [_tid pid] (GetWindowThreadProcessId hwnd))
  (when (= (int/u64 0) pid)
    (break nil))

  (def path (wm-get-pid-path self pid))

  (var uwp-pid nil)
  (when (and (not (nil? path))
             (string/has-suffix? "ApplicationFrameHost.exe" path))
    # Executables for UWP apps live somewhere else
    (EnumChildWindows hwnd
                      (fn [child-hwnd]
                        (def [_tid child-pid] (GetWindowThreadProcessId child-hwnd))
                        (when (and (not= (int/u64 0) child-pid)
                                   (not= pid child-pid))
                          (set uwp-pid child-pid)
                          (break FALSE))
                        TRUE)))
  (if uwp-pid
    (wm-get-pid-path self uwp-pid)
    path))


(defn wm-get-hwnd-virtual-desktop [self hwnd]
  (try
    (:GetWindowDesktopId (in self :vdm-com) hwnd)
    ((err fib)
     (log/debug "GetWindowDesktopId failed: %n\n%s"
                err
                (get-stack-trace fib))
     nil)))


(defn wm-should-manage-hwnd? [self hwnd? &opt uia-win?]
  (def uia-man (in self :uia-manager))
  (def [hwnd uia-win]
    (cond
      (nil? uia-win?)
      (if (or (nil? hwnd?) (null? hwnd?))
        (error "invalid hwnd and uia-win")
        [hwnd? (try
                 (:ElementFromHandleBuildCache (in uia-man :com)
                                               hwnd?
                                               (in uia-man :focus-cr))
                 ((err fib)
                  (log/debug "ElementFromHandleBuildCache failed: %n\n%s"
                             err
                             (get-stack-trace fib))
                  nil))])

      (or (nil? hwnd?) (null? hwnd?))
      [(try
         (:get_CachedNativeWindowHandle uia-win?)
         ((err fib)
          (log/debug "get_CachedNativeWindowHandle failed: %n\n%s"
                     err
                     (get-stack-trace fib))
          nil))
       uia-win?]

      true
      [hwnd? uia-win?]))

  (def exe-path
    (unless (nil? hwnd)
      (wm-get-hwnd-path self hwnd)))

  (def desktop-id
    (unless (nil? hwnd)
      (wm-get-hwnd-virtual-desktop self hwnd)))

  (def result
    (cond
      (nil? uia-win)
      # ElementFromHandleBuildCache failed
      false

      (nil? hwnd)
      # get_CachedNativeWindowHandle failed
      false

      (nil? exe-path)
      # wm-get-hwnd-path failed
      false

      (nil? desktop-id)
      # wm-get-hwnd-virtual-desktop failed
      false

      true
      (:call-filter-hook (in self :hook-manager) :filter-window hwnd uia-win exe-path desktop-id)))

  (when (and (nil? uia-win?)
             (not (nil? uia-win)))
    # uia-win is constructed locally in this case, release it.
    (:Release uia-win))

  result)


(defn wm-add-hwnd [self hwnd]
  (log/debug "new window: %n" hwnd)

  (def exe-path (wm-get-hwnd-path self hwnd))
  (unless exe-path
    # The window disappeared just before wm-get-hwnd-path
    (break nil))

  (def new-win (window hwnd))
  (def uia-man (in self :uia-manager))
  (def hwnd-still-valid
    (with-uia [uia-win (try
                         (:ElementFromHandleBuildCache (in uia-man :com) hwnd (in uia-man :focus-cr))
                         ((err fib)
                          (log/debug "ElementFromHandleBuildCache failed: %n\n%s"
                                     err
                                     (get-stack-trace fib))
                          nil))]
      (if (nil? uia-win)
        # The window may have disappeared between (wm-should-manage-hwnd? ...) and (wm-add-hwnd ...)
        false
        (do
          (:call-hook (in self :hook-manager) :new-window new-win uia-win exe-path)
          true))))

  (when (not hwnd-still-valid)
    (break nil))

  (def tags (in new-win :tags))

  # TODO: floating windows?

  (def frame-found
    (if-let [override-frame (in tags :frame)]
      (do 
        (put tags :frame nil) # Clear the tag, in case the frame got invalidated later
        override-frame)
      (:find-frame-for-window (in self :root) new-win)))

  (:add-child frame-found new-win)
  (wm-transform-window self new-win frame-found)
  new-win)


(defn wm-focus-changed [self]
  # XXX: If the focus change is caused by a closing window, that
  # window may still be alive, so it won't be purged immediately.
  # Maybe I shoud check the hwnds everytime a window is manipulated?
  (def dead (:purge-windows (in self :root) |(not (:hwnd-alive? self (in $ :hwnd)))))
  (each dw dead
    (:call-hook (in self :hook-manager) :dead-window dw))
  (log/debug "purged %n dead windows" (length dead))

  (def uia-man (in self :uia-manager))
  (def hwnd-to-manage
    (with-uia [uia-win (:get-focused-window uia-man)]
      (when (nil? uia-win)
        (log/debug "No focused window")
        (break nil))
      
      (def hwnd (:get_CachedNativeWindowHandle uia-win))

      (when-let [win (:find-hwnd (in self :root) hwnd)]
        #Already managed
        (:activate win)
        (break nil))

      (when (not (wm-should-manage-hwnd? self hwnd uia-win))
        (log/debug "Ignoring window: %n" hwnd)
        (break nil))

      hwnd))

  (when hwnd-to-manage
    (if-let [new-win (wm-add-hwnd self hwnd-to-manage)]
      (:activate new-win)))
  self)


(defn wm-window-opened [self hwnd]
  (when-let [win (:find-hwnd (in self :root) hwnd)]
    (log/debug "window-opened event for managed window: %n" hwnd)
    (break self))

  (when (not (wm-should-manage-hwnd? self hwnd))
    (log/debug "Ignoring window: %n" hwnd)
    (break self))

  (wm-add-hwnd self hwnd)
  self)


(defn wm-activate [self node]
  (when node
    (:activate node))

  (def uia-man (in self :uia-manager))
  (def root (in uia-man :root))
  (def root-hwnd (:get_CachedNativeWindowHandle root))

  (def hwnd
    (cond
      (nil? node)
      root-hwnd

      (= :window (in node :type))
      (in node :hwnd)

      (or (= :frame (in node :type))
          (= :layout (in node :type)))
      (if-let [cur-win (:get-current-window node)]
        (in cur-win :hwnd)
        root-hwnd)))

  (log/debug "setting focus to window: %n" hwnd)
  (if (= hwnd root-hwnd)
    (SetForegroundWindow hwnd) # The UIA SetFocus method doesn't work for the desktop window
    (:set-focus-to-window uia-man hwnd))

  self)


(defn wm-retile [self &opt fr]
  (cond
    (nil? fr)
    # Retile the whole tree
    (wm-retile self (in self :root))

    true
    (cond
      (empty? (in fr :children))
      nil

      (= :window (get-in fr [:children 0 :type]))
      (each w (in fr :children)
        (wm-transform-window self w fr))

      (or (= :frame (get-in fr [:children 0 :type]))
          (= :layout (get-in fr [:children 0 :type])))
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


(defn wm-new-layout [self id]
  (def [work-areas main-idx] (wm-enumerate-monitors self))
  (def new-layout (layout id nil (map |(frame $) work-areas)))
  (def to-activate (or main-idx 0))
  (:activate (get-in new-layout [:children to-activate]))
  new-layout)


(defn wm-close-hwnd [self hwnd]
  (PostMessage hwnd WM_CLOSE 0 0))


(defn wm-hwnd-alive? [self hwnd]
  (and (not= FALSE (IsWindow hwnd))
       (not= FALSE (IsWindowVisible hwnd))))


(defn wm-destroy [self]
  (def {:vdm-com vdm-com} self)
  (:Release vdm-com))


(def- window-manager-proto
  @{:focus-changed wm-focus-changed
    :window-opened wm-window-opened

    :transform-window wm-transform-window
    :retile wm-retile
    :activate wm-activate

    :add-hwnd wm-add-hwnd
    :get-hwnd-virtual-desktop wm-get-hwnd-virtual-desktop
    :should-manage-hwnd? wm-should-manage-hwnd?
    :get-hwnd-path wm-get-hwnd-path
    :set-hwnd-alpha wm-set-hwnd-alpha
    :close-hwnd wm-close-hwnd
    :hwnd-alive? wm-hwnd-alive?
    :hwnd-process-elevated? wm-hwnd-process-elevated?

    :get-pid-path wm-get-pid-path
    :enumerate-monitors wm-enumerate-monitors
    :jwno-process-elevated? wm-jwno-process-elevated?

    :new-layout wm-new-layout

    :destroy wm-destroy})


(defn check-valid-uia-window [uia-win]
  (is-valid-uia-window? uia-win))


(defn default-window-filter [hwnd uia-win exe-path wm]
  (cond
    (not (is-valid-uia-window? uia-win))
    false

    # Exclude cloaked windows
    (not= 0 (try
              (DwmGetWindowAttribute hwnd DWMWA_CLOAKED)
              ((err fib)
               (log/debug "DwmGetWindowAttribute failed: %n\n%s"
                          err
                          (get-stack-trace fib))
               (if (= err E_HANDLE)
                 # The hwnd got invalidated before we checked it,
                 # assume the window is closed. Return an arbitrary
                 # flag so that the filter returns early.
                 0xffff
                 0))))
    false

    # Exclude windows we are not privileged enough to manage
    (and (not (:jwno-process-elevated? wm))
         (:hwnd-process-elevated? wm hwnd))
    false

    true))


(defn window-manager [uia-man hook-man]
  (def vdm-com
    (CoCreateInstance CLSID_VirtualDesktopManager
                      nil
                      CLSCTX_INPROC_SERVER
                      IVirtualDesktopManager))
  (def wm-obj
    (table/setproto
     @{:vdm-com vdm-com
       :uia-manager uia-man
       :hook-manager hook-man}
     window-manager-proto))
  (put wm-obj :root (virtual-desktop-container wm-obj))

  (:add-hook hook-man :filter-window
     (fn [hwnd uia-win exe-path _desktop-id]
       (default-window-filter hwnd uia-win exe-path wm-obj)))

  wm-obj)
