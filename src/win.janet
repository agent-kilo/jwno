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


# Forward declarations
(var frame nil)
(var frame-proto nil)
(var vertical-frame-proto nil)
(var horizontal-frame-proto nil)


######### Helpers #########

(defmacro rect-width [rect]
  ~(- (in ,rect :right) (in ,rect :left)))


(defmacro rect-height [rect]
  ~(- (in ,rect :bottom) (in ,rect :top)))


(defmacro rect-size [rect]
  ~[(- (in ,rect :right) (in ,rect :left))
    (- (in ,rect :bottom) (in ,rect :top))])


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


(defn- transform-hwnd [hwnd rect uia-man &opt tags]
  (default tags @{})

  (log/debug "transforming window: %n, rect = %n" hwnd rect)

  (def {:com uia-com
        :transform-cr cr}
    uia-man)

  (try
    (with-uia [uia-win (:ElementFromHandleBuildCache uia-com hwnd cr)]
      (with-uia [pat (:GetCachedPatternAs uia-win
                                          UIA_TransformPatternId
                                          IUIAutomationTransformPattern)]
        # TODO: restore maximized windows first
        (when (and pat
                   (not= 0 (:get_CachedCanMove pat)))
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
              (:Resize pat ;(rect-size rect)))))))
    ((err fib)
     # XXX: Don't manage a window which cannot be transformed?
     (log/error "window transformation failed for %n: %n\n%s"
                hwnd
                err
                (get-stack-trace fib)))))


(defn- set-hwnd-alpha [hwnd alpha]
  (def ex-style (GetWindowLong hwnd GWL_EXSTYLE))
  (SetWindowLong hwnd GWL_EXSTYLE (unsigned-to-signed-32 (bor ex-style WS_EX_LAYERED)))
  (SetLayeredWindowAttributes hwnd 0 alpha LWA_ALPHA))


(defn- get-hwnd-alpha [hwnd]
  (if-let [attrs (GetLayeredWindowAttributes hwnd)]
    (in attrs 1)
    (do
      (log/debug "GetLayeredWindowAttributes failed: %n" (GetLastError))
      255)))


(defn- hwnd-process-elevated? [hwnd]
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


(defn- get-pid-path [pid]
  (with [proc
         (OpenProcess (bor PROCESS_QUERY_INFORMATION
                           PROCESS_VM_READ)
                      true
                      pid)
         CloseHandle]
    (log/debug "proc = %n" proc)
    (QueryFullProcessImageName proc 0)))


(defn- get-hwnd-path [hwnd]
  (def [_tid pid] (GetWindowThreadProcessId hwnd))
  (when (= (int/u64 0) pid)
    (break nil))

  (def path (get-pid-path pid))

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
    (get-pid-path uwp-pid)
    path))


(defn- get-hwnd-uia-element [hwnd uia-com &opt cr]
  (try
    (if cr
      (:ElementFromHandleBuildCache uia-com hwnd cr)
      (:ElementFromHandle uia-com hwnd))
    ((err fib)
     (log/debug "ElementFromHandleBuildCache failed: %n\n%s"
                err
                (get-stack-trace fib))
     nil)))


(defn- normalize-hwnd-and-uia-element [hwnd? uia-win? uia-com cr]
  (cond
    (nil? uia-win?)
    (if (or (nil? hwnd?) (null? hwnd?))
      (error "invalid hwnd and uia-win")
      [hwnd? (get-hwnd-uia-element hwnd? uia-com cr)])

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


(defn- get-hwnd-virtual-desktop [hwnd? uia-man vdm-com &opt uia-win?]
  (def [hwnd uia-win]
    (normalize-hwnd-and-uia-element hwnd?
                                    uia-win?
                                    (in uia-man :com)
                                    (in uia-man :focus-cr)))

  (cond
    (nil? uia-win)
    nil

    (nil? hwnd)
    (do
      (when (nil? uia-win?)
        # uia-win is constructed locally, release it
        (:Release uia-win))
      nil)

    true
    (do
      (def desktop-id
        (try
          (:GetWindowDesktopId vdm-com hwnd)
          ((err fib)
           (log/debug "GetWindowDesktopId failed: %n\n%s"
                      err
                      (get-stack-trace fib))
           nil)))

      (def desktop-name
        (with-uia [root-elem (:get-root uia-man uia-win)]
          (if root-elem
            (do
              (when (= root-elem uia-win)
                # So that it won't be freed when leaving with-uia
                (:AddRef uia-win))
              (:get_CachedName root-elem))
            nil)))

      (when (nil? uia-win?)
        # uia-win is constructed locally in this case, release it.
        (:Release uia-win))

      (if (and (nil? desktop-id)
               (nil? desktop-name))
        nil
        {:id desktop-id :name desktop-name}))))


(defn- get-hwnd-info [hwnd? uia-man vdm-com &opt uia-win?]
  (def [hwnd uia-win]
    (normalize-hwnd-and-uia-element hwnd?
                                    uia-win?
                                    (in uia-man :com)
                                    (in uia-man :focus-cr)))
  (def exe-path
    (unless (nil? hwnd)
      (get-hwnd-path hwnd)))
  (def desktop-info
    (unless (nil? hwnd)
      (get-hwnd-virtual-desktop hwnd uia-man vdm-com uia-win)))

  (def ret
    (cond
      (nil? uia-win)
      # normalize-hwnd-and-uia-element failed
      nil

      (nil? hwnd)
      # normalize-hwnd-and-uia-element failed
      nil

      (nil? exe-path)
      # get-hwnd-path failed
      nil

      (nil? desktop-info)
      # get-hwnd-virtual-desktop failed
      nil

      true
      {:hwnd hwnd
       :uia-element uia-win
       :exe-path exe-path
       :virtual-desktop desktop-info}))

  (when (and (nil? ret)
             (not (nil? uia-win))
             (not= uia-win uia-win?))
    (:Release uia-win))

  ret)


######### Generic tree node #########

(defn tree-node-activate [self]
  (var child self)
  (var parent (in self :parent))
  (while parent
    (put parent :current-child child)
    (set child parent)
    (set parent (in parent :parent))))


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


(defn tree-node-get-top-window [self]
  (def win-set @{})
  (each w (:get-all-windows self)
    (put win-set (in w :hwnd) w))

  (var top-win nil)
  # XXX: EnumChildWindows() visits windows according to z-order,
  # but it seems this behavior is not documented.
  (EnumChildWindows nil
                    (fn [hwnd]
                      (if-let [w (in win-set hwnd)]
                        (do
                          (set top-win w)
                          # Stop enumeration
                          0)
                        # Carry on
                        1)))
  top-win)


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


(defn tree-node-enumerate-node [self node dir &opt wrap-around node-type]
  (default wrap-around true)
  (default node-type (in node :type))

  (when (= :window (in self :type))
    (error "invalid operation"))

  (let [parent (in node :parent)
        all-siblings (in parent :children)
        sibling-count (length all-siblings)
        fr-idx (if-let [idx (find-index |(= $ node) all-siblings)]
                 idx
                 (error "inconsistent states for frame tree"))
        idx-to-check (case dir
                       :next
                       (if (and wrap-around (= self parent)) # Toplevel frames wrap around
                         (% (+ fr-idx 1) sibling-count)
                         (+ fr-idx 1))
                       :prev
                       (if (and wrap-around (= self parent))
                         (% (+ fr-idx sibling-count -1) sibling-count)
                         (- fr-idx 1)))]
    (cond
      (or (< idx-to-check 0)
          (>= idx-to-check sibling-count))
      # We reached the end of the sub-frame list
      (if (= self parent)
        nil # Stop at the top node
        (:enumerate-node self parent dir wrap-around node-type))

      (= dir :next)
      (let [node-to-check (in all-siblings idx-to-check)]
        (cond
          (not= :window node-type)
          (:get-first-frame node-to-check)

          (= :window (in node-to-check :type))
          node-to-check

          true
          (first (:get-all-windows node-to-check))))

      (= dir :prev)
      (let [node-to-check (in all-siblings idx-to-check)]
        (cond
          (not= :window node-type)
          (:get-last-frame node-to-check)

          (= :window (in node-to-check :type))
          node-to-check

          true
          (last (:get-all-windows node-to-check)))))))


(defn- find-closest-child [children reference rect-key]
  (var found (first children))
  (var min-dist (math/abs (- (get-in found [:rect rect-key]) reference)))
  (each child (slice children 1)
    (def dist (math/abs (- (get-in child [:rect rect-key]) reference)))
    (when (< dist min-dist)
      (set min-dist dist)
      (set found child)))
  found)


(defn- get-adjacent-frame-impl-descent [orig-node node dir]
  (def children (in node :children))
  (def {:top orig-top :left orig-left} (in orig-node :rect))

  (cond
    (empty? children)
    node

    (= :window (in (first children) :type))
    node

    (= :frame (in (first children) :type))
    (get-adjacent-frame-impl-descent orig-node
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


(defn- get-adjacent-frame-impl-ascent [orig-node node dir]
  (let [parent (in node :parent)
        all-siblings (in parent :children)
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
      (get-adjacent-frame-impl-descent orig-node adj-fr dir)
      (if (= :layout (in parent :type))
        nil # We reached the top level
        (get-adjacent-frame-impl-ascent orig-node parent dir)))))


(defn tree-node-get-adjacent-frame [self dir]
  (if (= :window (in self :type))
    (let [parent (in self :parent)]
      (get-adjacent-frame-impl-ascent parent parent dir))
    (get-adjacent-frame-impl-ascent self self dir)))


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


(defn tree-node-get-root [self]
  (if-let [layout (:get-layout self)]
    (in layout :parent)
    nil))


(defn tree-node-get-window-manager [self]
  (if-let [root (:get-root self)]
    (in root :window-manager)
    nil))


(defn tree-node-get-top-frame [self]
  (def parent (in self :parent))
  (cond
    (nil? parent)
    (if (= :frame (in self :type))
      self
      nil)

    (= :layout (in parent :type))
    self

    true
    (:get-top-frame parent)))


(def- tree-node-proto
  @{:activate tree-node-activate
    :get-next-child tree-node-get-next-child
    :get-prev-child tree-node-get-prev-child
    :get-next-sibling tree-node-get-next-sibling
    :get-prev-sibling tree-node-get-prev-sibling
    :add-child tree-node-add-child
    :remove-child tree-node-remove-child
    :get-all-windows tree-node-get-all-windows
    :get-top-window tree-node-get-top-window
    :get-current-window tree-node-get-current-window
    :get-current-frame tree-node-get-current-frame
    :get-first-frame tree-node-get-first-frame
    :get-last-frame tree-node-get-last-frame
    :enumerate-node tree-node-enumerate-node
    :get-adjacent-frame tree-node-get-adjacent-frame
    :find-hwnd tree-node-find-hwnd
    :purge-windows tree-node-purge-windows
    :get-layout tree-node-get-layout
    :get-root tree-node-get-root
    :get-window-manager tree-node-get-window-manager
    :get-top-frame tree-node-get-top-frame})


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

(defn window-close [self]
  (PostMessage (in self :hwnd) WM_CLOSE 0 0))


(defn window-transform [self rect]
  (def wm (:get-window-manager self))
  (:transform-hwnd wm (in self :hwnd) rect (in self :tags)))


(defn window-set-alpha [self alpha]
  (set-hwnd-alpha (in self :hwnd) alpha))


(defn window-get-alpha [self]
  (get-hwnd-alpha (in self :hwnd)))


(defn window-elevated? [self]
  (hwnd-process-elevated? (in self :hwnd)))


(defn window-get-exe-path [self]
  (get-hwnd-path (in self :hwnd)))


(defn window-get-uia-element [self &opt cr]
  (def wm (:get-window-manager self))
  (:get-hwnd-uia-element wm (in self :hwnd) cr))


(defn window-get-virtual-desktop [self &opt uia-win]
  (def wm (:get-window-manager self))
  (:get-hwnd-virtual-desktop wm (in self :hwnd) uia-win))


(defn window-get-info [self]
  (def wm (:get-window-manager self))
  (:get-hwnd-info wm (in self :hwnd)))


(def- window-proto
  (table/setproto
   @{:close window-close
     :transform window-transform
     :get-alpha window-get-alpha
     :set-alpha window-set-alpha
     :elevated? window-elevated?
     :get-exe-path window-get-exe-path
     :get-uia-element window-get-uia-element
     :get-virtual-desktop window-get-virtual-desktop
     :get-info window-get-info}
   tree-node-proto))


(defn window [hwnd &opt parent]
  (let [node (tree-node :window parent nil
                        :hwnd hwnd
                        :tags @{})]
    (table/setproto node window-proto)))


######### Frame object #########

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
    (put first-sub-frame :current-child old-active-child)))


(defn frame-balance [self &opt recursive]
  (default recursive false)

  (def all-children (in self :children))

  (cond
    (empty? all-children)
    nil

    (not= :frame (get-in self [:children 0 :type]))
    nil

    true
    (let [child-count (length all-children)
          [width height] (rect-size (in self :rect))
          balanced-len (math/floor (/ (cond
                                        (= vertical-frame-proto (table/getproto self)) height
                                        (= horizontal-frame-proto (table/getproto self)) width)
                                      child-count))]
      (def new-rects (:calculate-sub-rects self (fn [_sub-fr _i] balanced-len)))
      (if recursive
        (map (fn [sub-fr rect]
               (put sub-fr :rect rect)
               (:balance sub-fr recursive))
             all-children
             new-rects)
        (map (fn [sub-fr rect]
               (:transform sub-fr rect))
             all-children
             new-rects)))))


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
       (first all-windows)))
  # Clear vertical/horizontal settings
  (table/setproto self frame-proto))


(defn frame-transform [self new-rect]
  (def old-rect (in self :rect))
  (put self :rect new-rect)

  (def all-children (in self :children))
  (cond
    (empty? all-children)
    (break)

    (= :window (get-in all-children [0 :type]))
    # Do not actually resize the windows until next retile
    (break)

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
             (:transform sub-fr rect))
           all-children
           new-rects))))


(defn frame-resize [self new-rect]
  (def parent (in self :parent))
  (when (= :layout (in parent :type))
    # This is a toplevel frame, which tracks the monitor
    # geometries and cannot be resized
    (break))

  (let [all-siblings (in parent :children)
        parent-rect (in parent :rect)
        [old-width old-height] (rect-size (in self :rect))
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
                             (if (= sib-fr self)
                               dh
                               (math/floor (* (- dh) (/ sib-height avail-h)))))
                           (+ sib-height sib-dh)))]
        (map (fn [sib-fr rect]
               (:transform sib-fr rect))
             all-siblings
             new-rects)
        (when (not= dw 0)
          (:resize parent
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
                             (if (= sib-fr self)
                               dw
                               (math/floor (* (- dw) (/ sib-width avail-w)))))
                           (+ sib-width sib-dw)))]
        (map (fn [sib-fr rect]
               (:transform sib-fr rect))
             all-siblings
             new-rects)
        (when (not= dh 0)
          (:resize parent
                   {:left (in parent-rect :left)
                    :top (in parent-rect :top)
                    :right (in parent-rect :right)
                    :bottom (+ dh (in parent-rect :bottom))}))))))


(defn frame-close [self]
  (def parent (in self :parent))
  (def children (in self :children))

  (cond
    (= :layout (in parent :type))
    # This is a toplevel frame, which tracks the monitor
    # geometries and cannot be closed
    nil

    (or (empty? children)
        (= :window (in (first children) :type)))
    (let [all-siblings (in parent :children)
          [width height] (rect-size (in self :rect))
          [parent-width parent-height] (rect-size (in parent :rect))]
      (if (> (length all-siblings) 2)
        (do
          (:remove-child parent self)

          (def calc-fn
            (cond
              (= horizontal-frame-proto (table/getproto parent))
              (let [rest-width (- parent-width width)]
                (fn [sib-fr _]
                  (let [sib-width (rect-width (in sib-fr :rect))
                        ratio (/ sib-width rest-width)]
                    (math/floor (* ratio parent-width)))))

              (= vertical-frame-proto (table/getproto parent))
              (let [rest-height (- parent-height height)]
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
          (each child children
            (put child :parent nil)
            (:add-child cur-frame child)))

        (do
          # When the frame is closed, the parent will only have a single child.
          # Remove that child too, and move all children to the parent,
          # so that the tree stays consistent.
          (def sibling (:get-next-sibling self))
          (def sibling-rect (in sibling :rect))
          (def [sibling-width sibling-height] (rect-size sibling-rect))

          (def to-frame (:get-current-frame sibling))
          (each child children
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

    (= :frame (in (first children) :type))
    (error "cannot close frames containing sub-frames")))


(set frame-proto
     (table/setproto
      @{:split frame-split
        :balance frame-balance
        :flatten frame-flatten
        :transform frame-transform
        :resize frame-resize
        :close frame-close}
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


(def- layout-proto
  (table/setproto
   @{}
   tree-node-proto))


(defn layout [id &opt name parent children]
  (default children @[])
  (def layout-obj (tree-node :layout parent children
                             :id id
                             :name name))
  (table/setproto layout-obj layout-proto))


######### Virtual desktop container object #########

(defn vdc-get-current-frame-on-desktop [self desktop-info]
  (def {:id desktop-id
        :name desktop-name}
    desktop-info)

  (var layout-found nil)
  (each lo (in self :children)
    (when (= (in lo :id) desktop-id)
      (set layout-found lo)
      (break)))
  (if layout-found
    (:get-current-frame layout-found)
    (let [new-layout (:new-layout self desktop-info)]
      (:add-child self new-layout)
      (:get-current-frame new-layout))))


(defn vdc-new-layout [self desktop-info]
  (def wm (in self :window-manager))
  (def [work-areas main-idx] (:enumerate-monitors wm))
  (def {:id id :name name} desktop-info)
  (def new-layout (layout id name nil (map |(frame $) work-areas)))
  (def to-activate (or main-idx 0))
  (:activate (get-in new-layout [:children to-activate]))
  new-layout)


(def- virtual-desktop-container-proto
  (table/setproto
   @{:new-layout vdc-new-layout
     :get-current-frame-on-desktop vdc-get-current-frame-on-desktop}
   tree-node-proto))


(defn virtual-desktop-container [wm &opt children]
  (default children @[])
  (def vdc-obj (tree-node :virtual-desktop-container nil children
                          :window-manager wm))
  (table/setproto vdc-obj virtual-desktop-container-proto))


######### Window manager object #########

(defn wm-transform-hwnd [self hwnd rect &opt tags]
  (transform-hwnd hwnd rect (in self :uia-manager) tags))


(defn wm-hwnd-process-elevated? [self hwnd]
  (hwnd-process-elevated? hwnd))


(defn wm-jwno-process-elevated? [self]
  (with [[ret token]
         (OpenProcessToken (GetCurrentProcess) TOKEN_QUERY)
         (fn [[_ token]] (CloseHandle token))]
    (when (= 0 ret)
      (break false))
    (def [_gti-ret elevated] (GetTokenInformation token TokenElevation))
    elevated))


(defn wm-get-pid-path [self pid]
  (get-pid-path pid))


(defn wm-get-hwnd-path [self hwnd]
  (get-hwnd-path hwnd))


(defn wm-get-hwnd-uia-element [self hwnd &opt cr]
  (default cr (get-in self [:uia-manager :focus-cr]))
  (def uia-man (in self :uia-manager))
  (get-hwnd-uia-element hwnd (in uia-man :com) cr))


(defn wm-get-hwnd-virtual-desktop [self hwnd? &opt uia-win?]
  (get-hwnd-virtual-desktop hwnd?
                            (in self :uia-manager)
                            (in self :vdm-com)
                            uia-win?))


(defn wm-get-hwnd-info [self hwnd? &opt uia-win?]
  (get-hwnd-info hwnd? (in self :uia-manager) (in self :vdm-com) uia-win?))


(defn wm-should-manage-hwnd? [self hwnd-info]
  (def {:hwnd hwnd
        :uia-element uia-win
        :exe-path exe-path
        :virtual-desktop desktop-info}
    hwnd-info)
  (:call-filter-hook (in self :hook-manager) :filter-window
     hwnd uia-win exe-path desktop-info))


(defn wm-add-hwnd [self hwnd-info]
  (def {:hwnd hwnd
        :uia-element uia-win
        :exe-path exe-path
        :virtual-desktop desktop-info}
    hwnd-info)

  (log/debug "new window: %n" hwnd)

  (def new-win (window hwnd))
  (:call-hook (in self :hook-manager) :window-created
     new-win uia-win exe-path desktop-info)

  (def tags (in new-win :tags))

  # TODO: floating windows?

  (def frame-found
    (if-let [override-frame (in tags :frame)]
      (do 
        (put tags :frame nil) # Clear the tag, in case the frame got invalidated later
        override-frame)
      (:get-current-frame-on-desktop (in self :root) desktop-info)))

  (:add-child frame-found new-win)
  (:transform-hwnd self
                   (in new-win :hwnd)
                   (in frame-found :rect)
                   (in new-win :tags))
  new-win)


(defn- activate-and-call-hooks [node wm]
  (def old-frame (:get-current-frame (in wm :root)))
  (def old-win (:get-current-window old-frame))
  (:activate node)
  
  (def [fr-to-signal win-to-signal]
    (if (= :window (in node :type))
      [(in node :parent) node]
      (let [fr (:get-current-frame node)
            win (:get-current-window fr)]
        [fr win])))

  (unless (= fr-to-signal old-frame)
    (when-let [top-win (:get-top-window fr-to-signal)]
      # Explicitly make the top window our current window, to
      # prevent lower windows popping up when switching to a frame
      (put fr-to-signal :current-child top-win))
    (:call-hook (in wm :hook-manager) :frame-activated
       fr-to-signal))

  (unless (or (nil? win-to-signal)
              (= win-to-signal old-win))
    (:call-hook (in wm :hook-manager) :window-activated
       win-to-signal)))


(defn window-purge-pred [win wm layout]
  (def hwnd (in win :hwnd))
  (def layout-vd-id (in layout :id))
  (def win-vd-id
    (try
      (:GetWindowDesktopId (in wm :vdm-com) hwnd)
      ((err fib)
       (log/debug "GetWindowDesktopId failed: %n\n%s"
                  err
                  (get-stack-trace fib))
       nil)))
  (or (not= win-vd-id layout-vd-id)
      (not (:hwnd-alive? wm hwnd))))


(defn wm-focus-changed [self]
  (:call-hook (in self :hook-manager) :focus-changed)

  (def uia-man (in self :uia-manager))
  (with-uia [uia-win (:get-focused-window uia-man)]
    (when (nil? uia-win)
      (log/debug "No focused window")
      (break))
    
    (def hwnd (:get_CachedNativeWindowHandle uia-win))

    (when-let [win (:find-hwnd (in self :root) hwnd)]
      #Already managed
      (activate-and-call-hooks win self)
      (break))

    (def hwnd-info
      (get-hwnd-info hwnd
                     (in self :uia-manager)
                     (in self :vdm-com)
                     uia-win))
    (when (nil? hwnd-info)
      (log/debug "Window %n vanished?" hwnd)
      (break))

    (when (not (:should-manage-hwnd? self hwnd-info))
      (log/debug "Ignoring window: %n" hwnd)
      (break))

    (if-let [new-win (:add-hwnd self hwnd-info)]
      (activate-and-call-hooks new-win self))))


(defn wm-window-opened [self hwnd]
  (when-let [win (:find-hwnd (in self :root) hwnd)]
    (log/debug "window-opened event for managed window: %n" hwnd)
    (break))

  (def hwnd-info
    (get-hwnd-info hwnd
                   (in self :uia-manager)
                   (in self :vdm-com)))
  (when (nil? hwnd-info)
    (log/debug "Window %n vanished?" hwnd)
    (break))

  (with-uia [_uia-win (in hwnd-info :uia-element)]
    (unless (:should-manage-hwnd? self hwnd-info)
      (log/debug "Ignoring window: %n" hwnd)
      (break))

    (:add-hwnd self hwnd-info)))


(defn wm-activate [self node]
  (when node
    (activate-and-call-hooks node self))

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
    (:set-focus-to-window uia-man hwnd)))


(defn wm-retile [self &opt fr]
  (cond
    (nil? fr)
    # Retile the whole tree
    (:retile self (in self :root))

    true
    (cond
      (empty? (in fr :children))
      nil

      (= :window (get-in fr [:children 0 :type]))
      (each w (in fr :children)
        (:transform-hwnd self
                         (in w :hwnd)
                         (in fr :rect)
                         (in w :tags)))

      (or (= :frame (get-in fr [:children 0 :type]))
          (= :layout (get-in fr [:children 0 :type])))
      (each f (in fr :children)
        (:retile self f)))))


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

    :transform-hwnd wm-transform-hwnd
    :retile wm-retile
    :activate wm-activate

    :get-hwnd-path wm-get-hwnd-path
    :get-hwnd-virtual-desktop wm-get-hwnd-virtual-desktop
    :get-hwnd-uia-element wm-get-hwnd-uia-element
    :get-hwnd-info wm-get-hwnd-info
    :hwnd-alive? wm-hwnd-alive?
    :hwnd-process-elevated? wm-hwnd-process-elevated?

    :should-manage-hwnd? wm-should-manage-hwnd?
    :add-hwnd wm-add-hwnd

    :close-hwnd wm-close-hwnd

    :get-pid-path wm-get-pid-path
    :enumerate-monitors wm-enumerate-monitors
    :jwno-process-elevated? wm-jwno-process-elevated?

    :destroy wm-destroy})


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
         (hwnd-process-elevated? hwnd))
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
     (fn [hwnd uia-win exe-path _desktop-info]
       (default-window-filter hwnd uia-win exe-path wm-obj)))
  (:add-hook hook-man :focus-changed
     (fn []
       # XXX: If the focus change is caused by a closing window, that
       # window may still be alive, so it won't be purged immediately.
       # Maybe I shoud check the hwnds everytime a window is manipulated?
       (each layout (get-in wm-obj [:root :children])
         (def dead
           (:purge-windows layout |(window-purge-pred $ wm-obj layout)))
         (each dw dead
           (:call-hook hook-man :window-removed dw))
         (log/debug "purged %n dead windows from desktop %n"
                    (length dead)
                    (in layout :id)))))

  wm-obj)
