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

(import ./const)
(import ./log)


# Forward declarations
(var frame nil)
(var frame-proto nil)
(var vertical-frame-proto nil)
(var horizontal-frame-proto nil)


######### Helpers #########

(defn- calc-win-coords-in-frame [win-rect fr-rect fit anchor]
  (def [fr-width fr-height] (rect-size fr-rect))
  (def [win-width win-height] (rect-size win-rect))

  (def [x y]
    (cond
      (= :center anchor)
      [(math/floor
        (+ (in fr-rect :left)
           (/ fr-width 2)
           (/ win-width -2)))
       (math/floor
        (+ (in fr-rect :top)
           (/ fr-height 2)
           (/ win-height -2)))]

      (= :left anchor)
      [(in fr-rect :left)
       (math/floor
        (+ (in fr-rect :top)
           (/ fr-height 2)
           (/ win-height -2)))]

      (= :right anchor)
      [(- (in fr-rect :right) win-width)
       (math/floor
        (+ (in fr-rect :top)
           (/ fr-height 2)
           (/ win-height -2)))]

      (= :top anchor)
      [(math/floor
        (+ (in fr-rect :left)
           (/ fr-width 2)
           (/ win-width -2)))
       (in fr-rect :top)]

      (= :bottom anchor)
      [(math/floor
        (+ (in fr-rect :left)
           (/ fr-width 2)
           (/ win-width -2)))
       (- (in fr-rect :bottom) win-height)]

      (or (= :top-left anchor)
          (= :left-top anchor))
      [(in fr-rect :left)
       (in fr-rect :top)]

      (or (= :top-right anchor)
          (= :right-top anchor))
      [(- (in fr-rect :right) win-width)
       (in fr-rect :top)]

      (or (= :bottom-left anchor)
          (= :left-bottom anchor))
      [(in fr-rect :left)
       (- (in fr-rect :bottom) win-height)]

      (or (= :bottom-right anchor)
          (= :right-bottom anchor))
      [(- (in fr-rect :right) win-width)
       (- (in fr-rect :bottom) win-height)]

      (errorf "unknown anchor: %n" anchor)))

  (if fit
    (let [fitted-x (max x (in fr-rect :left))
          fitted-width (min win-width fr-width)
          fitted-y (max y (in fr-rect :top))
          fitted-height (min win-height fr-height)]
      [fitted-x fitted-y fitted-width fitted-height])
    [x y win-width win-height]))


(defn- reset-hwnd-visual-state [hwnd uia-com restore-minimized restore-maximized]
  (with-uia [cr (:CreateCacheRequest uia-com)]
    (:AddPattern cr UIA_WindowPatternId)
    (:AddProperty cr UIA_WindowWindowVisualStatePropertyId)

    (with-uia [uia-win (:ElementFromHandleBuildCache uia-com hwnd cr)]
      (with-uia [win-pat
                 (try
                   (:GetCachedPatternAs uia-win UIA_WindowPatternId IUIAutomationWindowPattern)
                   ((err fib)
                    (log/warning "failed to get window pattern for %n: %n%s"
                                 hwnd
                                 err
                                 (get-stack-trace fib))
                    nil))]
        (if win-pat
          (let [old-state (:get_CachedWindowVisualState win-pat)]
            (when (and restore-minimized
                       (= WindowVisualState_Minimized old-state))
              (:SetWindowVisualState win-pat WindowVisualState_Normal))
            (when (and restore-maximized
                       (= WindowVisualState_Maximized old-state))
              (:SetWindowVisualState win-pat WindowVisualState_Normal))
            old-state)
          # XXX: Default to Normal state when failed to get the window pattern
          WindowVisualState_Normal)))))


(defn get-hwnd-dwm-border-margins [hwnd &opt outer-rect]
  (default outer-rect
    (let [[gwr-ret rect] (GetWindowRect hwnd)]
      (when (= FALSE gwr-ret)
        (errorf "failed to get bounding rectangle for window %n" hwnd))
      rect))

  (def inner-rect (DwmGetWindowAttribute hwnd DWMWA_EXTENDED_FRAME_BOUNDS))

  (log/debug "outer-rect = %n" outer-rect)
  (log/debug "inner-rect = %n" inner-rect)

  {:top (- (in outer-rect :top) (in inner-rect :top))
   :left (- (in outer-rect :left) (in inner-rect :left))
   :bottom (- (in inner-rect :bottom) (in outer-rect :bottom))
   :right (- (in inner-rect :right) (in outer-rect :right))})


(defn- get-margins-or-paddings-from-tags [tags key dkey]
  (if-let [v (in tags dkey)]
    v
    (let [v (in tags key 0)]
      {:top v
       :left v
       :bottom v
       :right v})))


(defn- set-window-pos [hwnd x y w h &opt scaled]
  (default scaled false)

  (def flags
    (if (or (<= w 0) (<= h 0))
      (bor SWP_NOZORDER SWP_NOACTIVATE SWP_NOSIZE)
      (bor SWP_NOZORDER SWP_NOACTIVATE)))
  (when (= FALSE (SetWindowPos hwnd nil x y w h flags))
    (errorf "SetWindowPos failed for window %n: %n" hwnd (GetLastError)))
  # XXX: I couldn't work out why SetWindowPos sometimes wouldn't respect
  # the x, y, w and h values when moving windows between monitors with
  # different DPIs (besides the DPI-unaware window's case described in
  # transform-hwnd), but calling it again SEEMED to fix it.
  (when scaled
    (when (= FALSE (SetWindowPos hwnd nil x y w h flags))
      (errorf "SetWindowPos failed for window %n: %n" hwnd (GetLastError)))))


(defn calc-window-rect-with-margins [win-rect orig-rect cur-scale scale dwm-margins margins]
  (log/debug "win-rect = %n" win-rect)
  (log/debug "orig-rect = %n" orig-rect)
  (log/debug "cur-scale = %n" cur-scale)
  (log/debug "scale = %n" scale)
  (log/debug "dwm-margins = %n" dwm-margins)
  (log/debug "margins = %n" margins)

  (def [cur-scale-x cur-scale-y] cur-scale)
  (def [scale-x scale-y] scale)

  # These DWM margin values are in "physical pixels." They're already scaled
  # according to the current monitor's DPI, but we need to re-scale them in
  # case the window is moving to another monitor with different DPI.
  (def scaled-dwm-margins
    (if (and (= scale-x cur-scale-x) (= scale-y cur-scale-y))
      dwm-margins
      {:top (* scale-y (/ (in dwm-margins :top) cur-scale-y))
       :left (* scale-x (/ (in dwm-margins :left) cur-scale-x))
       :bottom (* scale-y (/ (in dwm-margins :bottom) cur-scale-y))
       :right (* scale-x (/ (in dwm-margins :right) cur-scale-x))}))
  (log/debug "scaled-dwm-margins = %n" scaled-dwm-margins)

  (def scaled-margins
    (if (and (= 1 scale-x) (= 1 scale-y))
      margins
      {:top (* scale-y (in margins :top))
       :left (* scale-x (in margins :left))
       :bottom (* scale-y (in margins :bottom))
       :right (* scale-x (in margins :right))}))
  (log/debug "scaled-margins = %n" scaled-margins)

  (def combined-margins
    (combine-rect-border-space scaled-dwm-margins
                               scaled-margins))
  (log/debug "combined-margins = %n" combined-margins)

  (def rect (shrink-rect orig-rect combined-margins))
  (log/debug "final rect = %n" rect)

  rect)


(defn- transform-hwnd [hwnd orig-rect uia-man &opt tags]
  (default tags @{})

  (log/debug "transforming window: %n" hwnd)

  # Code below will restore maximized windows, but ignore minimized windows
  # before transforming. This feels more natural in practice, since minimized
  # windows are more like "hidden", but maximized windows are still visible.
  (try
    (let [{:com uia-com :transform-cr cr} uia-man
          old-v-state (reset-hwnd-visual-state hwnd uia-com false true)]
      (unless (= WindowVisualState_Minimized old-v-state)
        (with-uia [uia-win (:ElementFromHandleBuildCache uia-com hwnd cr)]
          (with-uia [tran-pat (:GetCachedPatternAs uia-win
                                                   UIA_TransformPatternId
                                                   IUIAutomationTransformPattern)]
            (def no-resize (in tags :no-resize))
            (def no-expand (in tags :no-expand))
            (def anchor (in tags :anchor :top-left))
            (def forced (in tags :forced))

            # XXX: When dealing with QT windows, the bounding rectangle returned by
            # uia-win will be incorrect, and I have absolutely no idea why. GetWindowRect
            # returns the right values though, so that's what we use here.
            (def [gwr-ret win-rect] (GetWindowRect hwnd))
            (when (= FALSE gwr-ret)
              (errorf "failed to get bounding rectangle for window %n" hwnd))

            (def scale (calc-pixel-scale orig-rect))
            (def cur-scale (calc-pixel-scale win-rect))
            (def scaled (not= scale cur-scale))

            (def margins (get-margins-or-paddings-from-tags tags :margin :margins))
            (def dwm-margins (get-hwnd-dwm-border-margins hwnd win-rect))

            (def rect
              (calc-window-rect-with-margins win-rect
                                             orig-rect
                                             cur-scale
                                             scale
                                             dwm-margins
                                             margins))

            (def can-move
              (or forced
                  (and tran-pat
                       (not= 0 (:get_CachedCanMove tran-pat)))))
            (def can-resize
              (or forced
                  (and tran-pat
                       (not= 0 (:get_CachedCanResize tran-pat)))))

            (when can-move
              (cond
                (or (not can-resize)
                    no-resize)
                (let [[x y _w _h] (calc-win-coords-in-frame win-rect rect false anchor)]
                  (set-window-pos hwnd x y 0 0 scaled))

                no-expand
                (let [[x y w h] (calc-win-coords-in-frame win-rect rect true anchor)]
                  (set-window-pos hwnd x y w h scaled))

                true
                (let [x (in rect :left)
                      y (in rect :top)
                      [w h] (rect-size rect)]
                  # This was achieved by separate calls to :Move and :Resize methods
                  # from the tran-pat object. But, when moving windows between monitors
                  # with different DPIs, the first :Move call may generate a WM_DPICHANGED
                  # message for the target window, then the following :Resize call will
                  # scale w and h incorrectly. It seems SetWindowPos doesn't have that
                  # problem because it moves and resizes the target window at the same time.
                  #
                  # There's another issue though. (Tested on Windows 10 build 19045.4651)
                  # DPI-unaware (and system DPI-aware?) windows would NOT respect the geometries
                  # passed to SetWindowPos, if any part of the target rect (i.e. the transformed
                  # window) would appear on another monitor with different DPI. For example,
                  # there are two monitors, Monitor A is of 96 DPI, and monitor B 144. If
                  # SetWindowPos would transform a DPI-unaware window such that the window will
                  # mainly be displayed on monitor A, but has its right-most column of pixels
                  # (the pixels from the invisible border added by the shell) shown on
                  # monitor B, it will re-calculate (?) its own geometries and use those.
                  # To remedy this issue, margins or paddings should be added around DPI-unaware
                  # windows, to keep their invisible borders clear from the edges of monitors.
                  (set-window-pos hwnd x y w h scaled))))))))

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
    (do
      (:AddRef uia-win?)
      [(try
         (:get_CachedNativeWindowHandle uia-win?)
         ((err fib)
          (log/debug "get_CachedNativeWindowHandle failed: %n\n%s"
                     err
                     (get-stack-trace fib))
          nil))
       uia-win?])

    true
    (do
      (:AddRef uia-win?)
      [hwnd? uia-win?])))


(defn- try-to-get-window-desktop-id [vdm-com hwnd]
  (try
    (:GetWindowDesktopId vdm-com hwnd)
    ((err fib)
     (log/debug "GetWindowDesktopId failed for %n: %n\n%s"
                hwnd
                err
                (get-stack-trace fib))
     nil)))


(defn- get-hwnd-virtual-desktop-id [hwnd vdm-com]
  # Only top-level windows can be managed by virtual desktops
  (var cur-hwnd (GetAncestor hwnd GA_ROOT))
  (var desktop-id? (try-to-get-window-desktop-id vdm-com cur-hwnd))

  (while (or # GetWindowDesktopId failed
             (nil? desktop-id?)
             # Window not managed by virtual desktops
             (= desktop-id? "{00000000-0000-0000-0000-000000000000}"))
    # Try the owner instead
    (def owner (GetWindow cur-hwnd GW_OWNER))
    (when (null? owner)
      (break))

    (set cur-hwnd owner)
    (set desktop-id? (try-to-get-window-desktop-id vdm-com cur-hwnd)))

  desktop-id?)


(defn- get-current-virtual-desktop-name [uia-win uia-man]
  # XXX: The returned root element will always have the name of
  # the current virtual desktop, but this is not documented at
  # all.
  (with-uia [root-elem (:get-root uia-man uia-win)]
    (if root-elem
      (:get_CachedName root-elem)
      nil)))


(defn- get-hwnd-virtual-desktop [hwnd? uia-man vdm-com &opt uia-win?]
  (def [hwnd uia-win]
    (normalize-hwnd-and-uia-element hwnd?
                                    uia-win?
                                    (in uia-man :com)
                                    (in uia-man :focus-cr)))

  (with-uia [_uia-win uia-win]
    (cond
      (nil? uia-win)
      nil

      (nil? hwnd)
      nil

      true
      (let [desktop-id (get-hwnd-virtual-desktop-id hwnd vdm-com)
            # XXX: The name of the HWND's virtual desktop can only be
            # retrieved when that virtual desktop is active. Find a way
            # around this?
            desktop-name (if (= FALSE (:IsWindowOnCurrentVirtualDesktop vdm-com hwnd))
                           nil
                           (get-current-virtual-desktop-name uia-win uia-man))]
        (if (and (nil? desktop-id)
                 (nil? desktop-name))
          nil
          {:id desktop-id :name desktop-name})))))


(defn- get-hwnd-info [hwnd? uia-man vdm-com &opt uia-win?]
  (def [hwnd uia-win]
    (normalize-hwnd-and-uia-element hwnd?
                                    uia-win?
                                    (in uia-man :com)
                                    (in uia-man :focus-cr)))

  (with-uia [_uia-win uia-win]
    (def exe-path
      (unless (nil? hwnd)
        (get-hwnd-path hwnd)))
    (def desktop-info
      (unless (nil? hwnd)
        (get-hwnd-virtual-desktop hwnd uia-man vdm-com uia-win)))

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
      (do
        # Always :AddRef when returning a UIA element. Its the
        # callers' responsibility to :Release all the UIA elements
        # they get.
        (:AddRef uia-win)
        {:hwnd hwnd
         :uia-element uia-win
         :exe-path exe-path
         :virtual-desktop desktop-info}))))


(defn- window-purge-pred [win wm layout]
  (def hwnd (in win :hwnd))
  (or (not (:alive? win))
      (not= (in layout :id)
            (get-hwnd-virtual-desktop-id hwnd (in wm :vdm-com)))))


######### Generic tree node #########

(defn tree-node-activate [self]
  (var child self)
  (var parent (in self :parent))
  (while parent
    (put parent :current-child child)
    (set child parent)
    (set parent (in parent :parent))))


(defn tree-node-attached? [self]
  (def parent (in self :parent))

  (cond
    (= :virtual-desktop-container (in self :type))
    # The root is always attached
    true

    (nil? parent)
    false

    (not (:has-child? parent self))
    false

    (not (:attached? parent))
    false

    true))


(defn tree-node-has-child? [self child]
  (def children (in self :children))
  (var ret false)
  (each c children
    (when (= c child)
      (set ret true)
      (break)))
  ret)


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


(defn tree-node-get-all-windows [self &opt arr]
  (default arr @[])

  (def children (in self :children))
  (cond
    (or (nil? children) (empty? children))
    arr

    (= :window (in (first children) :type))
    (do
      (each w children
        (array/push arr w))
      arr)

    true # children are other container nodes
    (do
      (each c children
        (:get-all-windows c arr))
      arr)))


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


(defn tree-node-get-window-stack [self]
  (def win-set @{})
  (each w (:get-all-windows self)
    (put win-set (in w :hwnd) w))
  (def win-stack @[])
  (EnumChildWindows nil
                    (fn [hwnd]
                      (when-let [w (in win-set hwnd)]
                        (array/push win-stack w))
                      1 # !!! IMPORTANT
                      ))
  win-stack)


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
    (if (= :frame (in self :type))
      self
      # else
      nil)

    (= :window (in (first children) :type))
    self

    (not (nil? current-child))
    (:get-current-frame current-child)

    true
    # There are children, but no current-child
    (error "inconsistent states for frame tree")))


(defn tree-node-get-current-top-frame [self]
  (case (in self :type)
    :window
    (:get-top-frame self)

    :frame
    (:get-top-frame self)

    :layout
    (in self :current-child)

    :virtual-desktop-container
    (when-let [lo (in self :current-child)]
      (in lo :current-child))))


(defn tree-node-get-first-frame [self]
  (def children (in self :children))

  (cond
    (= :window (in self :type))
    (error "invalid operation")

    (empty? children)
    (if (= :frame (in self :type))
      self
      # else
      nil)

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
    (if (= :frame (in self :type))
      self
      # else
      nil)

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

    (if (not= :layout (in parent :type))
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

      # Children of layout objects are not sorted
      (do
        (def [node-dist-prop sib-dist-prop cmp-op]
          (case dir
            :left [:left :right >=]
            :right [:right :left <=]
            :up [:top :bottom >=]
            :down [:bottom :top <=]))
        (var min-dist math/int-max)
        (each sibling all-siblings
          (def sib-prop-val (get-in sibling [:rect sib-dist-prop]))
          (def node-prop-val (get-in node [:rect node-dist-prop]))
          (when (and (not= sibling node)
                     (cmp-op node-prop-val sib-prop-val ))
            (def cur-dist (math/abs (- node-prop-val sib-prop-val)))
            (when (< cur-dist min-dist)
              (set min-dist cur-dist)
              (set adj-fr sibling)
              (break))))))

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


(defn tree-node-purge-windows [self &opt pred?]
  (def pred
    (if pred?
      pred?
      # XXX: Does not work for the root node. The root node has its own
      # :purge-windows method
      (let [wm (:get-window-manager self)
            lo (:get-layout self)]
        |(window-purge-pred $ wm lo))))

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
    (:get-layout (in self :parent))))


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


(defn tree-node-get-depth [self &opt cur-depth]
  # Start from -2 by default so that top-level frames have a depth of 0.
  # Depths of layout and virtual-desktop-container objects are negative
  # by default.
  (default cur-depth -2)

  (def parent (in self :parent))
  (if (nil? parent)
    cur-depth
    (:get-depth parent (+ cur-depth 1))))


(defn- unwrap-rect [rect]
  [(in rect :left)
   (in rect :top)
   (in rect :right)
   (in rect :bottom)])


(defn tree-node-dump-subtree [self &opt level indent-width indent-char]
  (default level 0)
  (default indent-width const/DEFAULT-FRAME-TREE-DUMP-INDENT-WIDTH)
  (default indent-char const/DEFAULT-FRAME-TREE-DUMP-INDENT-CHAR)

  (def indent
    (buffer/new-filled (* level indent-width) indent-char))

  (case (in self :type)
    :virtual-desktop-container
    (printf "%sRoot Container" indent)

    :layout
    (printf "%sVirtual Desktop (name=%n, id=%n)"
            indent
            (in self :name)
            (in self :id))

    :frame
    (let [rect (in self :rect)]
      (if-let [mon (in self :monitor)]
        (printf "%sMonitor (primary=%n,dir=%s,work-area={l:%d,t:%d,r:%d,b:%d},dpi=%n,device=%n)"
                indent
                # primary
                (> (band (in mon :flags) MONITORINFOF_PRIMARY) (int/u64 0))
                # dir
                (if-let [dir (:get-direction self)]
                  (string dir)
                  "none")
                # work-area
                ;(unwrap-rect rect)
                # dpi
                (in mon :dpi)
                # device
                (in mon :device))
        (printf "%sFrame (dir=%s,rect={l:%d,t:%d,r:%d,b:%d})"
                indent
                # dir
                (if-let [dir (:get-direction self)]
                  (string dir)
                  "none")
                # rect
                ;(unwrap-rect rect))))

    :window
    (if-let [win-info (:get-info self)]
      (with-uia [_uia-win (in win-info :uia-element)]
        (def more-indent
          (string indent (buffer/new-filled indent-width indent-char)))
        (printf "%sWindow (hwnd=%n)"
                indent
                (in self :hwnd))
        (printf "%sName: %s"
                more-indent
                (:get_CachedName (in win-info :uia-element)))
        (printf "%sClass: %s"
                more-indent
                (:get_CachedClassName (in win-info :uia-element)))
        (printf "%sExe: %s"
                more-indent
                (in win-info :exe-path))
        (printf "%sRect: {l:%d,t:%d,r:%d,b:%d}"
                more-indent
                ;(unwrap-rect (:get_CachedBoundingRectangle (in win-info :uia-element))))
        (printf "%sExtended Frame Bounds: {l:%d,t:%d,r:%d,b:%d}"
                more-indent
                ;(unwrap-rect (DwmGetWindowAttribute (in self :hwnd) DWMWA_EXTENDED_FRAME_BOUNDS)))
        (printf "%sVirtual Desktop ID: %s"
                more-indent
                (get-in win-info [:virtual-desktop :id])))

      (printf "%sWindow (hwnd=%n,failed to get info)" indent))

    (printf "%sUnknown (%n)" indent self))

  (when-let [children (in self :children)]
    (each child children
      (:dump-subtree child (+ 1 level) indent-width indent-char))))


(def- tree-node-proto
  @{:activate tree-node-activate
    :attached? tree-node-attached?
    :has-child? tree-node-has-child?
    :get-next-child tree-node-get-next-child
    :get-prev-child tree-node-get-prev-child
    :get-next-sibling tree-node-get-next-sibling
    :get-prev-sibling tree-node-get-prev-sibling
    :add-child tree-node-add-child
    :remove-child tree-node-remove-child
    :get-all-windows tree-node-get-all-windows
    :get-top-window tree-node-get-top-window
    :get-window-stack tree-node-get-window-stack
    :get-current-window tree-node-get-current-window
    :get-current-frame tree-node-get-current-frame
    :get-current-top-frame tree-node-get-current-top-frame
    :get-first-frame tree-node-get-first-frame
    :get-last-frame tree-node-get-last-frame
    :enumerate-node tree-node-enumerate-node
    :get-adjacent-frame tree-node-get-adjacent-frame
    :find-hwnd tree-node-find-hwnd
    :purge-windows tree-node-purge-windows
    :get-layout tree-node-get-layout
    :get-root tree-node-get-root
    :get-window-manager tree-node-get-window-manager
    :get-top-frame tree-node-get-top-frame
    :get-depth tree-node-get-depth
    :dump-subtree tree-node-dump-subtree})


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


(defn window-alive? [self]
  (def hwnd (in self :hwnd))
  (and (not= FALSE (IsWindow hwnd))
       (not= FALSE (IsWindowVisible hwnd))))


(defn window-transform [self rect &opt tags]
  (default tags @{})
  (def wm (:get-window-manager self))
  (:transform-hwnd wm
                   (in self :hwnd)
                   rect
                   (merge (in self :tags) tags)))


(defn window-reset-visual-state [self &opt restore-minimized restore-maximized]
  (def wm (:get-window-manager self))
  (:reset-hwnd-visual-state wm
                            (in self :hwnd)
                            restore-minimized
                            restore-maximized))


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


(defn window-get-margins [self &opt scaled filter-fn]
  (default scaled true)
  (default filter-fn identity)

  (def margins
    (get-margins-or-paddings-from-tags (in self :tags) :margin :margins))
  (if scaled
    (let [hwnd (in self :hwnd)
          [ret win-rect] (GetWindowRect hwnd)
          [scale-x scale-y] (calc-pixel-scale win-rect)]
      (when (= ret FALSE)
        (errorf "failed to get bounding rectangle for window %n" hwnd))
      {:top (filter-fn (* scale-y (in margins :top)))
       :left (filter-fn (* scale-x (in margins :left)))
       :bottom (filter-fn (* scale-y (in margins :bottom)))
       :right (filter-fn (* scale-x (in margins :right)))})
    margins))


(defn window-get-dwm-border-margins [self &opt bounding-rect]
  (def hwnd (in self :hwnd))
  (get-hwnd-dwm-border-margins hwnd bounding-rect))


(def- window-proto
  (table/setproto
   @{:close window-close
     :alive? window-alive?
     :transform window-transform
     :reset-visual-state window-reset-visual-state
     :get-alpha window-get-alpha
     :set-alpha window-set-alpha
     :elevated? window-elevated?
     :get-exe-path window-get-exe-path
     :get-uia-element window-get-uia-element
     :get-virtual-desktop window-get-virtual-desktop
     :get-info window-get-info
     :get-margins window-get-margins
     :get-dwm-border-margins window-get-dwm-border-margins}
   tree-node-proto))


(defn window [hwnd &opt parent]
  (let [node (tree-node :window parent nil
                        :hwnd hwnd
                        :tags @{})]
    (table/setproto node window-proto)))


######### Frame object #########

(defn frame-split [self direction &opt n ratios]
  (default n 2)
  (default ratios (array/new-filled (- n 1) (/ 1 n)))

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

  (let [[width height] (rect-size (:get-padded-rect self))]
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


(defn frame-balance [self &opt recursive resized-frames]
  (default recursive false)

  (def all-children (in self :children))

  (cond
    (empty? all-children)
    :nop

    (not= :frame (get-in self [:children 0 :type]))
    :nop

    true
    (let [child-count (length all-children)
          [width height] (rect-size (:get-padded-rect self))
          balanced-len (math/floor (/ (cond
                                        (= vertical-frame-proto (table/getproto self)) height
                                        (= horizontal-frame-proto (table/getproto self)) width)
                                      child-count))]
      (def new-rects (:calculate-sub-rects self (fn [_sub-fr _i] balanced-len)))
      (if recursive
        (map (fn [sub-fr rect]
               (def old-rect (in sub-fr :rect))
               (put sub-fr :rect rect)
               # Only keep track of leaf frames that actually have their rects altered
               (unless (or (rect-same-size? rect old-rect)
                           (= :frame (get-in sub-fr [:children 0 :type])))
                 (array/push resized-frames sub-fr))
               (:balance sub-fr recursive resized-frames))
             all-children
             new-rects)
        (map (fn [sub-fr rect]
               (:transform sub-fr rect nil resized-frames))
             all-children
             new-rects))))
  resized-frames)


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


(defn frame-transform [self new-rect &opt to-dpi resized-frames]
  (def old-rect (in self :rect))
  (def old-padded-rect (:get-padded-rect self))

  (def new-padded-rect
    (if to-dpi
      (let [[new-dpi-x new-dpi-y] (if (number? to-dpi)
                                    [to-dpi to-dpi]
                                    to-dpi)
            new-scale-x (/ new-dpi-x const/USER-DEFAULT-SCREEN-DPI)
            new-scale-y (/ new-dpi-y const/USER-DEFAULT-SCREEN-DPI)
            virt-paddings (:get-paddings self false)
            new-paddings {:top (* new-scale-y (in virt-paddings :top))
                          :left (* new-scale-x (in virt-paddings :left))
                          :bottom (* new-scale-y (in virt-paddings :bottom))
                          :right (* new-scale-x (in virt-paddings :right))}]
        (shrink-rect new-rect new-paddings))
      (let [new-paddings (:get-paddings self)]
        (shrink-rect new-rect new-paddings))))

  (put self :rect new-rect)

  (def all-children (in self :children))
  (cond
    (empty? all-children)
    (when resized-frames
      (array/push resized-frames self))

    (= :window (get-in all-children [0 :type]))
    # Do not actually resize the windows until next retile
    (when resized-frames
      (array/push resized-frames self))

    (= :frame (get-in all-children [0 :type]))
    (let [dx (- (in new-padded-rect :left) (in old-padded-rect :left))
          dy (- (in new-padded-rect :top) (in old-padded-rect :top))
          dw (+ (- dx)
                (- (in new-padded-rect :right)
                   (in old-padded-rect :right)))
          dh (+ (- dy)
                (- (in new-padded-rect :bottom)
                   (in old-padded-rect :bottom)))
          [old-padded-width old-padded-height] (rect-size old-padded-rect)]
      (def calc-fn
        (cond
          (= horizontal-frame-proto (table/getproto self))
          (fn [sub-fr _i]
            (let [w (rect-width (in sub-fr :rect))
                  wr (/ w old-padded-width)
                  sub-dw (math/floor (* wr dw))]
              (+ w sub-dw)))

          (= vertical-frame-proto (table/getproto self))
          (fn [sub-fr _i]
            (let [h (rect-height (in sub-fr :rect))
                  hr (/ h old-padded-height)
                  sub-dh (math/floor (* hr dh))]
              (+ h sub-dh)))))
      (def new-rects (:calculate-sub-rects self calc-fn nil new-padded-rect))
      (map (fn [sub-fr rect]
             (if (rect-same-size? rect (in sub-fr :rect))
               (:transform sub-fr rect to-dpi nil)
               (:transform sub-fr rect to-dpi resized-frames)))
           all-children
           new-rects)))
  resized-frames)


(defn frame-resize [self new-rect]
  (def parent (in self :parent))
  (when (= :layout (in parent :type))
    # This is a toplevel frame, which tracks the monitor
    # geometries and cannot be resized
    (break))

  (let [all-siblings (in parent :children)
        parent-rect (in parent :rect)
        parent-padded-rect (:get-padded-rect parent)
        [old-width old-height] (rect-size (in self :rect))
        [new-width new-height] (rect-size new-rect)
        [parent-width parent-height] (rect-size parent-padded-rect)
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
        (unless (= dw 0)
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
        (unless (= dh 0)
          (:resize parent
                   {:left (in parent-rect :left)
                    :top (in parent-rect :top)
                    :right (in parent-rect :right)
                    :bottom (+ dh (in parent-rect :bottom))}))))))


(defn frame-insert-sub-frame [self index &opt size-ratio direction]
  (def all-children (in self :children))
  (def child-count (length all-children))

  (cond
    (or (empty? all-children)
        (= :window (get-in all-children [0 :type])))
    # A leaf frame, split it
    (let [ratio (if size-ratio
                  size-ratio
                  0.5)
          dir (if direction
                direction
                (error "frame is not split, but no direction is provided"))
          [move-windows? split-ratio-list] (case index
                                             -1 [false [(- 1 ratio)]]
                                             0 [true [ratio]]
                                             1 [false [(- 1 ratio)]]
                                             (errorf "expected index in range [-1 1], got %n" index))]
      (:split self dir 2 split-ratio-list)
      (when move-windows?
        (def to-frame (get-in self [:children 1]))
        (each w all-children
          (:add-child to-frame w))))

    (and direction
         (not= direction (:get-direction self)))
    (error "directions don't match")

    true
    # children are sub-frames
    (let [dir (:get-direction self)
          ratio (if size-ratio
                  size-ratio
                  (/ 1 (+ 1 child-count)))
          idx (if (< index 0)
                (+ 1 index child-count) # -1 means insert to the end of the array
                index)
          ref-frame (if (>= idx child-count)
                      nil
                      (in all-children idx))
          ref-rect (when ref-frame
                     (in ref-frame :rect))]
      (def new-rect
        (case dir
          :horizontal
          (if ref-rect
            {:left (in ref-rect :left)
             :top (in ref-rect :top)
             :right (in ref-rect :left)
             :bottom (in ref-rect :bottom)}
            (let [last-frame (last all-children)
                  last-rect (in last-frame :rect)]
              {:left (in last-rect :right)
               :top (in last-rect :top)
               :right (in last-rect :right)
               :bottom (in last-rect :bottom)}))

          :vertical
          (if ref-rect
            {:left (in ref-rect :left)
             :top (in ref-rect :top)
             :right (in ref-rect :right)
             :bottom (in ref-rect :top)}
            (let [last-frame (last all-children)
                  last-rect (in last-frame :rect)]
              {:left (in last-rect :left)
               :top (in last-rect :bottom)
               :right (in last-rect :right)
               :bottom (in last-rect :bottom)}))))

      (def new-frame (frame new-rect self))
      (array/insert all-children idx new-frame)
      (def padded-rect (:get-padded-rect self))
      (def [width height] (rect-size padded-rect))
      (def resize-rect
        (case dir
          :horizontal
          (let [sub-width (math/floor (* width ratio))]
            {:left 0 :top 0 :right sub-width :bottom (- (in new-rect :bottom) (in new-rect :top))})
          :vertical
          (let [sub-height (math/floor (* height ratio))]
            {:left 0 :top 0 :right (- (in new-rect :right) (in new-rect :left)) :bottom sub-height})))
      (:resize new-frame resize-rect))))


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
          [parent-width parent-height] (rect-size (:get-padded-rect parent))]
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


(defn frame-sync-current-window [self]
  (def children (in self :children))
  (cond
    (empty? children)
    nil

    (= :window (get-in children [0 :type]))
    (when-let [top-win (:get-top-window self)]
      (when (not= top-win (in self :current-child))
        (log/debug "Resetting current window to %n" (in top-win :hwnd))
        (put self :current-child top-win)))

    true
    (each c children
      (:sync-current-window c))))


(defn frame-get-paddings [self &opt scaled filter-fn]
  (default scaled true)
  (default filter-fn identity)

  (def paddings
    (get-margins-or-paddings-from-tags (in self :tags) :padding :paddings))
  (if scaled
    (let [top-fr (:get-top-frame self)
          mon (in top-fr :monitor)
          [dpi-x dpi-y] (in mon :dpi)
          [scale-x scale-y] [(/ dpi-x const/USER-DEFAULT-SCREEN-DPI)
                             (/ dpi-y const/USER-DEFAULT-SCREEN-DPI)]]
      {:top (filter-fn (* scale-y (in paddings :top)))
       :left (filter-fn (* scale-x (in paddings :left)))
       :bottom (filter-fn (* scale-y (in paddings :bottom)))
       :right (filter-fn (* scale-x (in paddings :right)))})
    paddings))


(defn frame-get-padded-rect [self &opt scaled]
  (default scaled true)

  (def paddings (:get-paddings self scaled))
  (shrink-rect (in self :rect) paddings))


(defn frame-get-direction [self]
  (cond
    (= vertical-frame-proto (table/getproto self)) :vertical
    (= horizontal-frame-proto (table/getproto self)) :horizontal
    # The frame is not split
    true nil))


(set frame-proto
     (table/setproto
      @{:split frame-split
        :balance frame-balance
        :flatten frame-flatten
        :transform frame-transform
        :resize frame-resize
        :insert-sub-frame frame-insert-sub-frame
        :close frame-close
        :sync-current-window frame-sync-current-window
        :get-paddings frame-get-paddings
        :get-padded-rect frame-get-padded-rect
        :get-direction frame-get-direction}
      tree-node-proto))


(varfn frame [rect &opt parent children]
  (default children @[])
  (let [node (tree-node :frame parent children
                        :rect rect
                        :tags @{})]
    (table/setproto node frame-proto)))


(defn vertical-frame-calculate-sub-rects [self h-fn &opt count rect]
  (default rect (:get-padded-rect self))

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


(defn horizontal-frame-calculate-sub-rects [self w-fn &opt count rect]
  (default rect (:get-padded-rect self))

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


(defn layout-update-work-areas [self monitors]
  (def top-frames (in self :children))
  (def fr-count (length top-frames))
  (def mon-count (length monitors))

  (def hook-man (in (:get-window-manager self) :hook-manager))

  (cond
    (= fr-count mon-count)
    # Only the resolutions or monitor configurations are changed
    (map (fn [fr mon]
           (unless (= mon (in fr :monitor))
             (:transform fr (in mon :work-area) (in mon :dpi))
             (put fr :monitor mon)
             (:call-hook hook-man :monitor-updated fr)))
         top-frames
         monitors)

    (> fr-count mon-count)
    # Some of the monitors got unplugged
    (let [alive-frames (slice top-frames 0 mon-count)
          dead-frames (slice top-frames mon-count)
          orphan-windows @[]]
      (var main-fr (first alive-frames))
      (map (fn [fr mon]
             (unless (= mon (in fr :monitor))
               (:transform fr (in mon :work-area) (in mon :dpi))
               (put fr :monitor mon)
               (:call-hook hook-man :monitor-updated fr))
             # Find the frame closest to the origin
             (def wa (in mon :work-area))
             (when (< (+ (math/abs (in wa :top))
                         (math/abs (in wa :left)))
                      (+ (math/abs (get-in main-fr [:rect :top]))
                         (math/abs (get-in main-fr [:rect :left]))))
               (set main-fr fr)))
           alive-frames
           monitors)
      (each fr dead-frames
        (array/push orphan-windows ;(:get-all-windows fr)))
      (def move-to-fr (:get-current-frame main-fr))
      (each w orphan-windows
        (:add-child move-to-fr w))
      (put self :children @[;alive-frames])
      (when (find |(= $ (in self :current-child)) dead-frames)
        (put self :current-child main-fr)))

    (< fr-count mon-count)
    # New monitors got plugged in
    (let [old-mons (slice monitors 0 fr-count)
          new-mons (slice monitors fr-count)
          new-frames (map (fn [mon]
                            (def new-fr (frame (in mon :work-area)))
                            (put new-fr :monitor mon)
                            new-fr)
                          new-mons)]
      (map (fn [fr mon]
             (unless (= mon (in fr :monitor))
               (:transform fr (in mon :work-area) (in mon :dpi))
               (put fr :monitor mon)
               (:call-hook hook-man :monitor-updated fr)))
           top-frames
           old-mons)
      (each fr new-frames
        (:add-child self fr)
        (:call-hook hook-man :monitor-updated fr)))))


(def- layout-proto
  (table/setproto
   @{:update-work-areas layout-update-work-areas}
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
  (def [monitors main-idx] (:enumerate-monitors wm))
  (def {:id id :name name} desktop-info)
  (def new-layout
    (layout id name nil
            (map (fn [mon]
                   (def new-fr (frame (in mon :work-area)))
                   (put new-fr :monitor mon)
                   new-fr)
                 monitors)))
  (def to-activate (or main-idx 0))
  (:activate (get-in new-layout [:children to-activate]))
  (:call-hook (in self :hook-manager) :layout-created new-layout)
  (each fr (in new-layout :children)
    (:call-hook (in self :hook-manager) :monitor-updated fr))
  new-layout)


(defn vdc-purge-windows [self &opt pred]
  (def wm (in self :window-manager))
  (def dead @[])
  (each lo (in self :children)
    (def dw
      (if pred
        (:purge-windows lo |(pred $ lo))
        (:purge-windows lo |(window-purge-pred $ wm lo))))
    (array/push dead ;dw)
    (log/debug "Purged %n dead windows from virtual desktop %n"
               (length dw)
               (in lo :id)))
  dead)


(def- virtual-desktop-container-proto
  (table/setproto
   @{:new-layout vdc-new-layout
     :get-current-frame-on-desktop vdc-get-current-frame-on-desktop
     :purge-windows vdc-purge-windows}
   tree-node-proto))


(defn virtual-desktop-container [wm hook-man &opt children]
  (default children @[])
  (def vdc-obj (tree-node :virtual-desktop-container nil children
                          :window-manager wm
                          :hook-manager hook-man))
  (table/setproto vdc-obj virtual-desktop-container-proto))


######### Window manager object #########

(defn wm-transform-hwnd [self hwnd rect &opt tags]
  (transform-hwnd hwnd rect (in self :uia-manager) tags))


(defn wm-reset-hwnd-visual-state [self hwnd &opt restore-minimized restore-maximized]
  (default restore-minimized true)
  (default restore-maximized true)
  (reset-hwnd-visual-state hwnd
                           (get-in self [:uia-manager :com])
                           restore-minimized
                           restore-maximized))


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

  (cond
    (:call-filter-hook (in self :hook-manager) :or :filter-forced-window
       hwnd uia-win exe-path desktop-info)
    :forced

    (:call-filter-hook (in self :hook-manager) :and :filter-window
       hwnd uia-win exe-path desktop-info)
    :normal

    true
    :ignored))


(defn wm-add-hwnd [self hwnd-info &opt manage-state]
  (default manage-state :normal)

  (def {:hwnd hwnd
        :uia-element uia-win
        :exe-path exe-path
        :virtual-desktop desktop-info}
    hwnd-info)

  (log/debug "new window: %n" hwnd)

  (def new-win (window hwnd))
  (when (= :forced manage-state)
    (put (in new-win :tags) :forced true))

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
                   (:get-padded-rect frame-found)
                   (in new-win :tags))
  new-win)


(defn wm-remove-hwnd [self hwnd]
  (when-let [w (:find-hwnd (in self :root) hwnd)]
    (def parent-fr (in w :parent))
    (:remove-child parent-fr w)
    (:call-hook (in self :hook-manager) :window-removed w)
    w))


(defn wm-filter-hwnd [self hwnd &opt uia-win? exe-path? desktop-info?]
  (def uia-win
    (if uia-win?
      (do
        (:AddRef uia-win?)
        uia-win?)
      (:get-hwnd-uia-element self hwnd (get-in self [:uia-manager :focus-cr]))))
  (def exe-path
    (if exe-path?
      exe-path?
      (:get-hwnd-path self hwnd)))
  (def desktop-info
    (if desktop-info?
      desktop-info?
      (:get-hwnd-virtual-desktop self hwnd uia-win)))

  (def desktop-id
    (get-in desktop-info [:id]))

  (with-uia [_uia-win uia-win]
    (cond
      (get-in self [:ignored-hwnds hwnd])
      [false :ignored-window]

      (or (nil? desktop-id)
          (= "{00000000-0000-0000-0000-000000000000}" desktop-id))
      [false [:invalid-virtual-desktop desktop-id]]

      (not (find |(= $ (:GetCachedPropertyValue uia-win UIA_ControlTypePropertyId))
                 [UIA_WindowControlTypeId
                  # Strangely, some top level windows declare that they are pane controls.
                  UIA_PaneControlTypeId]))
      [false [:invalid-control-type (:GetCachedPropertyValue uia-win UIA_ControlTypePropertyId)]]

      (not= 0 (:GetCachedPropertyValue uia-win UIA_IsOffscreenPropertyId))
      [false :offscreen-window]

      (= 0 (:GetCachedPropertyValue uia-win UIA_IsWindowPatternAvailablePropertyId))
      [false :no-window-pattern]

      (= 0 (:GetCachedPropertyValue uia-win UIA_IsTransformPatternAvailablePropertyId))
      [false :no-transform-pattern]

      # We don't check for maximized windows, since we still want
      # to manage them, and transform-hwnd will restore them when
      # doing the resizing.

      (= WindowVisualState_Minimized
         (:GetCachedPropertyValue uia-win UIA_WindowWindowVisualStatePropertyId))
      [false :minimized-window]

      # Minimized and maximized windows always return FALSE for
      # UIA_TransformCanMovePropertyId, so we only check normal
      # windows here. In other words, minimized/maximized windows
      # are always treated as movable.
      (and
        (= WindowVisualState_Normal
           (:GetCachedPropertyValue uia-win UIA_WindowWindowVisualStatePropertyId))
        (= 0 (:GetCachedPropertyValue uia-win UIA_TransformCanMovePropertyId)))
      [false :immovable-window]

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
      [false :cloaked-window]

      (and (not (:jwno-process-elevated? self))
           (hwnd-process-elevated? hwnd))
      [false :elevated-window]

      true
      (let [styles (signed-to-unsigned-32 (GetWindowLong hwnd GWL_STYLE))
            ex-styles (signed-to-unsigned-32 (GetWindowLong hwnd GWL_EXSTYLE))]
        (cond
          (= WS_CHILD (band WS_CHILD styles))
          [false :child-window]

          (= WS_EX_TOOLWINDOW (band WS_EX_TOOLWINDOW ex-styles))
          [false :tool-window]

          (= WS_EX_NOACTIVATE (band WS_EX_NOACTIVATE ex-styles))
          [false :not-activatable-window]

          (= WS_EX_TOPMOST (band WS_EX_TOPMOST ex-styles))
          [false :topmost-window]

          true)))))


(defn wm-ignore-hwnd [self hwnd]
  (put (in self :ignored-hwnds) hwnd true))


(defn wm-clean-up-hwnds [self]
  (def {:hook-manager hook-man} self)

  # Clean up the ignored list
  (def ignored (in self :ignored-hwnds))
  (each h (keys ignored)
    (when (= FALSE (IsWindow h))
      (put ignored h nil)))

  # Clean up dead windows
  # XXX: If the focus change is caused by a closing window, that
  # window may still be alive, so it won't be purged immediately.
  # Maybe I shoud check the hwnds everytime a window is manipulated?
  (let [dead (:purge-windows (in self :root))]
    (each dw dead
      (:call-hook hook-man :window-removed dw))))


(defn wm-focus-changed [self]
  (def {:uia-manager uia-man
        :hook-manager hook-man}
    self)

  (with-uia [uia-win (:get-focused-window uia-man)]
    (def hwnd
      (when uia-win
        (:get_CachedNativeWindowHandle uia-win)))
    (def last-focused-hwnd (in self :last-focused-hwnd))

    # When hwnd is nil, we don't have a valid window focused, and
    # we can't be sure the focus actually changed or not, so we
    # always proceed in that case.
    (when (and hwnd (= hwnd last-focused-hwnd))
      (log/debug "Focus on same window")
      (break))
    (put self :last-focused-hwnd hwnd)

    (log/debug "Focused hwnd = %n" hwnd)

    (:clean-up-hwnds self)

    (:call-hook (in self :hook-manager) :focus-changed hwnd)

    (when (nil? hwnd)
      # No focused window, don't proceed
      (break))

    (when-let [win (:find-hwnd (in self :root) hwnd)]
      # Already managed
      (with-activation-hooks self
        (:activate win))
      (break))

    (def hwnd-info
      (get-hwnd-info hwnd
                     (in self :uia-manager)
                     (in self :vdm-com)
                     uia-win))
    (when (nil? hwnd-info)
      # Bad window
      (break))

    (with-uia [_uia-win (in hwnd-info :uia-element)]
      (def manage-state (:should-manage-hwnd? self hwnd-info))
      (log/debug "manage-state = %n" manage-state)

      (when (= :ignored manage-state)
        (log/debug "Ignoring window: %n" hwnd)
        (break))

      (if-let [new-win (:add-hwnd self hwnd-info manage-state)]
        (with-activation-hooks self
          (:activate new-win))))))


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
    (def manage-state (:should-manage-hwnd? self hwnd-info))
    (log/debug "manage-state = %n" manage-state)

    (when (= :ignored manage-state)
      (log/debug "Ignoring window: %n" hwnd)
      (break))

    (:add-hwnd self hwnd-info manage-state)))


(defn wm-desktop-name-changed [self vd-name]
  (def root (in self :root))
  (def layouts (in root :children))

  (def last-vd-name (in self :last-vd-name))
  (unless (= vd-name last-vd-name)
    # XXX: lo may be nil. The desktop names should be unique, and
    # they should not be changed while Jwno is running.
    (def lo (find |(= (in $ :name) vd-name) layouts))
    (:call-hook (in self :hook-manager) :virtual-desktop-changed vd-name lo)
    (put self :last-vd-name vd-name)
    (when lo
      (with-activation-hooks self
        (:activate self lo)))))


(defn wm-activate [self node]
  (when node
    (:activate node))

  (def uia-man (in self :uia-manager))
  (def defview (in uia-man :def-view))
  (def defview-hwnd (:get_CachedNativeWindowHandle defview))

  (def hwnd
    (cond
      (nil? node)
      defview-hwnd

      (= :window (in node :type))
      (in node :hwnd)

      (or (= :frame (in node :type))
          (= :layout (in node :type)))
      (if-let [cur-win (:get-current-window node)]
        (in cur-win :hwnd)
        defview-hwnd)))

  (log/debug "setting focus to window: %n" hwnd)
  (:set-focus-to-window uia-man hwnd))


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
                         (:get-padded-rect fr)
                         (in w :tags)))

      (or (= :frame (get-in fr [:children 0 :type]))
          (= :layout (get-in fr [:children 0 :type])))
      (each f (in fr :children)
        (:retile self f)))))


(defn wm-enumerate-monitors [self]
  (def monitors @[])
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
       (def [dpi-x dpi-y] (GetDpiForMonitor hmon MDT_DEFAULT))
       (array/push monitors
                   {:rect (in monitor-info :rcMonitor)
                    :work-area (in monitor-info :rcWork)
                    :flags (in monitor-info :dwFlags)
                    :device (in monitor-info :szDevice)
                    :dpi [(int/to-number dpi-x) (int/to-number dpi-y)]})
       (if (> (band (in monitor-info :dwFlags) MONITORINFOF_PRIMARY) (int/u64 0))
         (set main-idx idx))
       (+= idx 1)
       TRUE)))
  (if (= FALSE enum-ret)
    (error "EnumDisplayMonitors failed"))
  (log/debug "monitors = %n" monitors)
  (log/debug "main-idx = %n" main-idx)
  (when (empty? monitors)
    (error "no monitor found"))
  [monitors main-idx])


(defn wm-close-hwnd [self hwnd]
  (PostMessage hwnd WM_CLOSE 0 0))


(defn wm-hwnd-alive? [self hwnd]
  (and (not= FALSE (IsWindow hwnd))
       (not= FALSE (IsWindowVisible hwnd))))


(defn wm-with-activation-hooks [self op-fn]
  (def root (in self :root))
  (def old-frame (:get-current-frame root))
  (def old-win (:get-current-window old-frame))

  (def ret (op-fn))

  (def new-frame (:get-current-frame root))
  (def new-win (:get-current-window new-frame))

  (def hook-man (in self :hook-manager))
  (unless (= new-frame old-frame)
    (:call-hook hook-man :frame-activated new-frame))
  (unless (or (nil? new-win)
              (= new-win old-win))
    (:call-hook hook-man :window-activated new-win))

  ret)


(defn wm-destroy [self]
  (def {:vdm-com vdm-com} self)
  (:Release vdm-com))


(def- window-manager-proto
  @{:focus-changed wm-focus-changed
    :window-opened wm-window-opened
    :desktop-name-changed wm-desktop-name-changed

    :transform-hwnd wm-transform-hwnd
    :reset-hwnd-visual-state wm-reset-hwnd-visual-state
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
    :remove-hwnd wm-remove-hwnd
    :filter-hwnd wm-filter-hwnd
    :ignore-hwnd wm-ignore-hwnd
    :clean-up-hwnds wm-clean-up-hwnds

    :close-hwnd wm-close-hwnd

    :get-pid-path wm-get-pid-path
    :enumerate-monitors wm-enumerate-monitors
    :jwno-process-elevated? wm-jwno-process-elevated?

    :with-activation-hooks wm-with-activation-hooks

    :destroy wm-destroy})


(defn window-manager [uia-man ui-man hook-man]
  (def vdm-com
    (CoCreateInstance CLSID_VirtualDesktopManager
                      nil
                      CLSCTX_INPROC_SERVER
                      IVirtualDesktopManager))
  (def wm-obj
    (table/setproto
     @{:vdm-com vdm-com
       :uia-manager uia-man
       :ui-man ui-man
       :hook-manager hook-man
       :ignored-hwnds @{}
       :last-vd-name (:get_CurrentName (in uia-man :root))}
     window-manager-proto))
  (put wm-obj :root (virtual-desktop-container wm-obj hook-man))

  (:add-hook hook-man :filter-window
     (fn [hwnd uia-win exe-path desktop-info]
       (match (:filter-hwnd wm-obj hwnd uia-win exe-path desktop-info)
         [false reason]
         (do
           (log/debug "Window %n failed to pass default filter: %n" hwnd reason)
           false)

         true
         true)))
  (:add-hook hook-man :frame-activated
     (fn [fr]
       (:update-work-area ui-man (in (:get-top-frame fr) :rect))))

  wm-obj)
