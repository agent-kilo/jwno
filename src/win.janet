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

(defn rotate-array! [arr direction]
  (case direction
    :forward
    (when-let [first-elem (first arr)]
      (array/remove arr 0)
      (array/push arr first-elem))

    :backward
    (when-let [last-elem (last arr)]
      (array/remove arr -1)
      (array/insert arr 0 last-elem))

    (errorf "unknown direction: %n" direction)))


(defn calc-win-coords-in-frame [win-rect fr-rect fit anchor win-scale fr-scale]
  (def [fr-width fr-height] (rect-size fr-rect))
  (def [win-width win-height] (rect-size win-rect))
  (def [fr-scale-x fr-scale-y] fr-scale)
  (def [win-scale-x win-scale-y] win-scale)
  (def scaled (not= win-scale fr-scale))
  (def win-scaled-width
    (if scaled
      (* (/ win-width win-scale-x) fr-scale-x)
      win-width))
  (def win-scaled-height
    (if scaled
      (* (/ win-height win-scale-y) fr-scale-y)
      win-height))

  (def [x y]
    (cond
      (= :center anchor)
      [(math/floor
        (+ (in fr-rect :left)
           (/ fr-width 2)
           (/ win-scaled-width -2)))
       (math/floor
        (+ (in fr-rect :top)
           (/ fr-height 2)
           (/ win-scaled-height -2)))]

      (= :left anchor)
      [(in fr-rect :left)
       (math/floor
        (+ (in fr-rect :top)
           (/ fr-height 2)
           (/ win-scaled-height -2)))]

      (= :right anchor)
      [(math/floor
        (- (in fr-rect :right)
           win-scaled-width))
       (math/floor
        (+ (in fr-rect :top)
           (/ fr-height 2)
           (/ win-scaled-height -2)))]

      (= :top anchor)
      [(math/floor
        (+ (in fr-rect :left)
           (/ fr-width 2)
           (/ win-scaled-width -2)))
       (in fr-rect :top)]

      (= :bottom anchor)
      [(math/floor
        (+ (in fr-rect :left)
           (/ fr-width 2)
           (/ win-scaled-width -2)))
       (math/floor
        (- (in fr-rect :bottom)
           win-scaled-height))]

      (or (= :top-left anchor)
          (= :left-top anchor))
      [(in fr-rect :left)
       (in fr-rect :top)]

      (or (= :top-right anchor)
          (= :right-top anchor))
      [(math/floor
        (- (in fr-rect :right)
           win-scaled-width))
       (in fr-rect :top)]

      (or (= :bottom-left anchor)
          (= :left-bottom anchor))
      [(in fr-rect :left)
       (math/floor
        (- (in fr-rect :bottom)
           win-scaled-height))]

      (or (= :bottom-right anchor)
          (= :right-bottom anchor))
      [(math/floor
        (- (in fr-rect :right)
           win-scaled-width))
       (math/floor
        (- (in fr-rect :bottom)
           win-scaled-height))]

      (errorf "unknown anchor: %n" anchor)))

  (if fit
    (let [fitted-x (max x (in fr-rect :left))
          fitted-width (min (math/floor win-scaled-width) fr-width)
          fitted-y (max y (in fr-rect :top))
          fitted-height (min (math/floor win-scaled-height) fr-height)]
      [fitted-x fitted-y fitted-width fitted-height])
    [x y (math/floor win-scaled-width) (math/floor win-scaled-height)]))


(defn- reset-hwnd-visual-state [hwnd uia-com restore-minimized restore-maximized]
  (with-uia [cr (:CreateCacheRequest uia-com)]
    (:AddPattern cr UIA_WindowPatternId)
    (:AddProperty cr UIA_WindowWindowVisualStatePropertyId)

    (try
      (with-uia [uia-win (:ElementFromHandleBuildCache uia-com hwnd cr)]
        (with-uia [win-pat (:GetCachedPatternAs uia-win
                                                UIA_WindowPatternId
                                                IUIAutomationWindowPattern)]
          (def old-state (:get_CachedWindowVisualState win-pat))
          (when (and restore-minimized
                     (= WindowVisualState_Minimized old-state))
            (:SetWindowVisualState win-pat WindowVisualState_Normal))
          (when (and restore-maximized
                     (= WindowVisualState_Maximized old-state))
            (:SetWindowVisualState win-pat WindowVisualState_Normal))
          old-state))

      ((err fib)
       (log/error "failed to reset visual state for %n: %n%s"
                  hwnd
                  err
                  (get-stack-trace fib))
       nil))))


(defn- get-hwnd-rect [hwnd &opt no-frame?]
  (default no-frame? false)

  (if no-frame?
    (DwmGetWindowAttribute hwnd DWMWA_EXTENDED_FRAME_BOUNDS)
    # else
    (let [[gwr-ret rect] (GetWindowRect hwnd)]
      (when (= FALSE gwr-ret)
        (errorf "GetWindowRect failed for window %n" hwnd))
      rect)))


(defn get-hwnd-dwm-border-margins [hwnd &opt outer-rect]
  (default outer-rect (get-hwnd-rect hwnd false))

  (def inner-rect (get-hwnd-rect hwnd true))

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


(defn- set-window-pos [hwnd x y w h scaled extra-flags]
  (def common-flags
    (bor SWP_NOZORDER SWP_NOACTIVATE extra-flags))
  (def flags
    (if (or (<= w 0) (<= h 0))
      (bor common-flags SWP_NOSIZE)
      common-flags))
  (when (= FALSE (SetWindowPos hwnd nil x y w h flags))
    (errorf "SetWindowPos failed for window %n: %n" hwnd (GetLastError)))
  # XXX: I couldn't work out why SetWindowPos sometimes wouldn't respect
  # the x, y, w and h values when moving windows between monitors with
  # different DPIs (besides the DPI-unaware window's case described in
  # transform-hwnd), but calling it again SEEMED to fix it.
  (when scaled
    (when (= FALSE (SetWindowPos hwnd nil x y w h flags))
      (errorf "SetWindowPos failed for window %n: %n" hwnd (GetLastError)))))


(defn calc-window-rect-with-margins [orig-rect cur-scale scale dwm-margins margins]
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
            (def swp-flags (in tags :swp-flags 0))

            # XXX: When dealing with QT windows, the bounding rectangle returned by
            # uia-win will be incorrect, and I have absolutely no idea why. GetWindowRect
            # returns the right values though, so that's what we use here.
            (def win-rect (get-hwnd-rect hwnd))

            (def scale (calc-pixel-scale orig-rect))
            (def cur-scale (calc-pixel-scale win-rect))
            (def scaled (not= scale cur-scale))

            (def margins (get-margins-or-paddings-from-tags tags :margin :margins))
            (def dwm-margins (get-hwnd-dwm-border-margins hwnd win-rect))

            (log/debug "win-rect = %n" win-rect)
            (def rect
              (calc-window-rect-with-margins orig-rect
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
                (let [[x y _w _h] (calc-win-coords-in-frame win-rect rect false anchor cur-scale scale)]
                  (set-window-pos hwnd x y 0 0 scaled swp-flags))

                no-expand
                (let [[x y w h] (calc-win-coords-in-frame win-rect rect true anchor cur-scale scale)]
                  (set-window-pos hwnd x y w h scaled swp-flags))

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
                  (set-window-pos hwnd x y w h scaled swp-flags))))))))

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


(defn- try-to-get-window-desktop-id [vd-man hwnd]
  (match (:call-method vd-man :GetWindowDesktopId [hwnd])
    [true did]   did
    [false _err] nil))


(defn- get-hwnd-virtual-desktop-id [hwnd vd-man]
  # Only top-level windows can be managed by virtual desktops
  (var cur-hwnd (GetAncestor hwnd GA_ROOT))
  (var desktop-id? (try-to-get-window-desktop-id vd-man cur-hwnd))

  (while (or # GetWindowDesktopId failed
             (nil? desktop-id?)
             # Window not managed by virtual desktops
             (= desktop-id? "{00000000-0000-0000-0000-000000000000}"))
    # Try the owner instead
    (def owner (GetWindow cur-hwnd GW_OWNER))
    (when (null? owner)
      (break))

    (set cur-hwnd owner)
    (set desktop-id? (try-to-get-window-desktop-id vd-man cur-hwnd)))

  desktop-id?)


(defn- get-current-virtual-desktop-name [uia-man]
  # XXX: The returned root element will always have the name of
  # the current virtual desktop, but this is not documented at
  # all.
  (with-uia [root-elem (:get-root uia-man)]
    (if root-elem
      (:get_CachedName root-elem)
      nil)))


(defn- get-hwnd-virtual-desktop [hwnd? uia-man vd-man &opt uia-win?]
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
      (let [desktop-id (get-hwnd-virtual-desktop-id hwnd vd-man)
            [stat on-cur-vd?] (:call-method vd-man :IsWindowOnCurrentVirtualDesktop [hwnd])
            # XXX: The name of the HWND's virtual desktop can only be
            # retrieved when that virtual desktop is active. Find a way
            # around this?
            desktop-name (if (or (not stat) # IsWindowOnCurrentVirtualDesktop failed
                                 (= FALSE on-cur-vd?))
                           nil
                           (get-current-virtual-desktop-name uia-man))]
        (if (and (nil? desktop-id)
                 (nil? desktop-name))
          nil
          {:id desktop-id :name desktop-name})))))


#
# Give hwnd-info objects a prototype, so that they can
# be used with with-uia macro.
#
(def hwnd-info-proto
  {:AddRef  (fn hwnd-info-addref [self]
              (:AddRef (in self :uia-element)))
   :Release (fn hwnd-info-release [self]
              (:Release (in self :uia-element)))})


(defn- get-hwnd-info [hwnd? uia-man vd-man &opt uia-win?]
  (def [hwnd uia-win]
    (normalize-hwnd-and-uia-element hwnd?
                                    uia-win?
                                    (in uia-man :com)
                                    (in uia-man :focus-cr)))

  (with-uia [_uia-win uia-win]
    (def exe-path
      (unless (nil? hwnd)
        # get-hwnd-path needs PROCESS_QUERY_INFORMATION and PROCESS_VM_READ
        # access rights, but some processes explicitly deny them (e.g.
        # KeePassXC, to prevent memory dumps), so exe-path being nil here
        # may not necessarily mean that the hwnd is invalid. That's why we
        # don't check for nil exe-path here.
        (get-hwnd-path hwnd)))
    (def desktop-info
      (unless (nil? hwnd)
        (get-hwnd-virtual-desktop hwnd uia-man vd-man uia-win)))

    (cond
      (nil? uia-win)
      # normalize-hwnd-and-uia-element failed
      nil

      (nil? hwnd)
      # normalize-hwnd-and-uia-element failed
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
        (struct/with-proto hwnd-info-proto
                           :hwnd hwnd
                           :uia-element uia-win
                           :exe-path exe-path
                           :virtual-desktop desktop-info)))))


(defn- window-purge-pred [win wm layout]
  (def hwnd (in win :hwnd))
  (or (not (:alive? win))
      (not= (in layout :id)
            (get-hwnd-virtual-desktop-id hwnd (in wm :vd-manager)))))


(defn dump-tag-value [x]
  (def x-type (type x))
  (cond
    (or (= x-type :array)
        (= x-type :tuple))
    (tuple/slice (map |(dump-tag-value $) x))

    (or (= x-type :table)
        (= x-type :struct))
    (do
      (def new-tab @{})
      (eachp [k v] x
             (def new-k (dump-tag-value k))
             (def new-v (dump-tag-value v))
             (put new-tab new-k new-v))
      (table/to-struct new-tab))

    (or (= x-type :core/s64)
        (= x-type :core/u64))
    (int/to-number x) # XXX: type conversion

    (abstract? x)
    (errorf "non-trivial type: %n" x)

    (or (= x-type :function)
        (= x-type :cfunction)
        (= x-type :fiber))
    (errorf "non-trivial type: %n" x)

    (= x-type :pointer)
    (errorf "non-trivial type: %n" x)

    true
    x))


(defn dump-tags [orig-tags]
  (def tags @{})
  (each k (keys orig-tags)
    (if (or (keyword? k)
            (symbol? k))
      (do
        (def v
          (try
            (dump-tag-value (in orig-tags k))
            ((err fib)
             (if (and (string? err)
                      (string/has-prefix? "non-trivial type:" err))
               (do
                 (log/warning "non-trivial value in tag %n: %n\n%s"
                              k
                              (in orig-tags k)
                              (get-stack-trace fib))
                 nil)
               # else
               (do
                 (log/error "failed to dump tag value: %n\n%s"
                            err
                            (get-stack-trace fib))
                 (error err))))))
        (put tags k v))

      # else
      (log/warning "ignoring tag: %n" k)))
  (table/to-struct tags))


(defn hwnd-list-to-map [hwnd-list]
  (cond
    (table? hwnd-list)
    hwnd-list

    (indexed? hwnd-list)
    (let [hwnd-map @{}]
      (each hwnd hwnd-list
        (put hwnd-map (pointer-to-number hwnd) hwnd))
      hwnd-map)

    true
    (errorf "unsupported hwnd list: %n" hwnd-list)))


(defn enum-all-hwnds []
  (def hwnd-list @[])
  (EnumChildWindows
   nil
   (fn [hwnd]
     (array/push hwnd-list hwnd)
     1 # !!! IMPORTANT
     ))
  hwnd-list)


(defn get-dumped-viewport [dumped-frame]
  (def [tp rect viewport _tags _children] dumped-frame)
  (unless (= tp :frame)
    (errorf "no viewport for dumped type %n" tp))
  # Fallback to rect when viewport is not set
  (if (nil? viewport)
    rect
    viewport))


(defn calc-loading-split-params [dumped-children]
  (var direction nil)
  (var last-rect nil)
  (var total-width 0)
  (var total-height 0)

  (def child-widths @[])
  (def child-heights @[])

  # First pass: Calculate total-width, total-height and direction
  (each c dumped-children
    (def child-type (first c))

    (case child-type
      :window
      (do
        (set direction nil)
        # out of each loop
        (break))

      :frame
      (do
        (def rect (get-dumped-viewport c))
        (def [width height] (rect-size rect))
        (array/push child-widths width)
        (array/push child-heights height)
        (if last-rect
          (cond
            (= (in rect :top)
               (in last-rect :top))
            (do
              (set direction :horizontal)
              (+= total-width width))

            (= (in rect :left)
               (in last-rect :left))
            (do
              (set direction :vertical)
              (+= total-height height))

            true
            (errorf "unaligned child frames in dumped data: %n, %n" rect last-rect))

          # else
          (do
            (set total-width width)
            (set total-height height)))

        (set last-rect rect))

      (errorf "unknown child node type: %n" child-type)))

  (unless direction
    # early return
    (break nil))

  # Second pass: Calculate actual ratios
  (def ratios
    (case direction
      :horizontal
      (map |(/ $ total-width) child-widths)

      :vertical
      (map |(/ $ total-height) child-heights)))

  [direction ratios])


(defn find-closest-frame [rect all-frames]
  (def [center-x center-y] (rect-center rect))
  (var min-dist math/int-max)
  (var found nil)
  (var found-idx nil)
  (eachp [idx fr] all-frames
    (def fr-rect (in fr :rect))
    (def [fr-center-x fr-center-y] (rect-center fr-rect))
    (def dx (- center-x fr-center-x))
    (def dy (- center-y fr-center-y))
    (def dist (+ (* dx dx) (* dy dy)))
    (when (< dist min-dist)
      (set found fr)
      (set found-idx idx)
      (set min-dist dist)))
  [found found-idx min-dist])


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


(defn tree-node-get-all-leaf-frames [self &opt arr]
  (default arr @[])

  (def children (in self :children))
  (cond
    (= :window (in self :type))
    arr

    (not= :frame (in self :type)) # it's another container type
    (do
      (each c children
        (:get-all-leaf-frames c arr))
      arr)

    (empty? children)
    (do
      (array/push arr self)
      arr)

    (= :window (in (first children) :type))
    (do
      (array/push arr self)
      arr)

    true # children are other frames
    (do
      (each c children
        (:get-all-leaf-frames c arr))
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
                        (if (:visible? w)
                          (do
                            (set top-win w)
                            # Stop enumeration
                            0)
                          # Invisible window, carry on
                          1)
                        # Not our window, carry on
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


(defn tree-node-get-current-layout [self]
  (case (in self :type)
    :window
    (:get-layout self)

    :frame
    (:get-layout self)

    :layout
    self

    :virtual-desktop-container
    (in self :current-child)))


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
  (var min-dist (math/abs (- (in (:get-viewport found) rect-key) reference)))
  (each child (slice children 1)
    (def dist (math/abs (- (in (:get-viewport child) rect-key) reference)))
    (when (< dist min-dist)
      (set min-dist dist)
      (set found child)))
  found)


(defn- get-adjacent-frame-impl-descent [orig-node node dir]
  (def children (in node :children))
  (def orig-node-vp (:get-viewport orig-node))
  (def {:top orig-top :left orig-left} orig-node-vp)

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

    (def node-vp (:get-viewport node))

    (if (not= :layout (in parent :type))
      (case dir
        :left
        (loop [i :down-to [(- fr-idx 1) 0]]
          (def sibling (in all-siblings i))
          (def sibling-vp (:get-viewport sibling))
          (when (< (in sibling-vp :left) (in node-vp :left))
            (set adj-fr sibling)
            (break)))
        :right
        (loop [i :range [(+ fr-idx 1) (length all-siblings)]]
          (def sibling (in all-siblings i))
          (def sibling-vp (:get-viewport sibling))
          (when (> (in sibling-vp :left) (in node-vp :left))
            (set adj-fr sibling)
            (break)))
        :up
        (loop [i :down-to [(- fr-idx 1) 0]]
          (def sibling (in all-siblings i))
          (def sibling-vp (:get-viewport sibling))
          (when (< (in sibling-vp :top) (in node-vp :top))
            (set adj-fr sibling)
            (break)))
        :down
        (loop [i :range [(+ fr-idx 1) (length all-siblings)]]
          (def sibling (in all-siblings i))
          (def sibling-vp (:get-viewport sibling))
          (when (> (in sibling-vp :top) (in node-vp :top))
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
          (def sibling-vp (:get-viewport sibling))
          (def sib-prop-val (in sibling-vp sib-dist-prop))
          (def node-prop-val (in node-vp node-dist-prop))
          (when (and (not= sibling node)
                     (cmp-op node-prop-val sib-prop-val))
            (def cur-dist (math/abs (- node-prop-val sib-prop-val)))
            (when (< cur-dist min-dist)
              (set min-dist cur-dist)
              (set adj-fr sibling))))))

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


(defn tree-node-purge-windows [self &opt pred? dead-arr]
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
    dead-arr

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
      
      (when dead-arr
        (each dw dead
          (array/push dead-arr dw))
        dead-arr))

    true # children are other container nodes
    (do
      (each c children
        (:purge-windows c pred dead-arr))
      dead-arr)))


(defn tree-node-get-layout [self]
  (cond
    (nil? self)
    nil

    (= :layout (in self :type))
    self

    (has-key? self :parent)
    (:get-layout (in self :parent))

    true
    nil))


(defn tree-node-get-root [self]
  (if (= :virtual-desktop-container (in self :type))
    self
    (if-let [layout (:get-layout self)]
      (in layout :parent)
      nil)))


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


(defn tree-node-print-subtree [self &opt level indent-width indent-char wm]
  (default level 0)
  (default indent-width const/DEFAULT-FRAME-TREE-DUMP-INDENT-WIDTH)
  (default indent-char const/DEFAULT-FRAME-TREE-DUMP-INDENT-CHAR)
  (default wm (:get-window-manager self))

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
    (if-let [win-info (:get-info self wm)]
      (with-uia [_uia-win (in win-info :uia-element)]
        (def more-indent
          (string indent (buffer/new-filled indent-width indent-char)))
        (def hwnd (in self :hwnd))
        (printf "%sWindow (hwnd=%n)" indent hwnd)
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
                ;(unwrap-rect (get-hwnd-rect hwnd false)))
        (printf "%sExtended Frame Bounds: {l:%d,t:%d,r:%d,b:%d}"
                more-indent
                ;(unwrap-rect (get-hwnd-rect hwnd true)))
        (def dpia-ctx (GetWindowDpiAwarenessContext hwnd))
        (def dpi-awareness (GetAwarenessFromDpiAwarenessContext dpia-ctx))
        (printf "%sDPI Awareness: %n" more-indent dpi-awareness)
        (printf "%sVirtual Desktop ID: %s"
                more-indent
                (get-in win-info [:virtual-desktop :id])))

      (printf "%sWindow (hwnd=%n,failed to get info)" indent))

    (printf "%sUnknown (%n)" indent self))

  (when-let [children (in self :children)]
    (each child children
      (:print-subtree child (+ 1 level) indent-width indent-char wm))))


(defn tree-node-clear-children [self]
  (when-let [child-arr (in self :children)]
    (def children (slice child-arr))
    (array/clear child-arr)
    (put self :current-child nil)
    (each c children
      (put c :parent nil))
    (when (= :frame (in self :type)) 
      # The frame is now empty, reset its direction
      (def dir (:get-direction self))
      (if (or (= dir :horizontal)
              (= dir :vertical))
        (table/setproto self frame-proto)
        # else
        (unless (= nil dir)
          (log/warning
           "unknown direction %n for frame %n, skip resetting prototype"
           dir (in self :rect)))))))


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
    :clear-children tree-node-clear-children
    :get-all-windows tree-node-get-all-windows
    :get-all-leaf-frames tree-node-get-all-leaf-frames
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
    :get-current-layout tree-node-get-current-layout
    :get-root tree-node-get-root
    :get-window-manager tree-node-get-window-manager
    :get-top-frame tree-node-get-top-frame
    :get-depth tree-node-get-depth
    :print-subtree tree-node-print-subtree})


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


# XXX: Check cloaking states as well?
(defn window-visible? [self]
  (and (:alive? self)
       (= FALSE (IsIconic (in self :hwnd)))))


(defn window-on-current-virtual-desktop? [self &opt wm]
  (default wm (:get-window-manager self))
  (def [stat ret]
    (:call-method (in wm :vd-manager)
                  :IsWindowOnCurrentVirtualDesktop
                  [(in self :hwnd)]))
  (unless stat
    (errorf "failed to get virtual desktop info for window %n" (in self :hwnd)))
  (not= FALSE ret))


(defn window-transform [self rect &opt tags wm]
  (default tags @{})
  (default wm (:get-window-manager self))
  (:transform-hwnd wm
                   (in self :hwnd)
                   rect
                   (merge (in self :tags) tags)))


(defn window-reset-visual-state [self &opt restore-minimized restore-maximized wm]
  (default wm (:get-window-manager self))
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


(defn window-get-uia-element [self &opt cr wm]
  (default wm (:get-window-manager self))
  (:get-hwnd-uia-element wm (in self :hwnd) cr))


(defn window-get-virtual-desktop [self &opt uia-win wm]
  (default wm (:get-window-manager self))
  (:get-hwnd-virtual-desktop wm (in self :hwnd) uia-win))


(defn window-get-info [self &opt wm]
  (default wm (:get-window-manager self))
  (:get-hwnd-info wm (in self :hwnd)))


(defn window-get-margins [self &opt scaled filter-fn]
  (default scaled true)
  (default filter-fn identity)

  (def margins
    (get-margins-or-paddings-from-tags (in self :tags) :margin :margins))
  (if scaled
    (let [win-rect (get-hwnd-rect (in self :hwnd))
          [scale-x scale-y] (calc-pixel-scale win-rect)]
      {:top (filter-fn (* scale-y (in margins :top)))
       :left (filter-fn (* scale-x (in margins :left)))
       :bottom (filter-fn (* scale-y (in margins :bottom)))
       :right (filter-fn (* scale-x (in margins :right)))})
    margins))


(defn window-get-dwm-border-margins [self &opt bounding-rect]
  (def hwnd (in self :hwnd))
  (get-hwnd-dwm-border-margins hwnd bounding-rect))


(defn window-get-rect [self &opt no-frame?]
  (get-hwnd-rect (in self :hwnd) no-frame?))


(defn window-set-focus [self &opt wm activate?]
  (default wm (:get-window-manager self))
  (default activate? (if (dyn :sync-focus) true false))

  (def old-v-state (:reset-visual-state self true false wm))
  (def parent (in self :parent))
  (when (and parent
             (= old-v-state WindowVisualState_Minimized))
    (:transform self (:get-padded-rect parent) nil wm))
  (:set-focus-to-hwnd wm (in self :hwnd))
  (when activate?
    (:activate self)))


(defn window-dump [self]
  [:window
   (pointer-to-number (in self :hwnd))
   (dump-tags (in self :tags))])


(defn window-load [self dumped]
  (def [dump-type hwnd-num tags] dumped)

  (unless (= :window dump-type)
    (errorf "can not restore dump type to a window: %n" dump-type))
  (unless (= hwnd-num
             (pointer-to-number (in self :hwnd)))
    (log/debug "restoring dump data for 0x%x to %n" hwnd-num (in self :hwnd)))

  (table/clear (in self :tags))
  (eachp [k v] tags
    (put (in self :tags) k v))

  # Return an empty table, to make it consistent with other
  # restore-* functions
  @{})


(def- window-proto
  (table/setproto
   @{:close window-close
     :alive? window-alive?
     :visible? window-visible?
     :on-current-virtual-desktop? window-on-current-virtual-desktop?
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
     :get-dwm-border-margins window-get-dwm-border-margins
     :get-rect window-get-rect
     :set-focus window-set-focus
     :dump window-dump
     :load window-load}
   tree-node-proto))


(defn window [hwnd &opt parent]
  (let [node (tree-node :window parent nil
                        :hwnd hwnd
                        :tags @{})]
    (table/setproto node window-proto)))


######### Frame object #########

(defn calc-viewport-transform [old-viewport new-viewport old-rect direction]
  (def [old-vp-width old-vp-height] (rect-size old-viewport))
  (def [old-vp-center-x old-vp-center-y] (rect-center old-viewport))
  (def [new-vp-width new-vp-height] (rect-size new-viewport))
  (def [new-vp-center-x new-vp-center-y] (rect-center new-viewport))
  (def [old-rc-width old-rc-height] (rect-size old-rect))
  (def old-top-len (- old-vp-center-y (in old-rect :top)))
  (def old-bottom-len (- (in old-rect :bottom) old-vp-center-y))
  (def old-left-len (- old-vp-center-x (in old-rect :left)))
  (def old-right-len (- (in old-rect :right) old-vp-center-x))
  (def rx (/ new-vp-width old-vp-width))
  (def ry (/ new-vp-height old-vp-height))

  (case direction
    :horizontal
    {:left (- new-vp-center-x (math/floor (* old-left-len rx)))
     :top (in new-viewport :top)
     :right (+ new-vp-center-x (math/floor (* old-right-len rx)))
     :bottom (in new-viewport :bottom)}

    :vertical
    {:left (in new-viewport :left)
     :top (- new-vp-center-y (math/floor (* old-top-len ry)))
     :right (in new-viewport :right)
     :bottom (+ new-vp-center-y (math/floor (* old-bottom-len ry)))}

    nil
    {:left (- new-vp-center-x (math/floor (* old-left-len rx)))
     :top (- new-vp-center-y (math/floor (* old-top-len ry)))
     :right (+ new-vp-center-x (math/floor (* old-right-len rx)))
     :bottom (+ new-vp-center-y (math/floor (* old-bottom-len ry)))}))


(defn frame-get-viewport [self]
  (if-let [vp (in self :viewport)]
    vp
    #else
    (in self :rect)))


(defn frame-set-viewport [self viewport]
  (def old-viewport (:get-viewport self))
  (def old-rect (in self :rect))
  (def direction (:get-direction self))
  (when (nil? direction)
    (error "cannot set viewport to a leaf frame"))
  (def new-rect (calc-viewport-transform old-viewport viewport old-rect direction))
  (put self :rect new-rect)
  (put self :viewport viewport))


(defn frame-remove-viewport [self]
  (unless (:constrained? self)
    (def viewport (in self :viewport))
    (put self :viewport nil)
    (:transform self viewport)))


(defn frame-constrained? [self]
  (not (has-key? self :viewport)))


(defn frame-move-into-viewport [self]
  (def parent (in self :parent))
  (when (or (nil? parent)
            (not= :frame (in parent :type)))
    # Early return
    (break nil))

  (def moved (:move-into-viewport parent))

  (when (:constrained? parent)
    # Position in parent is fixed, early return
    (break moved))

  (def parent-viewport (:get-padded-viewport parent))
  (def self-viewport (:get-viewport self))
  (when (= self-viewport (intersect-rect self-viewport parent-viewport))
    # self is already fully visible, early return
    (break moved))

  (def [parent-vp-center-x parent-vp-center-y] (rect-center parent-viewport))
  (def [self-vp-center-x self-vp-center-y] (rect-center self-viewport))

  (def parent-rect (in parent :rect))
  (def parent-new-rect
    (case (:get-direction parent)
      :horizontal
      (let [dx (- self-vp-center-x parent-vp-center-x)]
        (if (< 0 dx)
          # self is on the right side of parent's viewport
          (do
            (def dist (- (in self-viewport :right) (in parent-viewport :right)))
            {:left (- (in parent-rect :left) dist)
             :top (in parent-rect :top)
             :right (- (in parent-rect :right) dist)
             :bottom (in parent-rect :bottom)})
          # else, self is on the left side
          (do
            (def dist (- (in parent-viewport :left) (in self-viewport :left)))
            {:left (+ (in parent-rect :left) dist)
             :top (in parent-rect :top)
             :right (+ (in parent-rect :right) dist)
             :bottom (in parent-rect :bottom)})))

      :vertical
      (let [dy (- self-vp-center-y parent-vp-center-y)]
        (if (< 0 dy)
          # self is on the bottom side of parent's viewport
          (do
            (def dist (- (in self-viewport :bottom) (in parent-viewport :bottom)))
            {:left (in parent-rect :left)
             :top (- (in parent-rect :top) dist)
             :right (in parent-rect :right)
             :bottom (- (in parent-rect :bottom) dist)})
          # else, self is on the top side
          (do
            (def dist (- (in parent-viewport :top) (in self-viewport :top)))
            {:left (in parent-rect :left)
             :top (+ (in parent-rect :top) dist)
             :right (in parent-rect :right)
             :bottom (+ (in parent-rect :bottom) dist)})))

      (error "inconsistent states for frame tree")))

  (put parent :rect parent-new-rect)
  # Re-calculate children rects, including self
  (:transform parent (in parent :viewport))
  (or moved parent))


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

  (def [width height] (rect-size (:get-padded-rect self)))

  (def new-rects
    (case direction
      :horizontal
      (do
        (table/setproto self horizontal-frame-proto)
        (:calculate-sub-rects self
                              (fn [_ i]
                                (def r (in full-ratios i))
                                (if (< r 1)
                                  (math/floor (* r width))
                                  # else, it's an absolute value
                                  r))
                              n))

      :vertical
      (do
        (table/setproto self vertical-frame-proto)
        (:calculate-sub-rects self
                              (fn [_ i]
                                (def r (in full-ratios i))
                                (if (< r 1)
                                  (math/floor (* r height))
                                  # else, it's an absolutely value
                                  r))
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
               (def old-rect (:get-viewport sub-fr))
               (if (:constrained? sub-fr)
                 (put sub-fr :rect rect)
                 # else
                 (:set-viewport sub-fr rect))
               # Only keep track of leaf frames that actually have their rects altered
               (unless (or (rect-same-size? rect old-rect)
                           (= :frame (get-in sub-fr [:children 0 :type])))
                 (when resized-frames
                   (array/push resized-frames sub-fr)))
               (:balance sub-fr recursive resized-frames))
             all-children
             new-rects)
        (map (fn [sub-fr rect]
               (:transform sub-fr rect resized-frames))
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
  (table/setproto self frame-proto)
  # Leaf frames with viewports are...weird, remove them
  (:remove-viewport self))


(defn frame-transform [self rect &opt resized-frames]
  (def old-padded-rect (:get-padded-rect self))

  (if (:constrained? self)
    (put self :rect rect)
    # else, transform the viewport instead
    (:set-viewport self rect))

  (def new-padded-rect (:get-padded-rect self))

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
    (let [children-rect (union-rect ;(map |(:get-viewport $) all-children))
          dx (- (in new-padded-rect :left) (in children-rect :left))
          dy (- (in new-padded-rect :top) (in children-rect :top))
          dw (+ (- dx)
                (- (in new-padded-rect :right)
                   (in children-rect :right)))
          dh (+ (- dy)
                (- (in new-padded-rect :bottom)
                   (in children-rect :bottom)))
          [old-padded-width old-padded-height] (rect-size children-rect)]
      (log/debug "children-rect = %n, old-padded-rect = %n" children-rect old-padded-rect)
      (def calc-fn
        (cond
          (= horizontal-frame-proto (table/getproto self))
          (fn [sub-fr _i]
            (let [w (rect-width (:get-viewport sub-fr))
                  wr (/ w old-padded-width)
                  sub-dw (math/floor (* wr dw))]
              (+ w sub-dw)))

          (= vertical-frame-proto (table/getproto self))
          (fn [sub-fr _i]
            (let [h (rect-height (:get-viewport sub-fr))
                  hr (/ h old-padded-height)
                  sub-dh (math/floor (* hr dh))]
              (+ h sub-dh)))))
      (def new-rects (:calculate-sub-rects self calc-fn nil new-padded-rect))
      (map (fn [sub-fr rect]
             (if (rect-same-size? rect (in sub-fr :rect))
               (:transform sub-fr rect nil)
               (:transform sub-fr rect resized-frames)))
           all-children
           new-rects)))
  resized-frames)


(defn frame-resize [self new-rect]
  (def parent (in self :parent))
  (when (= :layout (in parent :type))
    # This is a toplevel frame, which tracks the monitor
    # geometries. Only unconstrained toplevel frames can
    # be resized.
    (unless (:constrained? self)
      (let [[new-width new-height] (rect-size new-rect)
            old-rect (in self :rect)]
        (def adjusted-rect
          (cond
            (= horizontal-frame-proto (table/getproto self))
            {:left (in old-rect :left)
             :top (in old-rect :top)
             :right (+ new-width (in old-rect :left))
             :bottom (in old-rect :bottom)}

            (= vertical-frame-proto (table/getproto self))
            {:left (in old-rect :left)
             :top (in old-rect :top)
             :right (in old-rect :right)
             :bottom (+ new-height (in old-rect :top))}

            true
            # An unconstrained leaf frame, resize freely
            {:left (in old-rect :left)
             :top (in old-rect :top)
             :right (+ new-width (in old-rect :left))
             :bottom (+ new-height (in old-rect :top))}))
        (def viewport (in self :viewport))
        (defer
          (put self :viewport viewport)
          # body, temporarily disable the viewport, to transform
          # self's actual rect
          (put self :viewport nil)
          (:transform self adjusted-rect))))

    # Early return
    (break))

  (let [all-siblings (in parent :children)
        parent-rect (in parent :rect)
        parent-viewport (:get-viewport parent)
        parent-padded-rect (:get-padded-rect parent)
        parent-constrained (:constrained? parent)
        [old-width old-height] (rect-size (:get-viewport self))
        [new-width new-height] (rect-size new-rect)
        [parent-width parent-height] (rect-size parent-padded-rect)
        dw (- new-width old-width)
        dh (- new-height old-height)
        avail-h (- parent-height old-height)
        avail-w (- parent-width old-width)]
    (cond
      (= vertical-frame-proto (table/getproto parent))
      (do
        (unless parent-constrained
          (put parent :rect {:left (in parent-rect :left)
                             :top (in parent-rect :top)
                             :right (in parent-rect :right)
                             :bottom (+ dh (in parent-rect :bottom))}))
        (def new-rects
          (if parent-constrained
            (:calculate-sub-rects
               parent
               (fn [sib-fr _i]
                 (def sib-height (rect-height (:get-viewport sib-fr)))
                 (def sib-dh
                   (if (= sib-fr self)
                     dh
                     (math/floor (* (- dh) (/ sib-height avail-h)))))
                 (+ sib-height sib-dh)))
            # else
            (:calculate-sub-rects
               parent
               (fn [sib-fr _i]
                 (if (= sib-fr self)
                   new-height
                   (rect-height (:get-viewport sib-fr)))))))
        (map (fn [sib-fr rect]
               (:transform sib-fr rect))
             all-siblings
             new-rects)
        (unless (= dw 0)
          (:resize parent
                   {:left (in parent-viewport :left)
                    :top (in parent-viewport :top)
                    :right (+ dw (in parent-viewport :right))
                    :bottom (in parent-viewport :bottom)})))

      (= horizontal-frame-proto (table/getproto parent))
      (do
        (unless parent-constrained
          (put parent :rect {:left (in parent-rect :left)
                             :top (in parent-rect :top)
                             :right (+ dw (in parent-rect :right))
                             :bottom (in parent-rect :bottom)}))
        (def new-rects
          (if parent-constrained
            (:calculate-sub-rects
               parent
               (fn [sib-fr _i]
                 (def sib-width (rect-width (:get-viewport sib-fr)))
                 (def sib-dw
                   (if (= sib-fr self)
                     dw
                     (math/floor (* (- dw) (/ sib-width avail-w)))))
                 (+ sib-width sib-dw)))
            # else
            (:calculate-sub-rects
               parent
               (fn [sib-fr _i]
                 (if (= sib-fr self)
                   new-width
                   (rect-width (:get-viewport sib-fr)))))))
        (map (fn [sib-fr rect]
               (:transform sib-fr rect))
             all-siblings
             new-rects)
        (unless (= dh 0)
          (:resize parent
                   {:left (in parent-viewport :left)
                    :top (in parent-viewport :top)
                    :right (in parent-viewport :right)
                    :bottom (+ dh (in parent-viewport :bottom))}))))))


(defn calc-insert-split-params [fr index ratio-or-size direction]
  (def rs
    (if ratio-or-size
      ratio-or-size
      # else, defaults to half of fr
      0.5))
  (def other-rs
    (if (> 1 rs)
      (- 1 rs)
      # else, rs is absolute size, in pixels
      (let [fr-rect (:get-padded-rect fr)
            fr-size (case direction
                      :horizontal (rect-width  fr-rect)
                      :vertical   (rect-height fr-rect)
                      (errorf "invalid direction: %n" direction))]
        (- fr-size rs))))
  (case index
    -1 [0 [other-rs]]
    0  [1 [rs]]
    1  [0 [other-rs]]
    (errorf "expected index in range [-1 1], got %n" index)))


(defn frame-insert-sub-frame [self index &opt size-ratio direction]
  (def all-children (in self :children))
  (def child-count (length all-children))

  (cond
    (or (empty? all-children)
        (= :window (get-in all-children [0 :type])))
    # A leaf frame, split it
    (let [dir (if direction
                direction
                (error "frame is not split, but no direction is provided"))
          [move-to-idx split-ratio-list] (calc-insert-split-params self index size-ratio dir)]
      (:split self dir 2 split-ratio-list)
      (unless (= 0 move-to-idx)
        (def to-frame (get-in self [:children 1]))
        (each w all-children
          (:add-child to-frame w))))

    (and direction
         (not= direction (:get-direction self)))
    (do
      (def cur-child (in self :current-child))
      (def viewport (in self :viewport))
      (def rect (in self :rect))
      (def proto (table/getproto self))

      (put self :children @[])
      (put self :current-child nil)
      (put self :viewport nil)
      (when viewport
        (put self :rect viewport))
      (table/setproto self frame-proto)

      (def [move-to-idx split-ratio-list]
        (calc-insert-split-params self index size-ratio direction))
      (:split self direction 2 split-ratio-list)

      (def move-to-fr (get-in self [:children move-to-idx]))
      (def move-to-rect (in move-to-fr :rect))
      (put move-to-fr :rect rect)
      (put move-to-fr :viewport viewport)
      (put move-to-fr :current-child cur-child)
      (each c all-children
        (:add-child move-to-fr c))
      (table/setproto move-to-fr proto)
      (:transform move-to-fr move-to-rect))

    true
    # children are sub-frames
    (let [dir (:get-direction self)
          ratio (cond
                  size-ratio
                  size-ratio

                  (:constrained? self)
                  (/ 1 (+ 1 child-count))

                  # unconstrained frame, default to half of the viewport size
                  true
                  0.5)
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
      (def padded-viewport (:get-padded-viewport self))
      (def [width height] (rect-size padded-viewport))
      (def resize-rect
        (case dir
          :horizontal
          (let [sub-width (if (> 1 ratio)
                            (math/floor (* width ratio))
                            # else, ratio is absolute size
                            ratio)]
            {:left 0 :top 0 :right sub-width :bottom (- (in new-rect :bottom) (in new-rect :top))})
          :vertical
          (let [sub-height (if (> 1 ratio)
                             (math/floor (* height ratio))
                             # else, ratio is absolute size
                             ratio)]
            {:left 0 :top 0 :right (- (in new-rect :right) (in new-rect :left)) :bottom sub-height})))
      (try
        (:resize new-frame resize-rect)
        ((err fib)
         (log/debug "frame-resize failed, removing inserted empty frame: %n\n%s"
                    err
                    (get-stack-trace fib))
         (:remove-child self new-frame)
         (error err))))))


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
          [width height] (rect-size (:get-viewport self))
          [parent-width parent-height] (rect-size (:get-padded-rect parent))
          cur-win (:get-current-window self)
          is-active? (= self (in parent :current-child))
          parent-constrained (:constrained? parent)]
      (if (> (length all-siblings) 2)
        (do
          (:remove-child parent self)

          (if parent-constrained
            (do
              (def calc-fn
                (cond
                  (= horizontal-frame-proto (table/getproto parent))
                  (let [rest-width (- parent-width width)]
                    (fn [sib-fr _]
                      (let [sib-width (rect-width (:get-viewport sib-fr))
                            ratio (/ sib-width rest-width)]
                        (math/floor (* ratio parent-width)))))

                  (= vertical-frame-proto (table/getproto parent))
                  (let [rest-height (- parent-height height)]
                    (fn [sib-fr _]
                      (let [sib-height (rect-height (:get-viewport sib-fr))
                            ratio (/ sib-height rest-height)]
                        (math/floor (* ratio parent-height)))))))
              (def new-rects (:calculate-sub-rects parent calc-fn))
              (map (fn [sib-fr rect]
                     (:transform sib-fr rect))
                   (in parent :children)
                   new-rects))
            # else
            (do
              (def parent-rect (in parent :rect))
              (def [parent-new-rect calc-fn]
                (cond
                  (= horizontal-frame-proto (table/getproto parent))
                  [{:left (in parent-rect :left)
                    :top (in parent-rect :top)
                    :right (- (in parent-rect :right) width)
                    :bottom (in parent-rect :bottom)}
                   (fn [sib-fr _]
                     (rect-width (:get-viewport sib-fr)))]

                  (= vertical-frame-proto (table/getproto parent))
                  [{:left (in parent-rect :left)
                    :top (in parent-rect :top)
                    :right (in parent-rect :right)
                    :bottom (- (in parent-rect :bottom) height)}
                   (fn [sib-fr _]
                     (rect-height (:get-viewport sib-fr)))]))
              (put parent :rect parent-new-rect)
              (def new-rects (:calculate-sub-rects parent calc-fn))
              (map (fn [sib-fr rect]
                     (:transform sib-fr rect))
                   (in parent :children)
                   new-rects)))

          (def cur-frame (:get-current-frame parent))
          (each child children
            (put child :parent nil)
            (:add-child cur-frame child)))

        (do
          # When the frame is closed, the parent will only have a single child.
          # Remove that child too, and move all children to the parent,
          # so that the tree stays consistent.
          (def sibling (:get-next-sibling self))
          (def [sibling-width sibling-height]
            (rect-size (:get-padded-rect sibling)))
          (def sibling-viewport (:get-viewport sibling))

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

          (def parent-viewport (:get-viewport parent))
          (if (:constrained? sibling)
            (do
              # Remove parent's viewport
              (put parent :viewport nil)
              (put parent :rect parent-viewport))
            # else
            (do
              # Copy and scale sibling's viewport
              (def parent-new-rect
                (calc-viewport-transform sibling-viewport
                                         parent-viewport
                                         (in sibling :rect)
                                         (:get-direction sibling)))
              (put parent :viewport parent-viewport) # In case parent is a constrained frame
              (put parent :rect parent-new-rect)))

          (def [parent-new-width parent-new-height]
            (rect-size (:get-padded-rect parent)))

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
                    (let [sub-width (rect-width (:get-viewport sub-fr))
                          ratio (/ sub-width sibling-width)]
                      (math/floor (* ratio parent-new-width))))

                  (= vertical-frame-proto (table/getproto parent))
                  (fn [sub-fr _]
                    (let [sub-height (rect-height (:get-viewport sub-fr))
                          ratio (/ sub-height sibling-height)]
                      (math/floor (* ratio parent-new-height))))))

              (def new-rects (:calculate-sub-rects parent calc-fn))
              (map (fn [sib-fr rect]
                     (:transform sib-fr rect))
                   (in parent :children)
                   new-rects)))))

      # If the closed frame was active, activate its current window,
      # to sync with actual focus state.
      (when (and cur-win
                 is-active?)
        # Don't use :activate method, since we don't want the
        # activation state propagate to ancestors.
        (put (in cur-win :parent) :current-child cur-win)))

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


(defn frame-get-padded-viewport [self &opt scaled]
  (default scaled true)

  (def paddings (:get-paddings self scaled))
  (shrink-rect (:get-viewport self) paddings))


(defn frame-get-direction [self]
  (def proto (table/getproto self))
  (cond
    (= proto vertical-frame-proto) :vertical
    (= proto horizontal-frame-proto) :horizontal
    # The frame is not split
    (= proto frame-proto) nil
    true (errorf "unknown frame proto: %n" proto)))


(defn change-viewport-direction [viewport rect dir]
  (def [vp-width vp-height] (rect-size viewport))
  (def [vp-center-x vp-center-y] (rect-center viewport))
  (def [width height] (rect-size rect))

  (def top-len (- vp-center-y (in rect :top)))
  (def bottom-len (- (in rect :bottom) vp-center-y))
  (def left-len (- vp-center-x (in rect :left)))
  (def right-len (- (in rect :right) vp-center-x))

  (case dir
    :horizontal # :vertical -> :horizontal
    {:left (- vp-center-x (math/floor (* top-len (/ vp-width vp-height))))
     :top (in viewport :top)
     :right (+ vp-center-x (math/floor (* bottom-len (/ vp-width vp-height))))
     :bottom (in viewport :bottom)}

    :vertical # :horizontal -> :vertical
    {:left (in viewport :left)
     :top (- vp-center-y (math/floor (* left-len (/ vp-height vp-width))))
     :right (in viewport :right)
     :bottom (+ vp-center-y (math/floor (* right-len (/ vp-height vp-width))))}

    (errorf "invalid direction: %n" dir)))


(defn frame-set-direction [self dir &opt recursive]
  (default recursive false)

  (def cur-dir (:get-direction self))
  (def children (in self :children))

  (def update-child-rect
    (fn [c r]
      (cond
        (not recursive)
        (:transform c r)

        (nil? (:get-direction c))
        # This child is not split, update the rect only
        (put c :rect r)

        true
        (do
          (:transform c r)
          (:set-direction c dir recursive)))))

  #
  # The code below asusmes that, there must be more than one
  # child when cur-dir is not nil.
  #
  (cond
    (= dir cur-dir)
    (when recursive
      # Children's rects are not updated, but we reuse
      # update-child-rect to descent recursively
      (def old-rects (map |(in $ :rect) children))
      (map update-child-rect children old-rects))

    (and (= cur-dir :vertical)
         (= dir :horizontal))
    # :vertical -> :horizontal
    (do
      (def padded-rect (:get-padded-rect self))
      (def [width height] (rect-size padded-rect))
      (def ratios (map |(/ (rect-height (:get-viewport $)) height) children))
      (table/setproto self horizontal-frame-proto)
      (unless (:constrained? self)
        (put self :rect (change-viewport-direction (in self :viewport)
                                                   (in self :rect)
                                                   :horizontal)))
      (def new-width (rect-width (:get-padded-rect self)))
      (def new-rects
        (:calculate-sub-rects
           self
           (fn [_ i]
             (math/floor (* new-width (in ratios i))))))
      (map update-child-rect children new-rects))

    (and (= cur-dir :horizontal)
         (= dir :vertical))
    # :horizontal -> :vertical
    (do
      (def padded-rect (:get-padded-rect self))
      (def [width height] (rect-size padded-rect))
      (def ratios (map |(/ (rect-width (:get-viewport $)) width) children))
      (table/setproto self vertical-frame-proto)
      (unless (:constrained? self)
        (put self :rect (change-viewport-direction (in self :viewport)
                                                   (in self :rect)
                                                   :vertical)))
      (def new-height (rect-height (:get-padded-rect self)))
      (def new-rects
        (:calculate-sub-rects
           self
           (fn [_ i]
             (math/floor (* new-height (in ratios i))))))
      (map update-child-rect children new-rects))

    true
    (errorf "can not change direction from %n to %n" cur-dir dir)))


(defn frame-toggle-direction [self &opt recursive]
  (default recursive false)

  (def cur-dir (:get-direction self))

  (def set-dir
    (fn [dir]
      (:set-direction self dir)
      (when recursive
        (each c (in self :children)
          (when (:get-direction c)
            (:toggle-direction c recursive))))))

  (case cur-dir
    nil
    (error "leaf frames have no direction")

    :horizontal
    (set-dir :vertical)

    :vertical
    (set-dir :horizontal)

    (errorf "unknown direction: %n" cur-dir)))


(defn frame-rotate-children [self direction]
  (def children (in self :children))

  (cond
    (empty? children)
    :nop

    (>= 1 (length children))
    :nop

    (= :window (get-in self [:children 0 :type]))
    # XXX: Currently this has no visible effect
    (rotate-array! children direction)

    (= :frame (get-in self [:children 0 :type]))
    (do
      (rotate-array! children direction)
      # refresh children's rects
      (:transform self (:get-viewport self)))

    (error "inconsistent states for frame tree")))


(defn frame-reverse-children [self]
  (def children (in self :children))

  (cond
    (empty? children)
    :nop

    (>= 1 (length children))
    :nop

    (= :window (get-in self [:children 0 :type]))
    # XXX: Currently this has no visible effect
    (reverse! children)

    (= :frame (get-in self [:children 0 :type]))
    (do
      (reverse! children)
      # refresh children's rects
      (:transform self (:get-viewport self)))

    (error "inconsistent states for frame tree")))


(defn frame-dump [self]
  [:frame
   (in self :rect)
   (in self :viewport)
   (dump-tags (in self :tags))
   (tuple/slice (map |(:dump $) (in self :children)))])


(defn frame-load [self dumped &opt hwnd-list]
  (def [dump-type rect viewport tags children] dumped)
  (unless (= :frame dump-type)
    (errorf "can not restore dump type to a frame: %n" dump-type))

  # XXX: hwnd-list is also re-used in recursion to pass down hwnd-map,
  # so it can be a table, instead of a list/array.
  (default hwnd-list (enum-all-hwnds))
  (def hwnd-map (hwnd-list-to-map hwnd-list))

  (:clear-children self)
  (:remove-viewport self)

  # This has to happen before doing anything layout-related,
  # since the tags may contain layout settings (paddings etc.)
  (table/clear (in self :tags))
  (eachp [k v] tags
    (put (in self :tags) k v))

  (if-let [split-params (calc-loading-split-params children)]
    (do
      # The frame is not empty, and the children are sub-frames
      (def [direction ratios] split-params)
      (when viewport
        (def cur-viewport (:get-viewport self))
        (def new-rect
          (calc-viewport-transform viewport
                                   cur-viewport
                                   rect
                                   direction))
        (put self :viewport cur-viewport)
        (put self :rect new-rect))
      (:split self direction (length children) ratios)
      (map (fn [sub-fr d]
             (:load sub-fr d hwnd-map))
           (in self :children)
           children))

    # else
    (unless (empty? children)
      # The frame is not empty, and the children are windows
      (def lo (:get-layout self))
      (def wm (when lo (:get-window-manager lo)))

      (def maybe-restore
        (if lo
          # attached frame, need to check its virtual desktop
          (fn [hwnd-num hwnd c]
            (when-let [vd-id (:get-hwnd-virtual-desktop-id wm hwnd)]
              (when (= vd-id (in lo :id))
                (put hwnd-map hwnd-num nil)
                (def win (window hwnd))
                (:load win c)
                (:add-child self win))))
          # else, detached frame
          (fn [hwnd-num hwnd c]
            (put hwnd-map hwnd-num nil)
            (def win (window hwnd))
            (:load win c)
            (:add-child self win))))

      (each c children
        (def [_ hwnd-num _] c)
        (when-let [hwnd (in hwnd-map hwnd-num)]
          (maybe-restore hwnd-num hwnd c)))))

  hwnd-map)


(set frame-proto
     (table/setproto
      @{:get-viewport frame-get-viewport
        :set-viewport frame-set-viewport
        :remove-viewport frame-remove-viewport
        :constrained? frame-constrained?
        :move-into-viewport frame-move-into-viewport
        :split frame-split
        :balance frame-balance
        :flatten frame-flatten
        :transform frame-transform
        :resize frame-resize
        :insert-sub-frame frame-insert-sub-frame
        :close frame-close
        :sync-current-window frame-sync-current-window
        :get-paddings frame-get-paddings
        :get-padded-rect frame-get-padded-rect
        :get-padded-viewport frame-get-padded-viewport
        :get-direction frame-get-direction
        :set-direction frame-set-direction
        :toggle-direction frame-toggle-direction
        :rotate-children frame-rotate-children
        :reverse-children frame-reverse-children
        :dump frame-dump
        :load frame-load}
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


(defn layout-update-work-areas [self monitors &opt wm]
  (default wm (:get-window-manager self))

  (def top-frames (in self :children))
  (def fr-count (length top-frames))
  (def mon-count (length monitors))

  (cond
    (= fr-count mon-count)
    # Only the resolutions or monitor configurations are changed
    (do
      (var updated false)
      (map (fn [fr mon]
             (unless (= mon (in fr :monitor))
               (:transform fr (in mon :work-area))
               (put fr :monitor mon)
               (:monitor-updated wm fr)
               (set updated true)))
           top-frames
           monitors)
      (when updated
        # Trigger :layout-changed hook
        (:layouts-changed wm [self])))

    (> fr-count mon-count)
    # Some of the monitors got unplugged
    (let [alive-frames (slice top-frames 0 mon-count)
          dead-frames (slice top-frames mon-count)
          orphan-windows @[]]
      (var main-fr (first alive-frames))
      (map (fn [fr mon]
             (unless (= mon (in fr :monitor))
               (:transform fr (in mon :work-area))
               (put fr :monitor mon)
               (:monitor-updated wm fr))
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
        (put self :current-child main-fr))
      # Trigger :layout-changed hook
      (:layouts-changed wm [self]))

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
               (:transform fr (in mon :work-area))
               (put fr :monitor mon)
               (:monitor-updated wm fr)))
           top-frames
           old-mons)
      (each fr new-frames
        (:add-child self fr)
        (:monitor-updated wm fr))
      # Trigger :layout-changed hook
      (:layouts-changed wm [self]))))


(defn layout-rotate-children [self direction]
  (def children (in self :children))
  (def monitors (map |(in $ :monitor) children))
  (rotate-array! children direction)
  (:update-work-areas self monitors))


(defn layout-reverse-children [self]
  (def children (in self :children))
  (def monitors (map |(in $ :monitor) children))
  (reverse! children)
  (:update-work-areas self monitors))


(defn layout-dump [self]
  [:layout
   (in self :id)
   (in self :name)
   (tuple/slice (map |(:dump $) (in self :children)))])


(defn calc-rect-dist [a b]
  (def [cxa cya] (rect-center a))
  (def [cxb cyb] (rect-center b))
  (def dx (- cxa cxb))
  (def dy (- cya cyb))
  (+ (* dx dx) (* dy dy)))


# rects-a (jobs/cols) should be from top frames, and rects-b
# (workers/rows) should be from dumped data
(defn calc-rect-dist-matrix [rects-a rects-b]
  (def dist @[])
  (eachp [idx-a ra] rects-a
    (def col @[])
    (array/push dist col)
    (eachp [idx-b rb] rects-b
      (array/push col (calc-rect-dist ra rb))))
  dist)


(defn layout-load [self dumped &opt hwnd-list]
  (def [dump-type lo-id lo-name children] dumped)
  (unless (= :layout dump-type)
    (errorf "can not restore dump type to a layout: %n" dump-type))

  (default hwnd-list (enum-all-hwnds))
  (def hwnd-map (hwnd-list-to-map hwnd-list))

  (def all-top-frames (array/slice (in self :children)))

  (def frame-rects (map |(:get-viewport $) all-top-frames))
  (def dumped-rects (map |(get-dumped-viewport $) children))

  (if (<= (length frame-rects)
          (length dumped-rects))
    (do
      (def dist-matrix (calc-rect-dist-matrix frame-rects dumped-rects))
      (def [total-dist dumped-to-frame] (hungarian-assignment dist-matrix))

      (log/debug "dist-matrix = %n" dist-matrix)
      (log/debug "total-dist = %n" total-dist)
      (log/debug "dumped-to-frame = %n" dumped-to-frame)

      (eachp [dump-idx fr-idx] dumped-to-frame
        (when (<= 0 fr-idx)
          (:load (in all-top-frames fr-idx) (in children dump-idx) hwnd-map))))

    # else
    (do
      (def dist-matrix (calc-rect-dist-matrix dumped-rects frame-rects))
      (def [total-dist frame-to-dumped] (hungarian-assignment dist-matrix))

      (log/debug "dist-matrix = %n" dist-matrix)
      (log/debug "total-dist = %n" total-dist)
      (log/debug "frame-to-dumped = %n" frame-to-dumped)

      (eachp [fr-idx dump-idx] frame-to-dumped
        (when (<= 0 dump-idx)
          (:load (in all-top-frames fr-idx) (in children dump-idx) hwnd-map)))))

  hwnd-map)


(def- layout-proto
  (table/setproto
   @{:update-work-areas layout-update-work-areas
     :rotate-children layout-rotate-children
     :reverse-children layout-reverse-children
     :dump layout-dump
     :load layout-load}
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

  (def lo (:get-layout-on-desktop self desktop-info))
  (:get-current-frame lo))


(defn vdc-get-layout-on-desktop [self desktop-info]
  (def {:id desktop-id
        :name desktop-name}
    desktop-info)

  (var layout-found nil)
  (each lo (in self :children)
    (when (= (in lo :id) desktop-id)
      (set layout-found lo)
      (break)))
  (if layout-found
    layout-found
    (let [new-layout (:new-layout self desktop-info)]
      (:add-child self new-layout)
      (:layout-created (in self :window-manager) new-layout)
      new-layout)))


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
  new-layout)


(defn vdc-purge-windows [self &opt pred dead-arr]
  (def wm (in self :window-manager))
  (each lo (in self :children)
    (if pred
      (:purge-windows lo |(pred $ lo) dead-arr)
      (:purge-windows lo |(window-purge-pred $ wm lo) dead-arr)))
  (when dead-arr
    (log/debug "Purged %n dead windows" (length dead-arr)))
  dead-arr)


(defn vdc-dump [self]
  [:vdc
   (tuple/slice (map |(:dump $) (in self :children)))])


(defn vdc-load [self dumped &opt hwnd-list]
  (def [dump-type children] dumped)
  (unless (= :vdc dump-type)
    (errorf "can not restore dump type to a virtual desktop container: %n" dump-type))

  (default hwnd-list (enum-all-hwnds))
  (def hwnd-map (hwnd-list-to-map hwnd-list))

  (:clear-children self)

  (each c children
    (def [_ lo-id lo-name _] c)
    (def vd-info {:id lo-id :name lo-name})
    (def lo (:new-layout self vd-info))
    (:add-child self lo)
    (log/debug "restoring layout, id = %n" lo-id)
    (:load lo c hwnd-map))

  hwnd-map)


(def- virtual-desktop-container-proto
  (table/setproto
   @{:new-layout vdc-new-layout
     :get-layout-on-desktop vdc-get-layout-on-desktop
     :get-current-frame-on-desktop vdc-get-current-frame-on-desktop
     :purge-windows vdc-purge-windows
     :dump vdc-dump
     :load vdc-load}
   tree-node-proto))


(defn virtual-desktop-container [wm &opt children]
  (default children @[])
  (def vdc-obj (tree-node :virtual-desktop-container nil children
                          :window-manager wm))
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


(defn wm-own-process-elevated? [self]
  (with [[ret token]
         (OpenProcessToken (GetCurrentProcess) TOKEN_QUERY)
         (fn [[_ token]] (CloseHandle token))]
    (when (= 0 ret)
      (log/warning "failed to open own process token: 0x%x" (GetLastError))
      (break false))
    (def [gti-ret elevated] (GetTokenInformation token TokenElevation))
    (when (= 0 gti-ret)
      (log/warning "GetTokenInformation for TokenElevation failed: 0x%x" (GetLastError)))
    elevated))


(defn wm-has-uiaccess? [self]
  (with [[ret token]
         (OpenProcessToken (GetCurrentProcess) TOKEN_QUERY)
         (fn [[_ token]] (CloseHandle token))]
    (when (= 0 ret)
      (log/warning "failed to open own process token: 0x%x" (GetLastError))
      (break false))
    (def [gti-ret ua] (GetTokenInformation token TokenUIAccess))
    (when (= 0 gti-ret)
      (log/warning "GetTokenInformation for TokenUIAccess failed: 0x%x" (GetLastError)))
    ua))


(defn wm-get-focused-hwnd [self &opt top-level?]
  (default top-level? true)

  (with-uia [uia-win (:get-focused-window (in self :uia-manager) top-level?)]
    (when uia-win
      (when-let [hwnd (:get_CachedNativeWindowHandle uia-win)]
        (if (null? hwnd)
          nil
          hwnd)))))


(defn wm-get-hwnd-path [self hwnd]
  (get-hwnd-path hwnd))


(defn wm-get-hwnd-uia-element [self hwnd &opt cr]
  (default cr (get-in self [:uia-manager :focus-cr]))
  (def uia-man (in self :uia-manager))
  (get-hwnd-uia-element hwnd (in uia-man :com) cr))


(defn wm-get-hwnd-virtual-desktop-id [self hwnd]
  (get-hwnd-virtual-desktop-id hwnd (in self :vd-manager)))


(defn wm-get-hwnd-virtual-desktop [self hwnd? &opt uia-win?]
  (get-hwnd-virtual-desktop hwnd?
                            (in self :uia-manager)
                            (in self :vd-manager)
                            uia-win?))


(defn wm-get-hwnd-info [self hwnd? &opt uia-win?]
  (get-hwnd-info hwnd? (in self :uia-manager) (in self :vd-manager) uia-win?))


(defn wm-get-hwnd-rect [self hwnd &opt no-frame?]
  (get-hwnd-rect hwnd no-frame?))


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


(defn wm-add-hwnd [self hwnd-or-info &opt manage-state no-check]
  (default manage-state :normal)
  (default no-check false)

  (with-uia [hwnd-info (if (= :pointer (type hwnd-or-info))
                         # It's an hwnd
                         (:get-hwnd-info self hwnd-or-info)
                         # else, assume it's an hwnd-info object
                         (do
                           (:AddRef hwnd-or-info)
                           hwnd-or-info))]
    (when hwnd-info
      (def {:hwnd hwnd
            :uia-element uia-win
            :exe-path exe-path
            :virtual-desktop desktop-info}
        hwnd-info)

      (unless no-check
        (when-let [w (:find-hwnd (in self :root) hwnd)]
          # out of with-uia
          (break w)))

      (log/debug "new window: %n" hwnd)

      (def new-win (window hwnd))
      (when (= :forced manage-state)
        (put (in new-win :tags) :forced true))

      (:call-hook (in self :hook-manager) :window-created
         new-win uia-win exe-path desktop-info)

      (def tags (in new-win :tags))

      (def frame-found
        (if-let [override-frame (in tags :frame)]
          (do 
            (put tags :frame nil) # Clear the tag, in case the frame got invalidated later
            (:get-current-frame override-frame))
          (:get-current-frame-on-desktop (in self :root) desktop-info)))

      (:add-child frame-found new-win)
      (:layouts-changed self [(:get-layout frame-found)])
      (:transform new-win (:get-padded-rect frame-found) nil self)

      new-win)))


(defn wm-remove-hwnd [self hwnd]
  (when-let [w (:find-hwnd (in self :root) hwnd)]
    (def parent-fr (in w :parent))
    (:remove-child parent-fr w)
    (:call-hook (in self :hook-manager) :window-removed w)
    w))


(defn wm-find-hwnd [self hwnd]
  (:find-hwnd (in self :root) hwnd))


(defn wm-filter-hwnd [self hwnd &opt uia-win? _exe-path? desktop-info?]
  (def uia-win
    (if uia-win?
      (do
        (:AddRef uia-win?)
        uia-win?)
      (:get-hwnd-uia-element self hwnd (get-in self [:uia-manager :focus-cr]))))
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

      (and (not (:own-process-elevated? self))
           (not (:has-uiaccess? self))
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


(defn wm-do-not-ignore-hwnd [self hwnd]
  (put (in self :ignored-hwnds) hwnd nil))


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
  (let [dead (:purge-windows (in self :root) nil @[])]
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

    (when (null? hwnd)
      # (:get-focused-window uia-man) returned a bad window
      # XXX: I don't really know what it is, more logs for postmortem.
      (log/debug "NULL HWND: name=%n, class=%n, controltype=%n"
                 (try
                   (:get_CachedName uia-win)
                   ((err _fib)
                    (string/format "<error %n>" err)))
                 (try
                   (:get_CachedClassName uia-win)
                   ((err _fib)
                    (string/format "<error %n>" err)))
                 (try
                   (:GetCachedPropertyValue uia-win UIA_ControlTypePropertyId)
                   ((err _fib)
                    (string/format "<error %n>" err))))
      (break))

    (when-let [win (:find-hwnd (in self :root) hwnd)]
      # Already managed
      (with-activation-hooks self
        (:activate win))
      (break))

    (with-uia [hwnd-info (get-hwnd-info hwnd
                                        (in self :uia-manager)
                                        (in self :vd-manager)
                                        uia-win)]
      (when hwnd-info
        (def manage-state (:should-manage-hwnd? self hwnd-info))
        (log/debug "manage-state = %n" manage-state)

        (when (= :ignored manage-state)
          (log/debug "Ignoring window: %n" hwnd)
          # out of with-uia
          (break))

        (if-let [new-win (:add-hwnd self hwnd-info manage-state true)]
          (with-activation-hooks self
            (:activate new-win)))))))


(defn wm-window-opened [self hwnd]
  (when-let [win (:find-hwnd (in self :root) hwnd)]
    (log/debug "window-opened event for managed window: %n" hwnd)
    (break))

  (with-uia [hwnd-info (get-hwnd-info hwnd
                                      (in self :uia-manager)
                                      (in self :vd-manager))]
    (when hwnd-info
      (def manage-state (:should-manage-hwnd? self hwnd-info))
      (log/debug "manage-state = %n" manage-state)

      (when (= :ignored manage-state)
        (log/debug "Ignoring window: %n" hwnd)
        # out of with-uia
        (break))

      (if-let [new-win (:add-hwnd self hwnd-info manage-state true)
               fg-hwnd (GetForegroundWindow)]
        # Some windows only send one window-opened event, but no focus-changed
        # event, when they firt appear, even though they have input focus
        # (e.g. Windows Terminal). We need to explicitly activate their nodes
        # here if they're opened in the foreground.
        (when (= hwnd fg-hwnd)
          (with-activation-hooks self
            (:activate new-win)))))))


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
        (:set-focus self lo)))))


(defn wm-layouts-changed [self lo-list]
  (def {:hook-manager hook-man} self)
  (each lo lo-list
    (:call-hook hook-man :layout-changed lo)))


(defn wm-frames-resized [self frame-list &opt trigger-layout-changed]
  (default trigger-layout-changed true)

  (def hook-man (in self :hook-manager))
  (def changed-layouts @{})

  (each fr frame-list
    (when trigger-layout-changed
      (when-let [lo (:get-layout fr)]
        (put changed-layouts (in lo :id) lo)))

    (def children (in fr :children))

    # Only call hooks on leaf frames
    (cond
      (empty? children)
      (:call-hook hook-man :frame-resized fr)

      (= :window (get-in fr [:children 0 :type]))
      (:call-hook hook-man :frame-resized fr)

      (= :frame (get-in fr [:children 0 :type]))
      (:frames-resized self (in fr :children) false)))

  (unless (empty? changed-layouts)
    (:layouts-changed self (values changed-layouts))))


(defn wm-monitor-updated [self top-frame]
  (:call-hook (in self :hook-manager) :monitor-updated top-frame))


(defn wm-layout-created [self new-layout]
  (def hook-man (in self :hook-manager))
  (:call-hook hook-man :layout-created new-layout)
  (each fr (in new-layout :children)
    (:monitor-updated self fr)))


(defn wm-set-focus [self node]
  (cond
    (nil? node)
    (:set-focus-to-desktop self)

    (= :window (in node :type))
    (:set-focus node self)

    (or (= :frame (in node :type))
        (= :layout (in node :type)))
    (if-let [cur-win (:get-current-window node)]
      (:set-focus cur-win self)
      (do
        (:activate node)
        (:set-focus-to-desktop self)))))


(defn wm-set-focus-to-hwnd [self hwnd]
  (:set-focus-to-window (in self :uia-manager) hwnd))


(defn wm-set-focus-to-desktop [self]
  (:set-focus-to-desktop (in self :uia-manager)))


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
        (:transform w (:get-padded-rect fr) nil self))

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
  (def old-win
    (when old-frame
      (:get-current-window old-frame)))

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
  :nop)


(def- window-manager-proto
  @{:focus-changed wm-focus-changed
    :window-opened wm-window-opened
    :desktop-name-changed wm-desktop-name-changed
    :layouts-changed wm-layouts-changed
    :frames-resized wm-frames-resized
    :monitor-updated wm-monitor-updated
    :layout-created wm-layout-created

    :transform-hwnd wm-transform-hwnd
    :reset-hwnd-visual-state wm-reset-hwnd-visual-state
    :retile wm-retile
    :set-focus wm-set-focus
    :set-focus-to-hwnd wm-set-focus-to-hwnd
    :set-focus-to-desktop wm-set-focus-to-desktop

    :get-focused-hwnd wm-get-focused-hwnd
    :get-hwnd-path wm-get-hwnd-path
    :get-hwnd-virtual-desktop-id wm-get-hwnd-virtual-desktop-id
    :get-hwnd-virtual-desktop wm-get-hwnd-virtual-desktop
    :get-hwnd-uia-element wm-get-hwnd-uia-element
    :get-hwnd-info wm-get-hwnd-info
    :get-hwnd-rect wm-get-hwnd-rect
    :hwnd-alive? wm-hwnd-alive?
    :hwnd-process-elevated? wm-hwnd-process-elevated?

    :should-manage-hwnd? wm-should-manage-hwnd?
    :add-hwnd wm-add-hwnd
    :remove-hwnd wm-remove-hwnd
    :find-hwnd wm-find-hwnd
    :filter-hwnd wm-filter-hwnd
    :ignore-hwnd wm-ignore-hwnd
    :do-not-ignore-hwnd wm-do-not-ignore-hwnd
    :clean-up-hwnds wm-clean-up-hwnds

    :close-hwnd wm-close-hwnd

    :enumerate-monitors wm-enumerate-monitors
    :own-process-elevated? wm-own-process-elevated?
    :has-uiaccess? wm-has-uiaccess?

    :with-activation-hooks wm-with-activation-hooks

    :destroy wm-destroy})


(defn window-manager [uia-man ui-man hook-man vd-man]
  (def wm-obj
    (table/setproto
     @{:vd-manager vd-man
       :uia-manager uia-man
       :ui-man ui-man
       :hook-manager hook-man
       :ignored-hwnds @{}
       :last-vd-name (with-uia [root (:get-root uia-man)]
                       (:get_CurrentName root))}
     window-manager-proto))
  (put wm-obj :root (virtual-desktop-container wm-obj))

  # Some windows sometimes don't have their virtual desktop info ready
  # when they're open. This hook function is to check for these windows,
  # and re-schedule a :window-opened event, so that they can be managed
  # properly when ready.
  #
  # Currently only Windows Terminal exhibits this behavior.
  (:add-hook hook-man :filter-forced-window
     (fn slow-vd-fix [hwnd uia-win exe-path desktop-info]
       (when (nil? (in desktop-info :id))
         (when (and (= "CASCADIA_HOSTING_WINDOW_CLASS" (:get_CachedClassName uia-win))
                    (string/has-suffix? "\\WindowsTerminal.exe" exe-path))
           (log/debug "Virtual desktop info not yet initialized for window %n, scheduling retry..." hwnd)
           (ev/spawn
            # XXX: Arbitrary value
            (ev/sleep 0.2)
            (:window-opened wm-obj hwnd))))
       false))

  (:add-hook hook-man :filter-window
     (fn default-window-filter [hwnd uia-win exe-path desktop-info]
       (match (:filter-hwnd wm-obj hwnd uia-win exe-path desktop-info)
         [false reason]
         (do
           (log/debug "Window %n failed to pass default filter: %n" hwnd reason)
           false)

         true
         true)))

  (:add-hook hook-man :frame-activated
     (fn default-frame-activated-action [fr]
       (:update-work-area ui-man (in (:get-top-frame fr) :rect))
       # "Scroll" unconstrained frames. Should mostly be nop for
       # constrained frames
       (when-let [to-retile (:move-into-viewport fr)]
         (:retile wm-obj to-retile))))

  # This hook is needed for commands like :rotate-sibling-frames,
  # when rotating top-level frames, to update the current active
  # work area, since the activation hooks won't fire in that case.
  (:add-hook hook-man :frame-resized
     (fn default-frame-resized-action [fr]
       (def cur-fr (:get-current-frame (in wm-obj :root)))
       (when (= fr cur-fr)
         (:update-work-area ui-man (in (:get-top-frame fr) :rect)))))

  wm-obj)
