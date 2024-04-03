(use jw32/winuser)
(use jw32/processthreadsapi)
(use jw32/securitybaseapi)
(use jw32/combaseapi)
(use jw32/winnt)
(use jw32/handleapi)
(use jw32/uiautomation)
(use jw32/util)

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


(defn tree-node-get-next-sibling [self]
  (cond
    (nil? (in self :parent))
    nil

    true
    (let [all-siblings (get-in self [:parent :children])
          sibling-count (length all-siblings)]
      (if-let [idx (find-index |(= $ self) all-siblings)]
        (let [next-idx (% (+ idx 1) sibling-count)]
          (in all-siblings next-idx))
        (error "inconsistent states for frame tree")))))


(defn tree-node-get-prev-sibling [self]
  (cond
    (nil? (in self :parent))
    nil

    true
    (let [all-siblings (get-in self [:parent :children])
          sibling-count (length all-siblings)]
      (if-let [idx (find-index |(= $ self) all-siblings)]
        (let [prev-idx (% (+ sibling-count (- idx 1)) sibling-count)]
          (in all-siblings prev-idx))
        (error "inconsistent states for frame tree")))))


(def- tree-node-proto
  @{:activate tree-node-activate
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
  (not= FALSE (IsWindow (in self :hwnd))))


(def- window-proto
  (table/setproto
   @{:alive? window-alive?}
   tree-node-proto))


(defn window [hwnd &opt parent]
  (let [node (tree-node parent nil
                        :type :window
                        :hwnd hwnd)]
    (table/setproto node window-proto)))


######### Frame object #########

# Forward declaration of the frame constructor
(var frame nil)


(defn frame-add-child [self child]
  (let [children (in self :children)
        old-parent (in child :parent)]
    (cond
      (empty? children)
      (do
        (put child :parent self)
        (array/push children child))

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
      (put old-parent :children
         (filter |(not= $ child) (in old-parent :children)))))
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
        width (- right left)
        new-frames @[]]

    (var current-top top)
    (var current-left left)

    (case direction
      :horizontal
      (for i 0 n
        (def current-width
          (if (>= i (length full-ratios)) # Is this the last sub-frame?
            (- (+ width left) current-left)
            (math/floor (* width (in full-ratios i)))))
        (if (<= current-width 0)
          (error "cannot create zero-width frames"))
        (array/push new-frames
                    (frame {:top current-top
                            :left current-left
                            :right (+ current-left current-width)
                            :bottom bottom}
                           self))
        (+= current-left current-width))

      :vertical
      (for i 0 n
        (def current-height
          (if (>= i (length full-ratios)) # Is this the last sub-frame?
            (- (+ height top) current-top)
            (math/floor (* height (in full-ratios i)))))
        (if (<= current-height 0)
          (error "cannot create zero-height frames"))
        (array/push new-frames
                    (frame {:top current-top
                            :left current-left
                            :right right
                            :bottom (+ current-top current-height)}
                           self))
        (+= current-top current-height)))

    (def old-children (in self :children))
    (def old-active-child (in self :current-child))
    (put self :children new-frames)
    # The caller should decide which sub-frame to activate after the split
    (put self :current-child nil)
    (def first-sub-frame (in new-frames 0))
    # XXX: Move all window children to the first sub-frame
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


(defn frame-flatten [self]
  (def all-windows (frame-get-all-windows self))
  (each w all-windows
    (put w :parent self))
  (put self :children all-windows)
  # The caller should decide which sub-frame to activate after the flattening
  (put self :current-child nil)
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
            (put self :current-child (in alive 0))))

        true # There are children, but none of them is active
        (error "inconsistent states for frame tree"))
      dead)))


(defn frame-get-current-window [self]
  (var parent self)
  (var cur-child (in parent :current-child))
  (while (and cur-child
              (not= :window (in cur-child :type)))
    (set parent cur-child)
    (set cur-child (in parent :current-child)))
  cur-child)


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


(def- frame-proto
  (table/setproto
   @{:add-child frame-add-child
     :split frame-split
     :flatten frame-flatten
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


(defn layout-get-next-frame [self node]
  (let [all-siblings (get-in node [:parent :children])
        sibling-count (length all-siblings)
        fr-idx (if-let [idx (find-index |(= $ node) all-siblings)]
                 idx
                 (error "inconsistent states for frame tree"))
        next-idx (if (= self (in node :parent)) # Toplevel frames wrap around
                   (% (+ fr-idx 1) sibling-count)
                   (+ fr-idx 1))]
    (cond
      (>= next-idx sibling-count)
      # We reached the end of the sub-frame list, go up
      (layout-get-next-frame self (in node :parent))

      true
      # there's the next sibling, go down
      (:get-first-frame (in all-siblings next-idx)))))


(defn layout-get-prev-frame [self node]
  (let [all-siblings (get-in node [:parent :children])
        sibling-count (length all-siblings)
        fr-idx (if-let [idx (find-index |(= $ node) all-siblings)]
                 idx
                 (error "inconsistent states for frame tree"))
        prev-idx (if (= self (in node :parent)) # Toplevel frames wrap around
                   (% (+ fr-idx sibling-count -1) sibling-count)
                   (- fr-idx 1))]
    (cond
      (< prev-idx 0)
      # We reached the beginning of the sub-frame list, go up
      (layout-get-prev-frame self (in node :parent))

      true
      # there's the previous sibling, go down
      (:get-last-frame (in all-siblings prev-idx)))))


(def layout-proto
  (table/setproto
   @{:split (fn [&] (error "unsupported operation"))
     :flatten (fn [&] (error "unsupported operation"))
     :get-next-frame layout-get-next-frame
     :get-prev-frame layout-get-prev-frame}
   frame-proto))


(defn layout [&opt children]
  (def layout-obj (frame nil nil children))
  (put layout-obj :type :layout)
  (table/setproto layout-obj layout-proto))


######### Window manager object #########

(defn wm-transform-window [self win fr]
  (let [uia (get-in self [:uia-context :uia])
        hwnd (in win :hwnd)
        rect (in fr :rect)]
    (log/debug "transforming window: %n, rect = %n" hwnd rect)
    (try
      (with [uia-win (:ElementFromHandle uia hwnd) (in uia-win :Release)]
        (with [pat
               (:GetCurrentPatternAs uia-win UIA_TransformPatternId IUIAutomationTransformPattern)
               (fn [pat] (when pat (:Release pat)))]
          # TODO: restore maximized windows first
          (when pat
            (:Move pat (in rect :left) (in rect :top))
            (:Resize pat
                     (- (in rect :right) (in rect :left))
                     (- (in rect :bottom) (in rect :top))))))
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


(defn wm-should-manage? [self hwnd]
  (let [uia-context (in self :uia-context)
        uia (in uia-context :uia)
        cr (in uia-context :focus-cr)]
    (with [uia-win
           (try
             (:ElementFromHandleBuildCache uia hwnd cr)
             ((err fib)
              # The window may have vanished
              (log/debug "ElementFromHandleBuildCache failed: %n" err)
              nil))
           (fn [uia-win] (when uia-win (:Release uia-win)))]
      (if uia-win
        (and (not= 0 (:GetCachedPropertyValue uia-win UIA_IsTransformPatternAvailablePropertyId))
             (not= 0 (:GetCachedPropertyValue uia-win UIA_IsWindowPatternAvailablePropertyId))
             (or (wm-is-jwno-process-elevated? self)
                 (not (wm-is-window-process-elevated? self hwnd))))
        false))))


(defn wm-add-window [self hwnd]
  (log/debug "new window: %n" hwnd)
  (when (not (wm-should-manage? self hwnd))
    (log/debug "Ignoring window: %n" hwnd)
    (break nil))

  (def new-win (window hwnd))
  (def frame-found (:find-frame-for-window (in self :layout) new-win))
  (wm-transform-window self new-win frame-found)
  (:add-child frame-found new-win)
  new-win)


(defn wm-focus-changed [self]
  # XXX: If the focus change is caused by a closing window, that
  # window may still be alive, so it won't be purged immediately.
  # Maybe I shoud check the hwnds everytime a window is manipulated?
  (def dead (:purge-windows (in self :layout)))
  (log/debug "purged %n dead windows" (length dead))

  (def hwnd
    (let [uia-context (in self :uia-context)]
      (with [uia-win
             (uia/get-focused-window uia-context)
             (fn [uia-win] (when uia-win (:Release uia-win)))]
        (when uia-win
          (:get_CachedNativeWindowHandle uia-win)))))

  (when (nil? hwnd)
    (log/debug "No focused window")
    (break self))

  (when-let [win (:find-window (in self :layout) hwnd)]
    # Already managed
    (:activate win)
    (break self))

  # TODO: window open/close events
  (when-let [new-win (wm-add-window self hwnd)]
    (:activate new-win))
  self)


(defn wm-window-opened [self hwnd]
  (when-let [win (:find-window (in self :layout) hwnd)]
    (log/debug "window-opened event for managed window: %n" hwnd)
    (break self))

  # TODO: window open events
  (when-let [new-win (wm-add-window self hwnd)]
    (:activate new-win))
  self)


(defn wm-activate [self node]
  (when node
    (:activate node))

  (def hwnd
    (cond
      (nil? node)
      (:get_CachedNativeWindowHandle (get-in self [:uia-context :root]))

      (= :window (in node :type))
      (in node :hwnd)

      (= :frame (in node :type))
      (if-let [cur-win (:get-current-window node)]
        (in cur-win :hwnd)
        (:get_CachedNativeWindowHandle (get-in self [:uia-context :root])))))

  (log/debug "setting foreground window to %n" hwnd)
  (def sfw-ret (SetForegroundWindow hwnd))
  (when (= FALSE sfw-ret)
    (log/debug "SetForegroundWindow failed")))


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
    :retile wm-retile
    :activate wm-activate
    :is-window-process-elevated? wm-is-window-process-elevated?
    :is-jwno-process-elevated? wm-is-jwno-process-elevated?})


(defn window-manager [uia-context]
  # Need this for SetForegroundWindow() to actually bring
  # the windows to the foreground.
  (when (= FALSE (SystemParametersInfo SPI_SETFOREGROUNDLOCKTIMEOUT 0 0 0))
    (error "SPI_SETFOREGROUNDLOCKTIMEOUT failed"))

  (def wm-obj
    (table/setproto
     @{:uia-context uia-context}
     wm-proto))

  (def [work-areas main-idx] (:enumerate-monitors wm-obj))
  (put wm-obj :layout
     (layout (map |(frame $) work-areas)))
  (if main-idx
    (:activate (get-in wm-obj [:layout :children main-idx]))
    (:activate (get-in wm-obj [:layout :children 0])))

  wm-obj)
