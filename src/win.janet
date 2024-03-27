(use jw32/winuser)
(use jw32/combaseapi)
(use jw32/uiautomation)
(use jw32/util)

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


(def- tree-node-proto
  @{:activate tree-node-activate})


(defn tree-node [parent children &keys extra-fields]
  (let [node (table/setproto
              @{:parent parent
                :children children}
              tree-node-proto)]
    (eachp [k v] extra-fields
      (put node k v))
    node))


######### Window object #########

(defn window-transform [self rect]
  (log/debug "transforming window: %n, rect = %n" (in self :hwnd) rect)
  (let [uia (get-in self [:wm :uia-context :uia])]
    (with [uia-win
           (:ElementFromHandle uia (in self :hwnd))
           (in uia-win :Release)]
      (with [pat
             (:GetCurrentPatternAs uia-win UIA_TransformPatternId IUIAutomationTransformPattern)
             (fn [pat] (when pat (:Release pat)))]
        # TODO: restore maximized windows first
        (when pat
          (:Move pat (in rect :left) (in rect :top))
          (:Resize pat
                   (- (in rect :right) (in rect :left))
                   (- (in rect :bottom) (in rect :top)))))))
  self)


(defn window-alive? [self]
  (not= FALSE (IsWindow (in self :hwnd))))


(def- window-proto
  (table/setproto
   @{:transform window-transform
     :alive? window-alive?}
   tree-node-proto))


(defn window [hwnd wm &opt parent]
  (let [node (tree-node parent nil
                        :type :window
                        :hwnd hwnd
                        :wm wm)]
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


(def- frame-proto
  (table/setproto
   @{:add-child frame-add-child
     :split frame-split
     :find-window frame-find-window
     :find-frame-for-window frame-find-frame-for-window
     :purge-windows frame-purge-windows}
   tree-node-proto))


(varfn frame [rect &opt parent children]
  (default children @[])
  (let [node (tree-node parent children
                        :type :frame
                        :rect rect
                        :current-child (if-not (empty? children)
                                         (in children 0)))]
    (table/setproto node frame-proto)))


######### Window manager object #########

(defn wm-purge-windows [self]
  (def dead @[])
  (each f (get-in self [:frame-tree :toplevels])
    (array/push dead ;(:purge-windows f)))
  (log/debug "purged %n dead windows" (length dead))
  dead)


(defn wm-find-window [self hwnd]
  (var win nil)
  (each f (get-in self [:frame-tree :toplevels])
    (set win (:find-window f hwnd))
    (if win (break)))
  win)


(defn wm-add-window [self hwnd]
  (log/debug "new window: %n" hwnd)
  (def new-win (window hwnd self))
  (def frame-found
    (:find-frame-for-window (get-in self [:frame-tree :current-toplevel]) new-win))
  (try
    (:transform new-win (in frame-found :rect))
    ((err fib)
     # XXX: Don't manage a window which cannot be transformed?
     (log/error "window transformation failed: %n" err)))
  (:add-child frame-found new-win)
  new-win)


(defn wm-focus-changed [self hwnd]
  # XXX: If the focus change is caused by a closing window, that
  # window may still be alive, so it won't be purged immediately.
  # Maybe I shoud check the hwnds everytime a window is manipulated?
  (wm-purge-windows self)

  (when-let [win (wm-find-window self hwnd)]
    (:activate win)
    (break self))

  # TODO: window open/close events
  (:activate (wm-add-window self hwnd))
  self)


(defn wm-window-opened [self hwnd]
  (when-let [win (wm-find-window self hwnd)]
    (log/debug "window-opened event for managed window: %n" hwnd)
    (break self))

  # TODO: window open events
  (:activate (wm-add-window self hwnd))
  self)


(def- wm-proto
  @{:focus-changed wm-focus-changed
    :window-opened wm-window-opened
    :purge-windows wm-purge-windows
    :find-window wm-find-window
    :add-window wm-add-window})


(defn window-manager [uia-context]
  (def toplevel-frames @[])
  (def monitor-info (MONITORINFOEX))
  (var main-frame nil)
  (def enum-ret
    (EnumDisplayMonitors
     nil nil
     (fn [hmon hmdc rect]
       (def ret (GetMonitorInfo hmon monitor-info))
       (if (= FALSE ret)
         (error (string/format "GetMonitorInfo failed for monitor %n" hmon)))
       (def new-frame (frame (in monitor-info :rcWork)))
       (array/push toplevel-frames new-frame)
       (if (> (band (in monitor-info :dwFlags) MONITORINFOF_PRIMARY) (int/u64 0))
         (set main-frame new-frame))
       TRUE)))
  (if (= FALSE enum-ret)
    (error "EnumDisplayMonitors failed"))
  (log/debug "toplevel-frames = %n" toplevel-frames)
  (log/debug "main-frame = %n" main-frame)
  (when (empty? toplevel-frames)
    (error "no monitor found"))
  (table/setproto
   @{:frame-tree @{:toplevels toplevel-frames
                   :current-toplevel main-frame}
     :uia-context uia-context}
   wm-proto))
