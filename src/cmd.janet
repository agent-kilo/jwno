(use jw32/_winbase)
(use jw32/_winuser)
(use jw32/_processthreadsapi)
(use jw32/_errhandlingapi)
(use jw32/_combaseapi)
(use jw32/_uiautomation)
(use jw32/_dwmapi)
(use jw32/_util)

(use ./repl)
(use ./win)
(use ./uia)
(use ./input)
(use ./key)
(use ./resource)
(use ./util)

(import ./const)
(import ./log)


(defn get-current-frame-of-depth [wm &opt depth?]
  (def cur-frame (:get-current-frame (in wm :root)))
  (unless cur-frame
    (break nil))

  (def depth
    (if depth?
      depth?
      (:get-depth cur-frame)))

  (var fr cur-frame)
  (while (and (< depth (:get-depth fr))
              (in fr :parent)
              (= :frame (get-in fr [:parent :type])))
    (set fr (in fr :parent)))
  fr)


(defn cmd-nop [])


(defn cmd-quit [ui-man]
  (try
    (:destroy ui-man)
    ((err fib)
     (log/warning "Failed to destroy UI thread: %n\n%s"
                  err
                  (get-stack-trace fib))
     (os/exit 0))))


# TODO?
(defn cmd-send-keys [keys context]
  (let [input-seqs @[]]
    (var cur-inputs @[])
    (array/push input-seqs cur-inputs)
    (each k keys
      (match k
        [:wait duration]
        (do
          (array/push input-seqs duration)
          (set cur-inputs @[])
          (array/push input-seqs cur-inputs))

        [key-code key-state]
        (array/push cur-inputs (keyboard-input key-code key-state))

        key-code
        (do
          (array/push cur-inputs (keyboard-input key-code :down))
          (array/push cur-inputs (keyboard-input key-code :up)))

        _
        (log/warning "Unknown key spec: %n" k)))

    (log/debug "input-seqs = %n" input-seqs)

    (each seq input-seqs
      (if (number? seq)
        (ev/sleep seq)
        (send-input ;seq)))))


(defn cmd-retile [wm]
  (:retile wm))


(defn cmd-split-frame [wm dir &opt nfr ratios after-split-fn]
  (default nfr 2)
  (default ratios [0.5])

  (def cur-frame (:get-current-frame (in wm :root)))
  (unless cur-frame
    (break))

  (with-activation-hooks wm
    (:split cur-frame dir nfr ratios)

    (when after-split-fn
      (after-split-fn cur-frame))

    (:layouts-changed wm [(:get-layout cur-frame)])

    (:retile wm cur-frame)
    (:set-focus wm cur-frame)))


(defn- check-for-frame-resized-hooks [wm frame old-rect]
  (def new-rect (in frame :rect))
  (def [new-width new-height] (rect-size new-rect))
  (def [old-width old-height] (rect-size old-rect))

  (cond
    (not= :frame (get-in frame [:parent :type]))
    # A top-level frame
    (when (or (not= new-width old-width)
              (not= new-height old-height))
      (:frames-resized wm [frame]))

    (and (not= new-width old-width)
         (not= new-height old-height))
    (:frames-resized wm (get-in frame [:parent :parent :children]))

    (and (= new-width old-width)
         (= new-height old-height))
    :nop

    (not= new-width old-width)
    (case (:get-direction (in frame :parent))
      :horizontal
      (:frames-resized wm (get-in frame [:parent :children]))

      :vertical
      (:frames-resized wm (get-in frame [:parent :parent :children])))

    (not= new-height old-height)
    (case (:get-direction (in frame :parent))
      :horizontal
      (:frames-resized wm (get-in frame [:parent :parent :children]))

      :vertical
      (:frames-resized wm (get-in frame [:parent :children])))))


(defn cmd-insert-frame [wm location &opt after-insertion-fn direction depth?]
  (def cur-frame (get-current-frame-of-depth wm depth?))
  (when (nil? cur-frame)
    (break))

  (when (in cur-frame :monitor)
    # Don't do top-level frames
    (break))

  (when (nil? (in cur-frame :parent))
    (break))

  (def insert-to (in cur-frame :parent))
  (def dir
    (if direction
      (if (or (= :horizontal direction)
              (= :vertical direction))
        direction
        (errorf "invalid direction: %n" direction))
      # else
      (if-let [d (:get-direction insert-to)]
        d
        # else, try to guess from the shape of the frame
        (let [[vp-w vp-h] (rect-size (:get-viewport insert-to))]
          (if (< vp-w vp-h) :vertical :horizontal)))))
  (def insert-idx
    (if (= dir (:get-direction insert-to))
      (do
        (def cur-idx (find-index |(= $ cur-frame) (in insert-to :children)))
        (case location
          :before cur-idx
          :after (+ cur-idx 1)
          (errorf "unknown location to insert: %n" location)))
      # else
      (case location
        :before 0
        :after 1
        (errorf "unknown location to insert: %n" location))))

  (with-activation-hooks wm
    (:insert-sub-frame insert-to insert-idx nil dir)
    (def new-frame (get-in insert-to [:children insert-idx]))
    (def siblings (filter |(not= $ new-frame) (in insert-to :children)))
    (:frames-resized wm siblings)

    (when after-insertion-fn
      (after-insertion-fn new-frame))

    (:retile wm insert-to)
    (:set-focus wm insert-to)))


(defn cmd-flatten-parent [wm]
  (def cur-frame (:get-current-frame (in wm :root)))
  (unless cur-frame
    (break))

  (def parent (in cur-frame :parent))
  (cond
    (nil? parent)
    (break)

    (not= :frame (in parent :type))
    (break)

    true
    (with-activation-hooks wm
      (:flatten parent)
      (:layouts-changed wm [(:get-layout parent)])
      (:retile wm parent)
      (:set-focus wm (:get-current-window parent)))))


(defn cmd-enum-frame [wm dir]
  (when-let [cur-frame (:get-current-frame (in wm :root))
             fr (:enumerate-node (:get-layout cur-frame) cur-frame dir)]
    (with-activation-hooks wm
      (:sync-current-window fr)
      (:set-focus wm fr))))


(defn cmd-adjacent-frame [wm dir]
  (when-let [cur-frame (:get-current-frame (in wm :root))
             adj-fr (:get-adjacent-frame cur-frame dir)]
    (with-activation-hooks wm
      (:sync-current-window adj-fr)
      (:set-focus wm adj-fr))))


(defn cmd-enum-window-in-frame [wm dir &opt skip-minimized]
  (default skip-minimized true)

  (when-let [cur-frame (:get-current-frame (in wm :root))
             cur-win (:get-current-window cur-frame)]
    (var sibling (:enumerate-node cur-frame cur-win dir))
    (when skip-minimized
      (while (and (not= sibling cur-win)
                  (not= FALSE (IsIconic (in sibling :hwnd))))
        (set sibling (:enumerate-node cur-frame sibling dir))))
    (unless (= sibling cur-win)
      (with-activation-hooks wm
        (:set-focus wm sibling)))))


(defn cmd-cascade-windows-in-frame [wm &opt dx dy]
  (default dx 32)
  (default dy dx)

  (def frame (:get-current-frame (in wm :root)))
  (unless frame
    (break))

  (def top-frame (:get-top-frame frame))
  (def [dpi-x dpi-y] (get-in top-frame [:monitor :dpi]))
  (def [scale-x scale-y]
    [(/ dpi-x const/USER-DEFAULT-SCREEN-DPI)
     (/ dpi-y const/USER-DEFAULT-SCREEN-DPI)])
  (def [scaled-dx scaled-dy]
    [(* dx scale-x)
     (* dy scale-y)])

  (def win-stack (:get-window-stack frame))
  (def cur-rect (struct/to-table (:get-padded-rect frame)))

  (reverse! win-stack)
  (each win win-stack
    (def hwnd (in win :hwnd))
    (unless (or (= FALSE (IsWindow hwnd))
                (= FALSE (IsWindowVisible hwnd))
                (not= FALSE (IsIconic hwnd)))
      (:transform win cur-rect {:anchor :top-left} wm)
      (+= (cur-rect :left) scaled-dx)
      (+= (cur-rect :top) scaled-dy)))
  # Update frame's window list so that it matches the z-order
  (put frame :children win-stack))


(defn cmd-move-window [wm dir]
  (cond
    (find |(= $ dir) [:left :right :up :down])
    (when-let [cur-frame (:get-current-frame (in wm :root))
               cur-win (:get-current-window cur-frame)
               adj-fr (:get-adjacent-frame cur-frame dir)]
      (with-activation-hooks wm
        (:add-child adj-fr cur-win)
        (:layouts-changed wm [(:get-layout cur-frame)])
        (:transform cur-win (:get-padded-rect adj-fr) nil wm)
        # The focus is still on cur-win, so focus-changed event will not
        # fire, we need to activate its new parent frame manually here
        (:activate cur-win)))

    (and (indexed? dir)
         (= 2 (length dir)))
    (when-let [hwnd (:get-focused-hwnd wm)]
      (def [dx dy] dir)
      (def rect (:get-hwnd-rect wm hwnd))
      (SetWindowPos hwnd
                    nil
                    (+ dx (in rect :left))
                    (+ dy (in rect :top))
                    0
                    0
                    (bor SWP_NOZORDER SWP_NOSIZE)))

    true
    (errorf "invalid direction: %n" dir)))


(defn cmd-resize-window [wm dw dh]
  (when-let [hwnd (:get-focused-hwnd wm)]
    (def rect (:get-hwnd-rect wm hwnd))
    (SetWindowPos hwnd
                  nil
                  (in rect :left)
                  (in rect :top)
                  (+ dw (rect-width rect))
                  (+ dh (rect-height rect))
                  (bor SWP_NOZORDER))))


(defn cmd-resize-frame [wm dw dh]
  (def cur-frame (:get-current-frame (in wm :root)))
  (when (nil? cur-frame)
    (break))

  (def rect (in cur-frame :rect))
  (:resize cur-frame
           {:left (in rect :left)
            :top (in rect :top)
            :right (+ dw (in rect :right))
            :bottom (+ dh (in rect :bottom))})
  (check-for-frame-resized-hooks wm cur-frame rect)
  (:retile wm (:get-top-frame cur-frame)))


(defn cmd-zoom-in [wm ratio]
  (def cur-frame (:get-current-frame (in wm :root)))
  (unless cur-frame
    (break))

  (def parent-frame
    (if-let [parent (in cur-frame :parent)]
      (if (= :frame (in parent :type))
        parent)))
  (var next-frame
       (if parent-frame
         (in parent-frame :parent)))
  (var ortho-frame nil)
  (while (and next-frame
              (= :frame (in next-frame :type)))
    (unless (= (table/getproto parent-frame)
               (table/getproto next-frame))
      (set ortho-frame next-frame))
    (set next-frame (in next-frame :parent)))

  (def parent-rect
    (when parent-frame
      (:get-padded-viewport parent-frame)))
  (def ortho-rect
    (when ortho-frame
      (:get-padded-viewport ortho-frame)))

  (def old-rect (in cur-frame :rect))
  (def [cur-width cur-height] (rect-size old-rect))

  (def [new-width new-height]
    (cond
      (and ortho-rect parent-rect)
      (let [[parent-width parent-height] (rect-size parent-rect)
            [ortho-width ortho-height] (rect-size ortho-rect)]
        (cond
          (> parent-width cur-width) # parent is horizontal
          [(math/floor (* ratio parent-width))
           (math/floor (* ratio ortho-height))]

          (> parent-height cur-height) # parent is vertical
          [(math/floor (* ratio ortho-width))
           (math/floor (* ratio parent-height))]))

      parent-rect # ortho-frame not found
      (let [[parent-width parent-height] (rect-size parent-rect)]
        (cond
          (> parent-width cur-width)
          [(math/floor (* ratio parent-width))
           parent-height]

          (> parent-height cur-height)
          [parent-width
           (math/floor (* ratio parent-height))]))

      true
      [cur-width cur-height]))

  (unless (and (= new-width cur-width)
               (= new-height cur-height))
    (:resize cur-frame
             {:left 0
              :top 0
              :right new-width
              :bottom new-height})
    (check-for-frame-resized-hooks wm cur-frame old-rect)
    (:retile wm (:get-top-frame cur-frame))))


(defn cmd-balance-frames [wm &opt recursive?]
  (default recursive? true)

  (def cur-frame (:get-current-frame (in wm :root)))
  (unless cur-frame
    (break))

  (if recursive?
    (let [top-fr (:get-top-frame cur-frame)]
      (def resized (:balance top-fr recursive? @[]))
      (:frames-resized wm resized)
      (:retile wm top-fr))
    (let [parent (in cur-frame :parent)]
      (when (and parent
                 (= :frame (in parent :type)))
        (def resized (:balance parent recursive? @[]))
        (:frames-resized wm resized)
        (:retile wm parent)))))


(defn cmd-rotate-sibling-frames [wm &opt dir steps depth?]
  (default dir :forward)
  (default steps 1)

  (def fr (get-current-frame-of-depth wm depth?))
  (unless fr
    (break))

  (def parent (in fr :parent))

  (cond
    (nil? parent)
    (break)

    (or (= :frame (in parent :type))
        (= :layout (in parent :type)))
    (do
      (repeat steps
        (:rotate-children parent dir))
      # To update visual indicators, the :frame-resized hook
      # needs to fire, even though the frame sizes are always
      # the same after rotation. Maybe rename the hook to
      # :frame-rect-updated ?
      (:frames-resized wm (slice (in parent :children)))
      (:retile wm parent))))


(defn cmd-reverse-sibling-frames [wm &opt depth?]
  (def fr (get-current-frame-of-depth wm depth?))
  (unless fr
    (break))

  (def parent (in fr :parent))

  (cond
    (nil? parent)
    (break)

    (or (= :frame (in parent :type))
        (= :layout (in parent :type)))
    (do
      (:reverse-children parent)
      # To update visual indicators, the :frame-resized hook
      # needs to fire, even though the frame sizes are always
      # the same after reversing.
      (:frames-resized wm (slice (in parent :children)))
      (:retile wm parent))))


(defn cmd-toggle-parent-direction [wm &opt recursive depth?]
  (def fr (get-current-frame-of-depth wm depth?))
  (unless fr
    (break))

  (def parent (in fr :parent))

  (cond
    (nil? parent)
    (break)

    (not= :frame (in parent :type))
    (break)

    true
    (do
      (:toggle-direction parent recursive)
      (:frames-resized wm (slice (in parent :children)))
      (:retile wm parent))))


(defn cmd-toggle-parent-viewport [wm &opt depth]
  (when-let [fr (get-current-frame-of-depth wm depth)
             parent (in fr :parent)]
    (unless (= :frame (in parent :type))
      (break))

    (if (:constrained? parent)
      (:set-viewport parent (in parent :rect))
      # else
      (:remove-viewport parent))

    (:frames-resized wm [parent])
    (:retile wm parent)))


(defn cmd-scroll-parent [wm distance &opt depth]
  (when-let [fr (get-current-frame-of-depth wm depth)
             parent (in fr :parent)]
    (unless (= :frame (in parent :type))
      (break))

    (when (:constrained? parent)
      (break))

    (def parent-dir (:get-direction parent))
    (def rect (in parent :rect))
    (def new-rect
      (case parent-dir
        :horizontal
        {:left   (- (in rect :left) distance)
         :right  (- (in rect :right) distance)
         :top    (in rect :top)
         :bottom (in rect :bottom)}

        :vertical
        {:left   (in rect :left)
         :right  (in rect :right)
         :top    (- (in rect :top) distance)
         :bottom (- (in rect :bottom) distance)}

        (errorf "unknown direction: %n" parent-dir)))

    (put parent :rect new-rect)
    (:transform parent (in parent :viewport))
    (:frames-resized wm [parent])
    (:retile wm parent)))


(defn cmd-close-frame [wm &opt cur-frame]
  (default cur-frame (:get-current-frame (in wm :root)))

  (when (or (nil? cur-frame)
            # Skip top-level frames
            (in cur-frame :monitor))
    (break))

  (def cur-win (:get-current-window cur-frame))
  (with-activation-hooks wm
    (:close cur-frame)

    (def parent (in cur-frame :parent))
    (def parent-cur-children (in parent :children))
    (cond
      (empty? parent-cur-children)
      # All sibling frames are closed
      (:layouts-changed wm [(:get-layout parent)])

      (= :window (get-in parent-cur-children [0 :type]))
      # The parent's current children are windows, in that
      # case all sibling frames are closed too.
      (:layouts-changed wm [(:get-layout parent)])

      (= :frame (get-in parent-cur-children [0 :type]))
      # :frames-resized will trigger :layouts-changed
      (:frames-resized wm parent-cur-children))

    (:retile wm (in cur-frame :parent))
    (if cur-win
      (:set-focus wm cur-win)
      (:set-focus wm (:get-current-window (in wm :root))))))


(defn cmd-frame-to-window-size [wm]
  (when-let [cur-frame (:get-current-frame (in wm :root))
             cur-win (:get-current-window cur-frame)]
    (def win-rect
      (:get-rect cur-win true))

    (def border-space
      (combine-rect-border-space (:get-margins cur-win)
                                 (:get-paddings cur-frame)))

    (def old-rect (in cur-frame :rect))
    (:resize cur-frame (expand-rect win-rect border-space))
    (check-for-frame-resized-hooks wm cur-frame old-rect)
    (:retile wm (:get-top-frame cur-frame))))


(defn cmd-close-window [wm ui-man]
  (def win-pat
    (with-uia [uia-win (:get-focused-window (in wm :uia-manager))]
      (when uia-win
        (:GetCurrentPatternAs uia-win UIA_WindowPatternId IUIAutomationWindowPattern))))

  (if win-pat
    (with-uia [_win-pat win-pat]
      (:Close win-pat))
    # else
    (if-let [root (in wm :root)
             cur-win (:get-current-window root)]
      (let [fg-hwnd (GetForegroundWindow)]
        (if (= fg-hwnd (in cur-win :hwnd))
          # The current window is minimized or invisible, but somehow it
          # got the focus. Try to close it anyway.
          (:close cur-win)
          # The current window lost focus, do nothing
          (:show-tooltip ui-man :close-window "No focused window.")))
      (:show-tooltip ui-man :close-window "No focused window."))))


(defn cmd-close-window-or-frame [wm ui-man]
  # This will be nil when SHELLDLL_DefView (the desktop) is focused
  (def win-pat
    (with-uia [uia-win (:get-focused-window (in wm :uia-manager))]
      (when uia-win
        (:GetCurrentPatternAs uia-win UIA_WindowPatternId IUIAutomationWindowPattern))))

  (if win-pat
    (with-uia [_win-pat win-pat]
      (:Close win-pat))
    # else
    (when-let [root (in wm :root)
               cur-frame (:get-current-frame root)]
      (if-let [cur-win (:get-current-window cur-frame)]
        (let [fg-hwnd (GetForegroundWindow)]
          (if (= fg-hwnd (in cur-win :hwnd))
            # The current window is minimized or invisible, but somehow it
            # got the focus. Try to close it anyway.
            (:close cur-win)
            # The current window lost focus, do nothing
            (:show-tooltip ui-man :close-window "No focused window.")))
        # The frame is empty, assume that we want to close it
        (cmd-close-frame wm cur-frame)))))


(defn cmd-change-window-alpha [wm delta]
  (def cur-win (:get-current-window (in wm :root)))
  (when (nil? cur-win)
    (break))

  (def old-alpha (:get-alpha cur-win))
  (def new-alpha
    (let [val (math/floor (+ old-alpha delta))]
      (cond
        (< val 0) 0
        (> val 255) 255
        true val)))
  (log/debug "Setting window alpha from %n to %n" old-alpha new-alpha)
  (:set-alpha cur-win new-alpha))


(defn cmd-describe-window [wm ui-man]
  (with-uia [uia-win (:get-focused-window (in wm :uia-manager) false)]
    (if (nil? uia-win)
      (:show-tooltip ui-man :describe-window "No focused window.")
      (do
        (def hwnd (:get_CachedNativeWindowHandle uia-win))
        (if-let [win-info (:get-hwnd-info wm hwnd uia-win)]
          (with-uia [_uia-win (in win-info :uia-element)]
            (def {:exe-path exe-path
                  :virtual-desktop desktop-info}
              win-info)
            (def rect (:get-hwnd-rect wm hwnd))
            (def efb-rect
              (try
                (:get-hwnd-rect wm hwnd true)
                ((err fib)
                 (log/debug ":get-hwnd-rect failed for %n: %n" hwnd err)
                 nil)))
            (def dpia-ctx (GetWindowDpiAwarenessContext hwnd))
            (def dpi-awareness (GetAwarenessFromDpiAwarenessContext dpia-ctx))
            (:show-tooltip ui-man
                           :describe-window
                           (string/format (string/join
                                           ["HWND: %n"
                                            "EXE: %s"
                                            "Name: %s"
                                            "Class Name: %s"
                                            "Control Type: %n"
                                            "Virtual Desktop Name: %s"
                                            "Virtual Desktop ID: %s"
                                            "Rect: %n"
                                            "EFB Rect: %s"
                                            "DPI Awareness: %n"
                                            "Filter Result: %n"]
                                           "\n")
                                          hwnd
                                          (or exe-path "n/a")
                                          (or (:get_CachedName uia-win) "n/a")
                                          (or (:get_CachedClassName uia-win) "n/a")
                                          (:get_CachedControlType uia-win)
                                          (or (in desktop-info :name) "n/a")
                                          (or (in desktop-info :id)   "n/a")
                                          rect
                                          (if efb-rect
                                            (string/format "%n" efb-rect)
                                            "n/a")
                                          dpi-awareness
                                          (:filter-hwnd wm hwnd uia-win exe-path desktop-info))
                           (if efb-rect
                             (in efb-rect :left)
                             (in rect :left))
                           (if efb-rect
                             (in efb-rect :top)
                             (in rect :top))))

          (:show-tooltip ui-man
                         :describe-window
                         (string/format "Failed to get window info for %n." hwnd)))))))


(defn cmd-manage-window [wm ui-man]
  (with-uia [uia-win (:get-focused-window (in wm :uia-manager) true)]
    (unless uia-win
      (:show-tooltip ui-man :manage-window "No focused window.")
      (break))

    (def hwnd (:get_CachedNativeWindowHandle uia-win))
    (put (in wm :ignored-hwnds) hwnd nil)

    (if (:find-hwnd (in wm :root) hwnd)
      (:show-tooltip ui-man :manage-window
         (string/format "Window \"%s\" is already managed."
                        (:get_CachedName uia-win)))
      (when-let [info (:get-hwnd-info wm hwnd uia-win)]
        (with-uia [_uia-win (in info :uia-element)]
          (:show-tooltip ui-man :manage-window
             (string/format "Adding \"%s\" to managed windows." (:get_CachedName uia-win)))
          (:add-hwnd wm info :forced))))))


(var wm-pause-hook-fn nil)

(defn cmd-toggle-window-management [hook-man ui-man]
  (if wm-pause-hook-fn
    (do
      (:remove-hook hook-man :filter-window wm-pause-hook-fn)
      (set wm-pause-hook-fn nil)
      (:show-tooltip ui-man :toggle-window-management "Window management enabled."))
    # else
    (do
      (set wm-pause-hook-fn (:add-hook hook-man :filter-window (fn wm-pause-window-filter [&] false)))
      (:show-tooltip ui-man :toggle-window-management "Window management disabled."))))


(defn cmd-ignore-window [wm ui-man]
  (with-uia [uia-win (:get-focused-window (in wm :uia-manager) true)]
    (unless uia-win
      (:show-tooltip ui-man :manage-window "No focused window.")
      (break))

    (def hwnd (:get_CachedNativeWindowHandle uia-win))

    (:show-tooltip ui-man :ignore-window
       (string/format "Ignoring \"%s\"" (:get_CachedName uia-win)))
    (:ignore-hwnd wm hwnd)
    (:remove-hwnd wm hwnd)))


(defn cmd-exec [wm ui-man verbose? cli]
  (when verbose?
    (:show-tooltip ui-man
                   :exec
                   (string/format "Running command: %n" cli)))

  # ev/spawn So that waiting for the command won't block the main loop
  (ev/spawn
   (def env (os/environ))
   (put env :out :pipe)
   (put env :err :pipe)
   (when-let [proc (try
                     (os/spawn cli :ep env)
                     ((err fib)
                      (:show-tooltip ui-man
                                     :exec
                                     (string/format "Failed to start command: %n\n%s\n%s"
                                                    cli
                                                    err
                                                    (get-stack-trace fib)))
                      nil))]
     # XXX: No limit on the output text
     (def out (:read (in proc :out) :all))
     (def err (:read (in proc :err) :all))
     (os/proc-wait proc)

     (def ret (in proc :return-code))
     (log/debug "Exit code of command %n: %n" cli ret)

     (when (not= 0 ret)
       (:show-tooltip ui-man
                      :exec
                      (string/format "Command: %n\nExit code %d:\n%s"
                                     cli
                                     ret
                                     (string out "\n" err)))))))


(defn cmd-summon [wm ui-man match-fn &opt pull? cli]
  (default pull? false)
  (default cli [])

  (def all-windows (:get-all-windows (in wm :root)))
  (var win-found nil)
  (each w all-windows
    (when (try
            (match-fn w)
            ((err fib)
             (log/debug "match-fn failed: %n\n%s"
                        err
                        (get-stack-trace fib))
             false))
      (set win-found w)
      (break)))

  (if win-found
    (do
      (:reset-visual-state win-found true false wm)
      (when pull?
        (def cur-frame (:get-current-frame (in wm :root)))
        (def cur-vd (in (:get-layout cur-frame) :id))
        (def win-vd (in (:get-virtual-desktop win-found nil wm) :id))
        # Only pull the window when it's in the same virtual desktop,
        # since the Windows API doesn't support moving windows across
        # virtual desktops.
        (when (= cur-vd win-vd)
          (:add-child cur-frame win-found)
          (:transform win-found (:get-padded-rect cur-frame) nil wm)))
      (with-activation-hooks wm
        (:set-focus wm win-found)))
    (if (empty? cli)
      (:show-tooltip ui-man :summon "Summoning failed. Window not found.")
      (cmd-exec wm ui-man true cli))))


(defn cmd-repl [context &opt start? host port]
  (default start? false)
  (default host const/DEFAULT-REPL-HOST)
  (default port const/DEFAULT-REPL-PORT)

  (def ui-man (in context :ui-manager))
  (def repl-man (in context :repl-manager))
  (def existing-repl (:get-default-server repl-man))

  (when (nil? existing-repl)
    (if start?
      (do
        (:show-tooltip ui-man :repl "REPL is not running. Trying to start it...")
        (:start-server repl-man host port))
      (do
        (:show-tooltip ui-man :repl "REPL is not running.")
        # Early return
        (break))))

  (def repl (:get-default-server repl-man))

  (def argv (dyn :args))
  (def argv0 (first argv))
  (def env (os/environ))

  (def exe-buf (QueryFullProcessImageName (GetCurrentProcess) 0))
  (unless exe-buf
    (def err-code (GetLastError))
    (log/error "repl: failed to locate process image: %n" err-code)
    (:show-tooltip ui-man :repl (string/format "Failed to locate Jwno executable file: %n" err-code))
    # Early return
    (break))

  (def exe-path (string exe-buf))
  (def [repl-host repl-port] (in repl :address))
  (def cmd-path (string (in env "SystemRoot") "\\system32\\cmd.exe"))

  (def repl-cli
    # Are we running from the source tree?
    (if (or (string/has-suffix? "/main.janet" argv0)
            (string/has-suffix? "\\main.janet" argv0))
      # Call cmd.exe to give the REPL client a new console
      [cmd-path "/c" "start" exe-path exe-path argv0 "-C" "--repl" (string/format "%s:%d" repl-host repl-port)]
      [cmd-path "/c" "start" exe-path exe-path "-C" "--repl" (string/format "%s:%d" repl-host repl-port)]))

  (log/debug "repl-cli = %n" repl-cli)
  (os/execute repl-cli))


(defn cmd-describe-key [context]
  (def {:key-manager key-man
        :hook-manager hook-man
        :ui-manager ui-man}
    context)

  (var saved-hook-fn nil)
  (set saved-hook-fn 
       (:add-hook hook-man :key-pressed
          (fn [key]
            # Ignore modifier key events, since they're included
            # in (key :modifiers)
            (when (in MODIFIER-KEYS (in key :key))
              (break))

            (log/debug "removing :key-pressed hook for :describe-key")
            (:remove-hook hook-man :key-pressed saved-hook-fn)
            (:set-key-mode key-man :command)

            (:show-tooltip
               ui-man
               :describe-key
               (string/format (string/join
                               ["Key Code: %n"
                                "Key Name: %n"
                                "Modifiers : %n"]
                               "\n")
                              (in key :key)
                              (in key-code-to-name (in key :key))
                              (in key :modifiers))))))

  (:set-key-mode key-man :raw)
  (:show-tooltip ui-man :describe-key "Please press a key." nil nil 0))


(defn add-default-commands [command-man context]
  (def {:ui-manager ui-man
        :hook-manager hook-man
        :window-manager wm}
    context)

  (:add-command command-man :nop cmd-nop
     ```
     (:nop)

     Does nothing. When you want to return from a sub-keymap, without
     executing any actual commands, map a key to call this command, and
     it'll act as a "cancel" key.
     ```)
  (:add-command command-man :quit
     (fn [] (cmd-quit ui-man))
     ```
     (:quit)

     Tells Jwno to stop running.
     ```)
  (:add-command command-man :retile
     (fn [] (cmd-retile wm))
     ```
     (:retile)

     Fits all managed windows to their frames.
     ```)

  (:add-command command-man :exec
     (fn [verbose? & cli]
       (cmd-exec wm ui-man verbose? cli))
     ```
     (:exec & cli)

     Executes a command line specified by cli.
     ```)
  (:add-command command-man :summon
     (fn [match-fn &opt pull? & cli]
       (cmd-summon wm ui-man match-fn pull? cli))
     ```
     (:summon match-fn &opt pull? & cli)

     Summons a managed window for which match-fn returns a truthy value.
     Match-fn should accept a window object as its sole argument. When
     pull? is truthy, and the matching window is on the currently active
     virtual desktop, that window will be pulled into the current frame.
     Otherwise, simply bring focus to the matching window. If no matching
     window is found, will try to execute cli unless its empty.
     ```)
  (:add-command command-man :repl
     (fn [&opt start? host port]
       (cmd-repl context start? host port))
     ```
     (:repl &opt start? host port)

     Opens an REPL window for the current Jwno process. If start? is
     truthy, and no REPL server is available, a new server will be
     started. Host and port specify the address to connect, and they
     default to "127.0.0.1" and 9999.
     ```)

  (:add-command command-man :split-frame
     (fn [dir &opt nfr ratios after-split-fn]
       (cmd-split-frame wm dir nfr ratios after-split-fn))
     ```
     (:split-frame dir &opt nfr ratios after-split-fn)

     Divides a frame into multiple sub-frames. Dir can be :vertical or
     :horizontal. Nfr specifies the number of sub-frames which defaults
     to 2. Ratios, if provided, should be a tuple or array, containing
     ratios for sub-frame sizes. After-split-fn is a function accepting
     the frame object being divided as its sole argument, and it will
     be called after the split.

     For example, calling the command (:split-frame :vertical 3 [0.1 0.3 0.6])
     splits the current frame into 3 vertical sub-frames, whose heights
     are 0.1, 0.3 and 0.6 of the original frame height, respectively.
     ```)
  (:add-command command-man :insert-frame
     (fn [location &opt after-insertion-fn direction depth]
       (cmd-insert-frame wm location after-insertion-fn direction depth))
     ```
     (:insert-frame location &opt after-insertion-fn direction depth)

     Inserts a new frame and adjusts its sibling or parent frames' sizes
     as needed.

     Location is relative to the current frame in the level specified by
     depth, can be :before or :after.

     After-insertion-fn is a function accepting the new inserted frame
     object as its sole argument, and it will be called after the
     insertion.

     Direction is the direction to insert the new frame in. Can be
     :horizontal or :vertical. Uses the parent frame's current direction
     if not provided.

     Depth specifies in which level the new frame should be inserted, and
     1 means to insert a child frame to the current top-level frame.
     Defaults to the depth of the current frame.
     ```)
  (:add-command command-man :flatten-parent
     (fn [] (cmd-flatten-parent wm))
     ```
     (:flatten-parent)

     Flattens the parent frame, by closing the current frame and all
     its siblings.
     ```)

  (:add-command command-man :resize-frame
     (fn [dw dh] (cmd-resize-frame wm dw dh))
     ```
     (:resize-frame dw dh)

     Resizes the current frame. Dw and dh are the deltas of width and
     height, respectively. Both of them are in physical pixels.

     Note that this command also resizes all windows contained in the
     frame. To resize a single window, use the :resize-window command.
     ```)
  (:add-command command-man :close-frame
     (fn [] (cmd-close-frame wm))
     ```
     (:close-frame)

     Closes the current frame.
     ```)
  (:add-command command-man :frame-to-window-size
     (fn [] (cmd-frame-to-window-size wm))
     ```
     (:frame-to-window-size)

     Resizes the current frame, so that it's the same size as its
     current active window. Constraints from the parent frame apply.
     ```)
  (:add-command command-man :balance-frames
     (fn [&opt recursive?] (cmd-balance-frames wm recursive?))
     ```
     (:balance-frames &opt recursive?)

     Resizes frames, so that all siblings belonging to the same parent
     have the same size. When recursive? is true (the default), resizes
     all frames in the current monitor. Otherwise, only resizes the
     siblings of the current frame.
     ```)
  (:add-command command-man :rotate-sibling-frames
     (fn [&opt dir steps depth] (cmd-rotate-sibling-frames wm dir steps depth))
     ```
     (:rotate-sibling-frames &opt dir steps depth)

     Rotates sibling frames.

     When dir is :forward, the first frame will be moved to the end
     of the frame list. When dir is :backward, the last frame will be
     moved to the beginning of the frame list instead. Defaults to
     :forward.

     Steps specifies how many times the frame list should be rotated.
     Defaults to 1.

     Depth specifies which level of frames should be rotated. 0 means
     to rotate top-level frames, 1 means to rotate children of top-level
     frames, etc. Defaults to the level of the current active leaf
     frame.
     ```)
  (:add-command command-man :reverse-sibling-frames
     (fn [&opt depth] (cmd-reverse-sibling-frames wm depth))
     ```
     (:reverse-sibling-frames &opt depth)

     Reverses the order of sibling frames.

     Depth specifies which level of frames should be reversed. 0 means
     to reverse top-level frames, 1 means to reverse children of top-level
     frames, etc. Defaults to the level of the current active leaf
     frame.
     ```)
  (:add-command command-man :toggle-parent-direction
     (fn [&opt recursive depth] (cmd-toggle-parent-direction wm recursive depth))
     ```
     (:toggle-parent-direction &opt recursive depth)

     Changes the direction of the parent frame. If the direction was
     :horizontal, it becomes :vertical, and vice versa.

     Depth specifies which level of frame's direction should be toggled.
     1 means to toggle top-level frames, 2 means to toggle children of
     top-level frames, etc. Defaults to the parent of the current active
     leaf frame.
     ```)
  (:add-command command-man :toggle-parent-viewport
     (fn [&opt depth] (cmd-toggle-parent-viewport wm depth))
     ```
     (:toggle-parent-viewport &opt depth)

     Enables or disables viewport for a parent frame. The parent frame
     becomes unconstrained when its viewport is enabled.

     Depth specifies which level of frame's viewport should be toggled.
     1 means to toggle top-level frames, 2 means to toggle children of
     top-level frames, etc. Defaults to the parent of the current active
     leaf frame.
     ```)
  (:add-command command-man :scroll-parent
     (fn [distance &opt depth] (cmd-scroll-parent wm distance depth))
     ```
     (:scroll-parent distance &opt depth)

     Scrolls an unconstained parent frame by the amount specified by
     distance (in pixels). Use a positive distance to scroll forward,
     or a negative distance to scroll backward.

     Depth specifies which level of frames should be affected. 1 means
     to scroll top-level frames, and 2 means to scroll children of
     top-level frames, etc. Defaults to the parent of the current active
     leaf frame.

     Does nothing if the parent frame is constrained.
     ```)
  (:add-command command-man :zoom-in
     (fn [ratio] (cmd-zoom-in wm ratio))
     ```
     (:zoom-in ratio)

     Resizes the current frame, so that its size is proportional to
     its ancestors', according to ratio.

     For example, (:zoom-in 0.7) makes a frame to have 0.7 of both
     its ancestors' width and height.
     ```)

  (:add-command command-man :enum-frame
     (fn [dir] (cmd-enum-frame wm dir))
     ```
     (:enum-frame dir)

     Brings focus to another frame in the specified direction. Dir
     can be :next or :prev.
     ```)
  (:add-command command-man :adjacent-frame
     (fn [dir] (cmd-adjacent-frame wm dir))
     ```
     (:adjacent-frame dir)

     Brings focus to the adjacent frame in the specified direction.
     Dir can be :left, :right, :up or :down.
     ```)

  (:add-command command-man :enum-window-in-frame
     (fn [dir] (cmd-enum-window-in-frame wm dir))
     ```
     (:enum-window-in-frame dir)

     Brings focus to another window in the current frame, in the
     specified direction. Dir can be :next or :prev.
     ```)
  (:add-command command-man :cascade-windows-in-frame
     (fn [&opt dx dy] (cmd-cascade-windows-in-frame wm dx dy))
     ```
     (:cascade-windows-in-frame &opt dx dy)

     Cascades windows in the current frame. Dx and dy are the X and
     Y offsets for each window, respectively. Dx and dy are both in
     virtual pixels, and default to 32.
     ```)

  (:add-command command-man :move-window
     (fn [dir] (cmd-move-window wm dir))
     ```
     (:move-window dir)

     Moves a window in the specified direction. Dir can be :left,
     :right, :up, :down, or a tuple in the form of [dx dy].

     When dir is a direction keyword, the window is moved between
     adjacent frames.

     When dir is a tuple, the window is moved freely, unconstrained
     by frames. Dx and dy are distances (in physical pixels) to move
     the window on the horizontal and vertical axes, respectively.
     ```)
  (:add-command command-man :resize-window
     (fn [dw dh] (cmd-resize-window wm dw dh))
     ```
     (:resize-window dw dh)

     Resizes the focused window. Dw and dh are the deltas of width
     and height, respectively. Both of them are in physical pixels.

     Note that this command resizes a single window. To resize all
     windows contained in a frame, use the :resize-frame command.
     ```)
  (:add-command command-man :close-window
     (fn [] (cmd-close-window wm ui-man))
     ```
     (:close-window)

     Tries to close the focused window. This command works by
     sending a message to the target window, instead of forcibly
     killing the window's process.
     ```)
  (:add-command command-man :change-window-alpha
     (fn [delta] (cmd-change-window-alpha wm delta))
     ```
     (:change-window-alpha delta)

     Changes the focused window's alpha value, making it partially
     transparent. Delta, which can be negative, is the increment to
     apply to the alpha value. A window's alpha value is in the
     range [0, 255]. Some window's alpha cannot be changed.
     ```)

  (:add-command command-man :close-window-or-frame
     (fn [] (cmd-close-window-or-frame wm ui-man))
     ```
     (:close-window-or-frame)

     If the current frame is not empty, closes its active window.
     Otherwise, closes the current frame.
     ```)

  (:add-command command-man :describe-window
     (fn [] (cmd-describe-window wm ui-man))
     ```
     (:describe-window)

     Shows basic info about a window.
     ```)

  (:add-command command-man :manage-window
     (fn [] (cmd-manage-window wm ui-man))
     ```
     (:manage-window)

     Forcibly adds the current window to the list of managed windows.
     ```)

  (:add-command command-man :toggle-window-management
     (fn [] (cmd-toggle-window-management hook-man ui-man))
     ```
     (:toggle-window-management)

     Pauses/unpauses window management. Will not affect windows that
     are already managed.
     ``` )

  (:add-command command-man :ignore-window
     (fn [] (cmd-ignore-window wm ui-man))
     ```
     (:ignore-window)

     Removes the current window from the list of managed windows.
     ```)

  (:add-command command-man :describe-key
     (fn [] (cmd-describe-key context))
     ```
     (:describe-key)

     Interactively reads a key, and shows some info about it.
     ```)
  )


(defn command-manager-call-command [self cmd & args]
  (def commands (in self :commands))
  (def hook-man (in self :hook-manager))
  (def found (in commands cmd))

  (cond
    (not found)
    (errorf "unknown command: %n, args: %n" cmd args)

    (:call-filter-hook hook-man :and :filter-command cmd args)
    (do
      ((in found :fn) ;args)
      (:call-hook hook-man :command-executed cmd args)
      true)

    false))


(defn command-manager-dispatch-command [self cmd-and-args]
  (def call-with
    (if (indexed? cmd-and-args)
      cmd-and-args
      [cmd-and-args]))
  (try
    (command-manager-call-command self ;call-with)
    ((err fib)
     (log/error "Command %n failed: %n\n%s" call-with err (get-stack-trace fib)))))


(defn command-manager-add-command [self name cmd-fn &opt doc]
  (put (in self :commands)
       name
       @{:fn cmd-fn
         :doc doc}))


(defn command-manager-get-command [self name]
  (get-in self [:commands name]))


(defn command-manager-remove-command [self name]
  (put (in self :commands) name nil))


(defn command-manager-print-doc [self name]
  (if-let [cmd (:get-command self name)
           cmd-doc (in cmd :doc)]
    (print (doc-format cmd-doc))
    (print (doc-format "No doc available."))))


(def- command-manager-proto
  @{:call-command command-manager-call-command
    :dispatch-command command-manager-dispatch-command
    :add-command command-manager-add-command
    :get-command command-manager-get-command
    :remove-command command-manager-remove-command
    :print-doc command-manager-print-doc})


(defn command-manager [hook-man]
  (table/setproto
   @{:commands @{}
     :hook-manager hook-man}
   command-manager-proto))
