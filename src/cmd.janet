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
(use ./resource)
(use ./util)

(import ./const)
(import ./log)


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

  (with-activation-hooks wm
    (:split cur-frame dir nfr ratios)

    (when after-split-fn
      (after-split-fn cur-frame))

    (:retile wm cur-frame)
    (:activate wm cur-frame)))


(defn- call-frame-resized-hooks [hook-man frame-list]
  (each fr frame-list
    (def children (in fr :children))

    # Only call hooks on leaf frames
    (cond
      (empty? children)
      (:call-hook hook-man :frame-resized fr)

      (= :window (get-in fr [:children 0 :type]))
      (:call-hook hook-man :frame-resized fr)

      (= :frame (get-in fr [:children 0 :type]))
      (call-frame-resized-hooks hook-man (in fr :children)))))


(defn- check-for-frame-resized-hooks [hook-man frame old-rect]
  (def new-rect (in frame :rect))
  (def [new-width new-height] (rect-size new-rect))
  (def [old-width old-height] (rect-size old-rect))

  (cond
    (and (not= new-width old-width)
         (not= new-height old-height))
    (call-frame-resized-hooks hook-man (get-in frame [:parent :parent :children]))

    (and (= new-width old-width)
         (= new-height old-height))
    :nop

    (not= new-width old-width)
    (case (:get-direction (in frame :parent))
      :horizontal
      (call-frame-resized-hooks hook-man (get-in frame [:parent :children]))

      :vertical
      (call-frame-resized-hooks hook-man (get-in frame [:parent :parent :children])))

    (not= new-height old-height)
    (case (:get-direction (in frame :parent))
      :horizontal
      (call-frame-resized-hooks hook-man (get-in frame [:parent :parent :children]))

      :vertical
      (call-frame-resized-hooks hook-man (get-in frame [:parent :children])))))


(defn cmd-insert-frame [wm hook-man location &opt after-insertion-fn]
  (def cur-frame (:get-current-frame (in wm :root)))
  (when (or (in cur-frame :monitor)
            (nil? (in cur-frame :parent)))
    (break))

  (def parent (in cur-frame :parent))
  (def cur-idx (find-index |(= $ cur-frame) (in parent :children)))
  (def insert-idx
    (case location
      :before cur-idx
      :after (+ cur-idx 1)
      (errorf "unknown location to insert: %n" location)))

  (with-activation-hooks wm
    (:insert-sub-frame parent insert-idx)

    (def new-frame (get-in parent [:children insert-idx]))
    (def siblings (filter |(not= $ new-frame) (in parent :children)))
    (call-frame-resized-hooks hook-man siblings)

    (when after-insertion-fn
      (after-insertion-fn new-frame))

    (:retile wm parent)
    (:activate wm parent)))


(defn cmd-flatten-parent [wm]
  (def cur-frame (:get-current-frame (in wm :root)))
  (def parent (in cur-frame :parent))
  (cond
    (nil? parent)
    (break)

    (not= :frame (in parent :type))
    (break)

    true
    (with-activation-hooks wm
      (:flatten parent)
      (:retile wm parent)
      (:activate wm (:get-current-window parent)))))


(defn cmd-enum-frame [wm dir]
  (def cur-frame (:get-current-frame (in wm :root)))
  (when-let [fr (:enumerate-node (:get-layout cur-frame) cur-frame dir)]
    (with-activation-hooks wm
      (:sync-current-window fr)
      (:activate wm fr))))


(defn cmd-adjacent-frame [wm dir]
  (def cur-frame (:get-current-frame (in wm :root)))
  (when-let [adj-fr (:get-adjacent-frame cur-frame dir)]
    (with-activation-hooks wm
      (:sync-current-window adj-fr)
      (:activate wm adj-fr))))


(defn cmd-enum-window-in-frame [wm dir &opt skip-minimized]
  (default skip-minimized true)
  (def cur-frame (:get-current-frame (in wm :root)))
  (when-let [cur-win (:get-current-window cur-frame)]
    (var sibling (:enumerate-node cur-frame cur-win dir))
    (when skip-minimized
      (while (and (not= sibling cur-win)
                  (not= FALSE (IsIconic (in sibling :hwnd))))
        (set sibling (:enumerate-node cur-frame sibling dir))))
    (unless (= sibling cur-win)
      (with-activation-hooks wm
        (:activate wm sibling)))))


(defn cmd-cascade-windows-in-frame [wm &opt dx dy]
  (default dx 32)
  (default dy dx)

  (def frame (:get-current-frame (in wm :root)))

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
      (:transform win cur-rect {:anchor :top-left})
      (+= (cur-rect :left) scaled-dx)
      (+= (cur-rect :top) scaled-dy)))
  # Update frame's window list so that it matches the z-order
  (put frame :children win-stack))


(defn cmd-move-window [wm dir]
  (def cur-frame (:get-current-frame (in wm :root)))
  (def cur-win (:get-current-window cur-frame))

  (when (nil? cur-win) (break))

  (when-let [adj-fr (:get-adjacent-frame cur-frame dir)]
    (with-activation-hooks wm
      (:add-child adj-fr cur-win)
      (:retile wm adj-fr)
      (:activate wm cur-win))))


(defn cmd-resize-frame [wm hook-man dw dh]
  (def cur-frame (:get-current-frame (in wm :root)))
  (when (in cur-frame :monitor)
    # Skip top-level frames
    (break))

  (def rect (in cur-frame :rect))
  (:resize cur-frame
           {:left (in rect :left)
            :top (in rect :top)
            :right (+ dw (in rect :right))
            :bottom (+ dh (in rect :bottom))})
  (check-for-frame-resized-hooks hook-man cur-frame rect)
  (:retile wm))


(defn cmd-zoom-in [wm hook-man ratio]
  (def cur-frame (:get-current-frame (in wm :root)))
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
      (:get-padded-rect parent-frame)))
  (def ortho-rect
    (when ortho-frame
      (:get-padded-rect ortho-frame)))

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
    (check-for-frame-resized-hooks hook-man cur-frame old-rect)
    (:retile wm)))


(defn cmd-balance-frames [wm]
  (def cur-frame (:get-current-frame (in wm :root)))
  (def cur-layout (:get-layout cur-frame))
  (each fr (in cur-layout :children)
    (:balance fr true))
  (:retile wm))


(defn cmd-close-frame [wm hook-man]
  (def root (in wm :root))
  (def cur-frame (:get-current-frame root))
  (when (in cur-frame :monitor)
    # Skip top-level frames
    (break))

  (def cur-win (:get-current-window cur-frame))
  (with-activation-hooks wm
    (:close cur-frame)

    (def parent-cur-children (get-in cur-frame [:parent :children]))
    (cond
      (empty? parent-cur-children)
      # All sibling frames are closed
      :nop

      (= :frame (get-in parent-cur-children [0 :type]))
      (call-frame-resized-hooks hook-man parent-cur-children)

      # :nop when the parent's current children are windows, since
      # in that case all sibling frames are closed too.
      )

    (:retile wm)
    (if cur-win
      (:activate wm cur-win)
      (:activate wm (:get-current-window root)))))


(defn cmd-frame-to-window-size [wm hook-man]
  (def cur-frame (:get-current-frame (in wm :root)))
  (def cur-win (:get-current-window cur-frame))
  (when (nil? cur-win)
    (break))

  (def win-rect
    (DwmGetWindowAttribute (in cur-win :hwnd) DWMWA_EXTENDED_FRAME_BOUNDS))

  (def border-space
    (combine-rect-border-space (:get-margins cur-win)
                               (:get-paddings cur-frame)))

  (def old-rect (in cur-frame :rect))
  (:resize cur-frame (expand-rect win-rect border-space))
  (check-for-frame-resized-hooks hook-man cur-frame old-rect)
  (:retile wm))


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
    (let [root (in wm :root)
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
        (with-activation-hooks wm
          (:close cur-frame)
          (:retile wm)
          (:activate wm (:get-current-window root)))))))


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
            (def rect (:get_CachedBoundingRectangle uia-win))
            (def efb-rect
              (try
                (DwmGetWindowAttribute hwnd DWMWA_EXTENDED_FRAME_BOUNDS)
                ((err fib)
                 (log/debug "DwmGetWindowAttribute failed for %n: %n" hwnd err)
                 nil)))
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
                                            "Filter Result: %n"]
                                           "\n")
                                          hwnd
                                          exe-path
                                          (:get_CachedName uia-win)
                                          (:get_CachedClassName uia-win)
                                          (:get_CachedControlType uia-win)
                                          (in desktop-info :name)
                                          (if-let [desktop-id (in desktop-info :id)]
                                            desktop-id
                                            "n/a")
                                          rect
                                          (if efb-rect
                                            (string/format "%n" efb-rect)
                                            "n/a")
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
      (:reset-visual-state win-found true false)
      (when pull?
        (def cur-frame (:get-current-frame (in wm :root)))
        (def cur-vd (in (:get-layout cur-frame) :id))
        (def win-vd (in (:get-virtual-desktop win-found) :id))
        # Only pull the window when it's in the same virtual desktop,
        # since the Windows API doesn't support moving windows across
        # virtual desktops.
        (when (= cur-vd win-vd)
          (:add-child cur-frame win-found)
          (:retile wm cur-frame)))
      (with-activation-hooks wm
        (:activate wm win-found)))
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
        (break))))

  (def repl (:get-default-server repl-man))

  (def argv (dyn :args))
  (def argv0 (first argv))
  (def env (os/environ))

  (def exe-path (string (QueryFullProcessImageName (GetCurrentProcess) 0)))
  (def [repl-host repl-port] (in repl :address))
  (def cmd-path (string (in env "SystemRoot") "\\system32\\cmd.exe"))

  (def repl-cli
    # Are we running from the source tree?
    (if (or (string/has-suffix? "/main.janet" argv0)
            (string/has-suffix? "\\main.janet" argv0))
      # Call cmd.exe to give the REPL client a new console
      [cmd-path "/c" "start" exe-path argv0 "-C" "--repl" (string/format "%s:%d" repl-host repl-port)]
      [cmd-path "/c" "start" exe-path "-C" "--repl" (string/format "%s:%d" repl-host repl-port)]))

  (log/debug "repl-cli = %n" repl-cli)
  (os/execute repl-cli))


(defn add-default-commands [command-man context]
  (def {:ui-manager ui-man
        :uia-manager uia-man
        :key-manager key-man
        :window-manager wm
        :hook-manager hook-man
        :repl repl}
    context)

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
     (fn [location &opt after-insertion-fn]
       (cmd-insert-frame wm hook-man location after-insertion-fn))
     ```
     (:insert-frame location &opt after-insertion-fn)

     Inserts a new frame and adjusts its sibling frames' sizes as needed.
     Location is relative to the current frame, can be :before or :after.
     After-insertion-fn is a function accepting the new inserted frame
     object as its sole argument, and it will be called after the insertion.
     If the current frame is a top-level frame, this command does nothing.
     ```)
  (:add-command command-man :flatten-parent
     (fn [] (cmd-flatten-parent wm))
     ```
     (:flatten-parent)

     Flattens the parent frame, by closing the current frame and all
     its siblings.
     ```)

  (:add-command command-man :resize-frame
     (fn [dw dh] (cmd-resize-frame wm hook-man dw dh))
     ```
     (:resize-frame dw dh)

     Resizes the current frame. Dw and dh are the deltas of width and
     height, respectively. Both of them are in physical pixels.
     ```)
  (:add-command command-man :close-frame
     (fn [] (cmd-close-frame wm hook-man))
     ```
     (:close-frame)

     Closes the current frame.
     ```)
  (:add-command command-man :frame-to-window-size
     (fn [] (cmd-frame-to-window-size wm hook-man))
     ```
     (:frame-to-window-size)

     Resizes the current frame, so that it's the same size as its
     current active window. Constraints from the parent frame apply.
     ```)
  (:add-command command-man :balance-frames
     (fn [] (cmd-balance-frames wm))
     ```
     (:balance-frames)

     Resizes all frames, so that all siblings belonging to the same
     parent have the same size.
     ```)
  (:add-command command-man :zoom-in
     (fn [ratio] (cmd-zoom-in wm hook-man ratio))
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
     :right, :up or :down.
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

  (:add-command command-man :ignore-window
     (fn [] (cmd-ignore-window wm ui-man))
     ```
     (:ignore-window)

     Removes the current window from the list of managed windows.
     ```))


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
