(use jw32/_winbase)
(use jw32/_winuser)
(use jw32/_processthreadsapi)
(use jw32/_errhandlingapi)
(use jw32/_combaseapi)
(use jw32/_uiautomation)
(use jw32/_util)

(use ./win)
(use ./uia)
(use ./input)
(use ./resource)
(use ./util)

(import ./repl)
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
  (def win-stack (:get-window-stack frame))
  (def cur-rect (struct/to-table (:get-padded-rect frame)))

  (reverse! win-stack)
  (each win win-stack
    (def hwnd (in win :hwnd))
    (unless (or (= FALSE (IsWindow hwnd))
                (= FALSE (IsWindowVisible hwnd))
                (not= FALSE (IsIconic hwnd)))
      (:transform win cur-rect {:anchor :top-left})
      (+= (cur-rect :left) dx)
      (+= (cur-rect :top) dy)))
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


(defn cmd-resize-frame [wm dw dh]
  (def cur-frame (:get-current-frame (in wm :root)))
  (def rect (in cur-frame :rect))
  (:resize cur-frame
           {:left (in rect :left)
            :top (in rect :top)
            :right (+ dw (in rect :right))
            :bottom (+ dh (in rect :bottom))})
  (:retile wm))


(defn cmd-zoom-in [wm ratio]
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

  (def [cur-width cur-height]
    (rect-size (in cur-frame :rect)))

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
    (:retile wm)))


(defn cmd-balance-frames [wm]
  (def cur-frame (:get-current-frame (in wm :root)))
  (def cur-layout (:get-layout cur-frame))
  (each fr (in cur-layout :children)
    (:balance fr true))
  (:retile wm))


(defn cmd-close-frame [wm]
  (def root (in wm :root))
  (def cur-frame (:get-current-frame root))
  (def cur-win (:get-current-window cur-frame))
  (with-activation-hooks wm
    (:close cur-frame)
    (:retile wm)
    (if cur-win
      (:activate wm cur-win)
      (:activate wm (:get-current-window root)))))


(defn cmd-frame-to-window-size [wm uia-man]
  (def cur-frame (:get-current-frame (in wm :root)))
  (def cur-win (:get-current-window cur-frame))
  (when (nil? cur-win)
    (break))

  (def win-rect
    (:get-window-bounding-rect uia-man (in cur-win :hwnd)))
  (when (nil? win-rect)
    (break))

  (:resize cur-frame
           (-> win-rect
               (expand-rect (:get-margins cur-win))
               (expand-rect (:get-paddings cur-frame))))
  (:retile wm))


(defn cmd-close-window [wm ui-man]
  (with-uia [uia-win (:get-focused-window (in wm :uia-manager))]
    (if uia-win
      (with-uia [win-pat (:GetCurrentPatternAs uia-win
                                               UIA_WindowPatternId
                                               IUIAutomationWindowPattern)]
        (with-activation-hooks wm
          (:Close win-pat)))
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
        (:show-tooltip ui-man :close-window "No focused window.")))))


(defn cmd-close-window-or-frame [wm ui-man]
  (with-uia [uia-win (:get-focused-window (in wm :uia-manager))]
    (if uia-win
      (with-uia [win-pat (:GetCurrentPatternAs uia-win
                                               UIA_WindowPatternId
                                               IUIAutomationWindowPattern)]
        (with-activation-hooks wm
          (:Close win-pat)))
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
            (:activate wm (:get-current-window root))))))))


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
  (with-uia [uia-win (:get-focused-window (in wm :uia-manager))]
    (if (nil? uia-win)
      (:show-tooltip ui-man :describe-window "No focused window.")
      (do
        (def hwnd (:get_CachedNativeWindowHandle uia-win))
        (if-let [win-info (:get-hwnd-info wm hwnd uia-win)]
          (do
            (def {:exe-path exe-path
                  :virtual-desktop desktop-info}
              win-info)
            (def rect (:get_CachedBoundingRectangle uia-win))
            (:show-tooltip ui-man
                           :describe-window
                           (string/format (string/join
                                           ["HWND: %n"
                                            "EXE: %s"
                                            "Name: %s"
                                            "Class Name: %s"
                                            "Virtual Desktop Name: %s"
                                            "Virtual Desktop ID: %s"
                                            "Rect: %n"]
                                           "\n")
                                          hwnd
                                          exe-path
                                          (:get_CachedName uia-win)
                                          (:get_CachedClassName uia-win)
                                          (in desktop-info :name)
                                          (in desktop-info :id)
                                          rect)
                           (in rect :left)
                           (in rect :top)))
          (:show-tooltip ui-man
                         :describe-window
                         (string/format "Failed to get window info for %n." hwnd)))))))


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


(defn cmd-repl [context &opt start? host port]
  (default start? false)
  (default host const/DEFAULT-REPL-HOST)
  (default port const/DEFAULT-REPL-PORT)

  (def existing-repl (in context :repl))
  (def ui-man (in context :ui-manager))

  (when (nil? existing-repl)
    (if start?
      (do
        (:show-tooltip ui-man :repl "REPL is not running. Trying to start it...")
        (put context :repl
           {:server (repl/start-server context host port)
            :address @[host port]}))
      (do
        (:show-tooltip ui-man :repl "REPL is not running.")
        (break))))

  (def repl (in context :repl))

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
        :repl repl}
    context)

  (:add-command command-man :quit
     (fn [] (cmd-quit ui-man)))
  (:add-command command-man :retile
     (fn [] (cmd-retile wm)))

  (:add-command command-man :exec
     (fn [verbose? & cli]
       (cmd-exec wm ui-man verbose? cli)))
  (:add-command command-man :repl
     (fn [&opt start? host port]
       (cmd-repl context start? host port)))

  (:add-command command-man :split-frame
     (fn [dir &opt nfr ratios after-split-fn]
       (cmd-split-frame wm dir nfr ratios after-split-fn)))
  (:add-command command-man :flatten-parent
     (fn [] (cmd-flatten-parent wm)))

  (:add-command command-man :resize-frame
     (fn [dw dh] (cmd-resize-frame wm dw dh)))
  (:add-command command-man :close-frame
     (fn [] (cmd-close-frame wm)))
  (:add-command command-man :frame-to-window-size
     (fn [] (cmd-frame-to-window-size wm uia-man)))
  (:add-command command-man :balance-frames
     (fn [] (cmd-balance-frames wm)))
  (:add-command command-man :zoom-in
     (fn [ratio] (cmd-zoom-in wm ratio)))

  (:add-command command-man :enum-frame
     (fn [dir] (cmd-enum-frame wm dir)))
  (:add-command command-man :adjacent-frame
     (fn [dir] (cmd-adjacent-frame wm dir)))

  (:add-command command-man :enum-window-in-frame
     (fn [dir] (cmd-enum-window-in-frame wm dir)))
  (:add-command command-man :cascade-windows-in-frame
     (fn [&opt dx dy] (cmd-cascade-windows-in-frame wm dx dy)))

  (:add-command command-man :move-window
     (fn [dir] (cmd-move-window wm dir)))
  (:add-command command-man :close-window
     (fn [] (cmd-close-window wm ui-man)))
  (:add-command command-man :change-window-alpha
     (fn [delta] (cmd-change-window-alpha wm delta)))

  (:add-command command-man :close-window-or-frame
     (fn [] (cmd-close-window-or-frame wm ui-man)))

  (:add-command command-man :describe-window
     (fn [] (cmd-describe-window wm ui-man))))


(defn command-manager-call-command [self cmd & args]
  (def commands (in self :commands))
  (def hook-man (in self :hook-manager))
  (def found (in commands cmd))

  (cond
    (not found)
    (errorf "unknown command: %n, args: %n" cmd args)

    (:call-filter-hook hook-man :filter-command cmd args)
    (do
      (found ;args)
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


(defn command-manager-add-command [self name cmd-fn]
  (put (in self :commands) name cmd-fn))


(defn command-manager-remove-command [self name]
  (put (in self :commands) name nil))


(def- command-manager-proto
  @{:call-command command-manager-call-command
    :dispatch-command command-manager-dispatch-command
    :add-command command-manager-add-command
    :remove-command command-manager-remove-command})


(defn command-manager [hook-man]
  (table/setproto
   @{:commands @{}
     :hook-manager hook-man}
   command-manager-proto))
