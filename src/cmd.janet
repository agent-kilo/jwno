(use jw32/_winuser)
(use jw32/_errhandlingapi)

(use ./win)
(use ./uia)
(use ./input)
(use ./resource)
(use ./util)

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


(defn cmd-enum-window-in-frame [wm dir]
  (def cur-frame (:get-current-frame (in wm :root)))
  (when-let [cur-win (:get-current-window cur-frame)]
    (def sibling (:enumerate-node cur-frame cur-win dir))
    (with-activation-hooks wm
      (:activate wm sibling))))


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
  (def cur-top (:get-top-frame cur-frame))
  (def mon-width (- (get-in cur-top [:rect :right]) (get-in cur-top [:rect :left])))
  (def mon-height (- (get-in cur-top [:rect :bottom]) (get-in cur-top [:rect :top])))
  (:resize cur-frame
           {:left 0
            :top 0
            :right (math/floor (* ratio mon-width))
            :bottom (math/floor (* ratio mon-height))})
  (:retile wm))


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

  (:resize cur-frame win-rect)
  (:retile wm))


(defn cmd-close-window [wm]
  (def cur-win (:get-current-window (in wm :root)))
  (when (nil? cur-win)
    (break))
  (with-activation-hooks wm
    (:close cur-win)))


(defn cmd-close-window-or-frame [wm]
  (def root (in wm :root))
  (def cur-frame (:get-current-frame root))
  (with-activation-hooks wm
    (if-let [cur-win (:get-current-window cur-frame)]
      (:close cur-win)
      (do
        (:close cur-frame)
        (:retile wm)
        (:activate wm (:get-current-window root))))))


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
      (:show-tooltip ui-man "No focused window." 0 0)
      (do
        (def hwnd (:get_CachedNativeWindowHandle uia-win))
        (if-let [win-info (:get-hwnd-info wm hwnd uia-win)]
          (do
            (def {:exe-path exe-path
                  :virtual-desktop desktop-info}
              win-info)
            (def rect (:get_CachedBoundingRectangle uia-win))
            (:show-tooltip ui-man
                           (string/format (string/join
                                           ["HWND: %n"
                                            "EXE: %s"
                                            "Name: %s"
                                            "Class Name: %s"
                                            "Virtual Desktop Name: %s"
                                            "Virtual Desktop ID: %s"]
                                           "\n")
                                          hwnd
                                          exe-path
                                          (:get_CachedName uia-win)
                                          (:get_CachedClassName uia-win)
                                          (in desktop-info :name)
                                          (in desktop-info :id))
                           (in rect :left)
                           (in rect :top)))
          (:show-tooltip ui-man
                         (string/format "Failed to get window info for %n." hwnd)
                         0 0))))))


(defn cmd-exec [wm ui-man verbose? cli]
  (def cur-frame (:get-current-frame (in wm :root)))
  (def mon-frame (:get-top-frame cur-frame))
  (def mon-rect (in mon-frame :rect))
  (def tt-x (in mon-rect :left))
  (def tt-y (in mon-rect :top))

  (when verbose?
    (:show-tooltip ui-man
                   (string/format "Running command: %n" cli)
                   tt-x tt-y))

  # ev/spawn So that waiting for the command won't block the main loop
  (ev/spawn
   (def env (os/environ))
   (put env :out :pipe)
   (put env :err :pipe)
   (when-let [proc (try
                     (os/spawn cli :ep env)
                     ((err fib)
                      (:show-tooltip ui-man
                                     (string/format "Failed to start command: %n\n%s\n%s"
                                                    cli
                                                    err
                                                    (get-stack-trace fib))
                                     tt-x tt-y)
                      nil))]
     # XXX: No limit on the output text
     (def out (:read (in proc :out) :all))
     (def err (:read (in proc :err) :all))
     (os/proc-wait proc)

     (def ret (in proc :return-code))
     (log/debug "Exit code of command %n: %n" cli ret)

     (when (not= 0 ret)
       (:show-tooltip ui-man
                      (string/format "Command: %n\nExit code %d:\n%s"
                                     cli
                                     ret
                                     (string out "\n" err))
                      tt-x tt-y)))))


(defn add-default-commands [command-man context]
  (def {:ui-manager ui-man
        :uia-manager uia-man
        :key-manager key-man
        :window-manager wm}
    context)

  (:add-command command-man :quit
     (fn [] (cmd-quit ui-man)))
  (:add-command command-man :retile
     (fn [] (cmd-retile wm)))

  (:add-command command-man :exec
     (fn [verbose? & cli]
       (cmd-exec wm ui-man verbose? cli)))

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

  (:add-command command-man :move-window
     (fn [dir] (cmd-move-window wm dir)))
  (:add-command command-man :close-window
     (fn [] (cmd-close-window wm)))
  (:add-command command-man :change-window-alpha
     (fn [delta] (cmd-change-window-alpha wm delta)))

  (:add-command command-man :close-window-or-frame
     (fn [] (cmd-close-window-or-frame wm)))

  (:add-command command-man :describe-window
     (fn [] (cmd-describe-window wm ui-man))))


(defn command-manager-call-command [self cmd & args]
  (def commands (in self :commands))
  (def found (in commands cmd))
  (if found
    (try
      (do
        (found ;args)
        true)
      ((err fib)
       (log/error "command %n failed: %n\n%s"
                  cmd
                  err
                  (get-stack-trace fib))
       false))
    (do
      (log/warning "unknown command: %n, args: %n" cmd args)
      false)))


(defn command-manager-dispatch-command [self cmd-and-args]
  (def call-with
    (if (indexed? cmd-and-args)
      cmd-and-args
      [cmd-and-args]))
  (command-manager-call-command self ;call-with))


(defn command-manager-add-command [self name cmd-fn]
  (put (in self :commands) name cmd-fn))


(defn command-manager-remove-command [self name]
  (put (in self :commands) name nil))


(def- command-manager-proto
  @{:call-command command-manager-call-command
    :dispatch-command command-manager-dispatch-command
    :add-command command-manager-add-command
    :remove-command command-manager-remove-command})


(defn command-manager []
  (table/setproto
   @{:commands @{}}
   command-manager-proto))
