(use jw32/_winuser)
(use jw32/_errhandlingapi)

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


(defn cmd-split-frame [wm dir nfr ratios to-activate move-win-to]
  (def cur-frame (:get-current-frame (in wm :root)))
  (def cur-win (:get-current-window cur-frame))
  (:split cur-frame dir nfr ratios)
  (when (and (> move-win-to 0) (< move-win-to nfr))
    (def move-to-fr (get-in cur-frame [:children move-win-to]))
    (each w (slice (in (:get-first-frame cur-frame) :children))
      (:add-child move-to-fr w)))
  (:retile wm cur-frame)
  # Do not actually focus this window, just mark it as activated
  (when cur-win
    (:activate cur-win))
  (:activate wm (get-in cur-frame [:children to-activate])))


(defn cmd-flatten-parent [wm]
  (def cur-frame (:get-current-frame (in wm :root)))
  (def parent (in cur-frame :parent))
  (cond
    (nil? parent)
    (break)

    (not= :frame (in parent :type))
    (break)

    true
    (do
      (:flatten parent)
      (:retile wm parent)
      (:activate wm (:get-current-window parent)))))


(defn cmd-enum-frame [wm dir]
  (def cur-frame (:get-current-frame (in wm :root)))
  (when-let [fr (:enumerate-node (:get-layout cur-frame) cur-frame dir)]
    (:activate wm fr)))


(defn cmd-adjacent-frame [wm dir]
  (def cur-frame (:get-current-frame (in wm :root)))
  (when-let [adj-fr (:get-adjacent-frame cur-frame dir)]
    (:activate wm adj-fr)))


(defn cmd-enum-window-in-frame [wm dir]
  (def cur-frame (:get-current-frame (in wm :root)))
  (when-let [cur-win (:get-current-window cur-frame)]
    (def sibling (:enumerate-node cur-frame cur-win dir))
    (:activate wm sibling)))


(defn cmd-move-window [wm dir]
  (def cur-frame (:get-current-frame (in wm :root)))
  (def cur-win (:get-current-window cur-frame))

  (when (nil? cur-win) (break))

  (when-let [adj-fr (:get-adjacent-frame cur-frame dir)]
    (:add-child adj-fr cur-win)
    (:retile wm adj-fr)
    (:activate wm cur-win)))


(defn cmd-resize-frame [wm dw dh]
  (def cur-frame (:get-current-frame (in wm :root)))
  (def rect (in cur-frame :rect))
  (:resize cur-frame
           {:left (in rect :left)
            :top (in rect :top)
            :right (+ dw (in rect :right))
            :bottom (+ dh (in rect :bottom))})
  (def cur-win (:get-current-window cur-frame))
  (:retile wm)
  (:activate wm cur-win))


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
  (def cur-win (:get-current-window cur-frame))
  (:retile wm)
  (:activate wm cur-win))


(defn cmd-balance-frames [wm]
  (def cur-frame (:get-current-frame (in wm :root)))
  (def cur-layout (:get-layout cur-frame))
  (each fr (in cur-layout :children)
    (:balance fr true))
  (def cur-win (:get-current-window cur-frame))
  (:retile wm)
  (:activate wm cur-win))


(defn cmd-close-frame [wm]
  (def cur-frame (:get-current-frame (in wm :root)))
  (def cur-win (:get-current-window cur-frame))
  (:close cur-frame)
  (:retile wm)
  (if cur-win
    (:activate wm cur-win)
    (:activate wm (:get-current-window cur-frame))))


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
  (:retile wm)
  (:activate wm cur-win))


(defn cmd-close-window [wm]
  (def cur-win (:get-current-window (in wm :root)))
  (when (nil? cur-win)
    (break))
  (:close cur-win))


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

  (:add-command command-man :split-frame
     (fn [dir nfr ratios to-activate move-win-to]
       (cmd-split-frame wm dir nfr ratios to-activate move-win-to)))
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
     (fn [delta] (cmd-change-window-alpha wm delta))))


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
