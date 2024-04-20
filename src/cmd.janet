(use jw32/_winuser)
(use jw32/_errhandlingapi)

(use ./input)
(use ./resource)

(import ./log)


(defn cmd-quit [context]
  (try
    (:destroy (in context :ui))
    ((err fib)
     (log/warning "Failed to destroy UI thread: %n" err)
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


(defn cmd-retile [context]
  (def wm (in context :wm))
  (def cur-win (:get-current-window (in wm :layout)))
  (:retile wm)
  (:activate wm cur-win))


(defn cmd-split [context dir nfr ratios to-activate move-win-to]
  (def wm (in context :wm))
  (def cur-frame (:get-current-frame (in wm :layout)))
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


(defn cmd-flatten-parent [context]
  (def cur-frame (:get-current-frame (get-in context [:wm :layout])))
  (def parent (in cur-frame :parent))
  (cond
    (nil? parent)
    (break)

    (not= :frame (in parent :type))
    (break)

    true
    (do
      (def wm (in context :wm))
      (:flatten parent)
      (:retile wm parent)
      (:activate wm (:get-current-window parent)))))


(defn cmd-enum-frame [context dir]
  (def wm (in context :wm))
  (def cur-frame (:get-current-frame (in wm :layout)))
  (when-let [fr (:enumerate-frame (in wm :layout) cur-frame dir)]
    (:activate wm fr)))


(defn cmd-adjacent-frame [context dir]
  (def wm (in context :wm))
  (def cur-frame (:get-current-frame (in wm :layout)))
  (when-let [adj-fr (:get-adjacent-frame (in wm :layout) cur-frame dir)]
    (:activate wm adj-fr)))


(defn cmd-next-window-in-frame [context]
  (def wm (in context :wm))
  (def cur-frame (:get-current-frame (in wm :layout)))
  (:purge-windows cur-frame)
  (when-let [cur-win (:get-current-window cur-frame)]
    (when-let [sibling (:get-next-sibling cur-win)]
      (:activate wm sibling))))


(defn cmd-prev-window-in-frame [context]
  (def wm (in context :wm))
  (def cur-frame (:get-current-frame (in wm :layout)))
  (:purge-windows cur-frame)
  (when-let [cur-win (:get-current-window cur-frame)]
    (when-let [sibling (:get-prev-sibling cur-win)]
      (:activate wm sibling))))


(defn cmd-move-current-window [context dir]
  (def wm (in context :wm))
  (def cur-frame (:get-current-frame (in wm :layout)))
  (def cur-win (:get-current-window cur-frame))

  (when (nil? cur-win) (break))

  (when-let [adj-fr (:get-adjacent-frame (in wm :layout) cur-frame dir)]
    (:add-child adj-fr cur-win)
    (:retile wm adj-fr)
    (:activate wm cur-win)))


(defn cmd-resize-current-frame [context dw dh]
  (def wm (in context :wm))
  (def cur-frame (:get-current-frame (in wm :layout)))
  (def rect (in cur-frame :rect))
  (:resize-frame (in wm :layout)
                 cur-frame
                 {:left (in rect :left)
                  :top (in rect :top)
                  :right (+ dw (in rect :right))
                  :bottom (+ dh (in rect :bottom))})
  (def cur-win (:get-current-window cur-frame))
  (:retile wm)
  (:activate wm cur-win))


(defn cmd-focus-mode [context ratio]
  (def wm (in context :wm))
  (def cur-monitor (get-in wm [:layout :current-child]))
  (def cur-frame (:get-current-frame cur-monitor))
  (when (= (in cur-frame :parent) (in wm :layout))
    # It's a toplevel frame, which can't be resized
    (break))

  (def mon-width (- (get-in cur-monitor [:rect :right]) (get-in cur-monitor [:rect :left])))
  (def mon-height (- (get-in cur-monitor [:rect :bottom]) (get-in cur-monitor [:rect :top])))
  (:balance-frames (in wm :layout) (in cur-frame :parent))
  (:resize-frame (in wm :layout)
                 cur-frame
                 {:left 0
                  :top 0
                  :right (math/floor (* ratio mon-width))
                  :bottom (math/floor (* ratio mon-height))})
  (def cur-win (:get-current-window cur-frame))
  (:retile wm)
  (:activate wm cur-win))


(defn cmd-balance-frames [context]
  (def wm (in context :wm))
  (:balance-frames (in wm :layout) nil true)
  (def cur-win (:get-current-window (in wm :layout)))
  (:retile wm)
  (:activate wm cur-win))


(defn cmd-frame-to-current-window-size [context]
  (def wm (in context :wm))
  (def cur-frame (:get-current-frame (in wm :layout)))
  (def cur-win (:get-current-window cur-frame))
  (when (nil? cur-win)
    (break))

  (def win-rect
    (:get-window-bounding-rect (in context :uia)
                               (in cur-win :hwnd)))
  (when (nil? win-rect)
    (break))

  (:resize-frame (in wm :layout) cur-frame win-rect)
  (:retile wm)
  (:activate wm cur-win))


(defn cmd-close-current-window [context]
  (def wm (in context :wm))
  (def cur-win (:get-current-window (in wm :layout)))
  (when (nil? cur-win)
    (break))

  (PostMessage (in cur-win :hwnd) WM_CLOSE 0 0))


(defn cmd-change-current-window-alpha [context delta]
  (def wm (in context :wm))
  (def cur-win (:get-current-window (in wm :layout)))
  (when (nil? cur-win)
    (break))

  (def old-alpha
    (if-let [attrs (GetLayeredWindowAttributes (in cur-win :hwnd))]
      (in attrs 1)
      (do
        (log/debug "GetLayeredWindowAttributes failed: %n" (GetLastError))
        255)))
  (def new-alpha
    (let [val (math/floor (+ old-alpha delta))]
      (cond
        (< val 0) 0
        (> val 255) 255
        true val)))
  (log/debug "Setting window alpha from %n to %n" old-alpha new-alpha)
  (:set-window-alpha wm (in cur-win :hwnd) new-alpha))


(defn add-default-commands [command-man context]
  (:add-command command-man :quit
     (fn [] (cmd-quit context)))
  (:add-command command-man :retile
     (fn [] (cmd-retile context)))

  (:add-command command-man :split
     (fn [dir nfr ratios to-activate move-win-to]
       (cmd-split context dir nfr ratios to-activate move-win-to)))
  (:add-command command-man :flatten-parent
     (fn [] (cmd-flatten-parent context)))

  (:add-command command-man :resize-current-frame
     (fn [dw dh] (cmd-resize-current-frame context dw dh)))
  (:add-command command-man :frame-to-current-window-size
     (fn [] (cmd-frame-to-current-window-size context)))
  (:add-command command-man :balance-frames
     (fn [] (cmd-balance-frames context)))
  (:add-command command-man :focus-mode
     (fn [ratio] (cmd-focus-mode context ratio)))

  (:add-command command-man :enum-frame
     (fn [dir] (cmd-enum-frame context dir)))
  (:add-command command-man :adjacent-frame
     (fn [dir] (cmd-adjacent-frame context dir)))

  (:add-command command-man :next-window-in-frame
     (fn [] (cmd-next-window-in-frame context)))
  (:add-command command-man :prev-window-in-frame
     (fn [] (cmd-prev-window-in-frame context)))

  (:add-command command-man :move-current-window
     (fn [dir] (cmd-move-current-window context dir)))
  (:add-command command-man :close-current-window
     (fn [] (cmd-close-current-window context)))
  (:add-command command-man :change-current-window-alpha
     (fn [delta] (cmd-change-current-window-alpha context delta))))


(defn command-manager-call-command [self cmd & args]
  (def commands (in self :commands))
  (def found (in commands cmd))
  (if found
    (try
      (found ;args)
      ((err fib)
       (log/error "command %n failed: %n" cmd err)))
    (log/warning "unknown command: %n, args: %n" cmd args)))


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
