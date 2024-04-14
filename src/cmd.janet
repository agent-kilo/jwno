(use jw32/_winuser)

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


(defn dispatch-command [cmd context]
  (match cmd
    :quit
    (cmd-quit context)

    :retile
    (cmd-retile context)

    [:split dir nfr ratios to-activate move-win-to]
    (cmd-split context dir nfr ratios to-activate move-win-to)

    :flatten-parent
    (cmd-flatten-parent context)

    [:enum-frame dir]
    (cmd-enum-frame context dir)

    [:adjacent-frame dir]
    (cmd-adjacent-frame context dir)

    :next-window-in-frame
    (cmd-next-window-in-frame context)

    :prev-window-in-frame
    (cmd-prev-window-in-frame context)

    [:move-current-window dir]
    (cmd-move-current-window context dir)

    [:resize-current-frame dw dh]
    (cmd-resize-current-frame context dw dh)

    [:focus-mode ratio]
    (cmd-focus-mode context ratio)

    :balance-frames
    (cmd-balance-frames context)

    :frame-to-current-window-size
    (cmd-frame-to-current-window-size context)

    _
    (log/warning "Unknown command: %n" cmd)))
