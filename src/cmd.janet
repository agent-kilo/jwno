(use jw32/_winuser)

(use ./resource)

(import ./log)


(defn cmd-quit [context]
  (if (in context :msg-hwnd)
    (PostMessage (in context :msg-hwnd) WM_COMMAND ID_MENU_EXIT 0)
    (os/exit 0)))


(defn cmd-map-to [key-code key-state]
  (def input
    (INPUT :type INPUT_KEYBOARD
           :ki.wVk key-code
           :ki.dwFlags (case key-state
                         :up KEYEVENTF_KEYUP
                         :down 0)))
  (SendInput [input]))


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
        (array/push cur-inputs
                    (INPUT :type INPUT_KEYBOARD
                           :ki.wVk key-code
                           :ki.dwFlags (if (= key-state :up)
                                         KEYEVENTF_KEYUP
                                         0)))

        key-code
        (do
          # Down event
          (array/push cur-inputs
                      (INPUT :type INPUT_KEYBOARD
                             :ki.wVk key-code
                             :ki.dwFlags 0))
          # Up event
          (array/push cur-inputs
                      (INPUT :type INPUT_KEYBOARD
                             :ki.wVk key-code
                             :ki.dwFlags KEYEVENTF_KEYUP)))

        _
        (log/warning "Unknown key spec: %n" k)))

    (log/debug "input-seqs = %n" input-seqs)

    (each seq input-seqs
      (if (number? seq)
        (ev/sleep seq)
        (SendInput seq)))))


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
  (:resize cur-frame {:left (in rect :left)
                      :top (in rect :top)
                      :right (+ dw (in rect :right))
                      :bottom (+ dh (in rect :bottom))})
  (def cur-win (:get-current-window cur-frame))
  (:retile wm)
  (:activate wm cur-win))


(defn cmd-focus-mode [context ratio]
  (def wm (in context :wm))
  (def cur-monitor (get-in wm [:layout :current-child]))
  (def mon-width (- (get-in cur-monitor [:rect :right]) (get-in cur-monitor [:rect :left])))
  (def mon-height (- (get-in cur-monitor [:rect :bottom]) (get-in cur-monitor [:rect :top])))
  (def cur-frame (:get-current-frame cur-monitor))
  (:resize cur-frame {:left 0
                      :top 0
                      :right (math/floor (* ratio mon-width))
                      :bottom (math/floor (* ratio mon-height))})
  (def cur-win (:get-current-window cur-frame))
  (:retile wm)
  (:activate wm cur-win))


(defn dispatch-command [cmd key-struct key-state context]
  (match cmd
    :quit
    (when (= key-state :down)
      (cmd-quit context))

    [:map-to key-code]
    (cmd-map-to key-code key-state)

    [:send-keys & keys]
    (when (= key-state :down)
      (cmd-send-keys keys context))

    :retile
    (when (= key-state :down)
      (cmd-retile context))

    [:split dir nfr ratios to-activate move-win-to]
    (when (= key-state :down)
      (cmd-split context dir nfr ratios to-activate move-win-to))

    :flatten-parent
    (when (= key-state :down)
      (cmd-flatten-parent context))

    [:enum-frame dir]
    (when (= key-state :down)
      (cmd-enum-frame context dir))

    [:adjacent-frame dir]
    (when (= key-state :down)
      (cmd-adjacent-frame context dir))

    :next-window-in-frame
    (when (= key-state :down)
      (cmd-next-window-in-frame context))

    :prev-window-in-frame
    (when (= key-state :down)
      (cmd-prev-window-in-frame context))

    [:move-current-window dir]
    (when (= key-state :down)
      (cmd-move-current-window context dir))

    [:resize-current-frame dw dh]
    (when (= key-state :down)
      (cmd-resize-current-frame context dw dh))

    [:focus-mode ratio]
    (when (= key-state :down)
      (cmd-focus-mode context ratio))

    _
    (log/warning "Unknown command: %n" cmd)))
