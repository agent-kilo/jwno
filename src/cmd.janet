(use jw32/winuser)

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
  (:retile (in context :wm)))


(defn cmd-hsplit [context]
  (def cur-frame (:get-current-frame (in context :wm)))
  (:split cur-frame :horizontal 2 [0.5])
  (:activate (get-in cur-frame [:children 0]))
  (:retile (in context :wm)))


(defn cmd-next-frame [context]
  (def cur-frame (:get-current-frame (in context :wm)))
  (when-let [sibling (:next-sibling cur-frame)]
    (:activate (in context :wm) sibling)))


(defn cmd-next-window-in-frame [context]
  (when-let [cur-win (:get-current-window (in context :wm))]
    (when-let [sibling (:next-sibling cur-win)]
      (:activate (in context :wm) sibling))))


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

    :hsplit
    (when (= key-state :down)
      (cmd-hsplit context))

    :next-frame
    (when (= key-state :down)
      (cmd-next-frame context))

    :next-window-in-frame
    (when (= key-state :down)
      (cmd-next-window-in-frame context))

    _
    (log/warning "Unknown command: %n" cmd)))
