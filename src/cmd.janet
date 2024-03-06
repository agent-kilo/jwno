(use jw32/winuser)

(use ./resource)

(import ./log)


(defn cmd-quit [context]
  (if (in context :msg-hwnd)
    (PostMessage (in context :msg-hwnd) WM_COMMAND ID_MENU_EXIT 0)
    (os/exit 0)))


(defn cmd-send-keys [keys context]
  (let [inputs @[]]
    (each k keys
      (match k
        [key-code key-state]
        (array/push inputs
                    (INPUT :type INPUT_KEYBOARD
                           :ki.wVk key-code
                           :ki.dwFlags (if (= key-state :up)
                                         KEYEVENTF_KEYUP
                                         0)))

        key-code
        (do
          # Down event
          (array/push inputs
                      (INPUT :type INPUT_KEYBOARD
                             :ki.wVk key-code
                             :ki.dwFlags 0))
          # Up event
          (array/push inputs
                      (INPUT :type INPUT_KEYBOARD
                             :ki.wVk key-code
                             :ki.dwFlags KEYEVENTF_KEYUP)))

        _
        (log/warning "Unknown key spec: %n" k)))

    (SendInput inputs)))


(defn dispatch-command [cmd key-state context]
  (match cmd
    :quit
    (when (= key-state :down)
      (cmd-quit context))

    [:send-keys & keys]
    (when (= key-state :down)
      (cmd-send-keys keys context))

    _
    (log/warning "Unknown command: %n" cmd)))
