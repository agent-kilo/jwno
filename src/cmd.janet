(use jw32/winuser)

(import ./log)


(defn dispatch-command [cmd]
  (match cmd
    [:send-keys & keys]
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

      (SendInput inputs))

    _
    (log/warning "Unknown command: %n" cmd)))
