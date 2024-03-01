(use jw32/winuser)


(defn show-error-and-exit [msg exit-code]
  (MessageBox nil
              (string/format "Error: %s" msg)
              "Error"
              (bor MB_ICONEXCLAMATION MB_OK))
  (os/exit exit-code))


