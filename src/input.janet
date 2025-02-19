(use jw32/_winuser)


(def KEI-SHIFT-BITS 32)
(def KEI-BITS-MASK (blshift (int/u64 0xffff) KEI-SHIFT-BITS))
# Flags for key event dwExtraInfo. Other programes using low level
# keyboard hooks may use this field too, so we use higher bits to
# try to reduce conflicts.
(def KEI-FLAG-INJECTED (blshift (int/u64 1) KEI-SHIFT-BITS))
(def KEI-FLAG-REMAPPED (blshift (int/u64 2) KEI-SHIFT-BITS))
(def KEI-FLAG-SUPPRESS (blshift (int/u64 4) KEI-SHIFT-BITS))
(def KEI-FLAG-PASSTHROUGH (blshift (int/u64 8) KEI-SHIFT-BITS))

# Dummy key to suppress the start menu
# XXX: It's a reserved value in Windows headers
(def VK_DUMMY 0xff)


(defmacro test-kei-flag [flag extra-info]
  ~(> (band ,flag ,extra-info) (int/u64 0)))


(defn keyboard-input [key-code key-state &opt extra-info]
  (default extra-info 0)
  (def ei-to-send (bor KEI-FLAG-INJECTED extra-info))
  (INPUT :type INPUT_KEYBOARD
         :ki.wVk key-code
         :ki.dwFlags (case key-state
                       :up KEYEVENTF_KEYUP
                       :down 0)
         :ki.dwExtraInfo ei-to-send))


(defn mouse-button-input [btn-name btn-state &opt extra-info]
  (default extra-info 0)
  (def ei-to-send (bor KEI-FLAG-INJECTED extra-info))
  (def combo [btn-name btn-state])
  (INPUT :type INPUT_MOUSE
         :mi.dwFlags (match combo
                       [:left :up] MOUSEEVENTF_LEFTUP
                       [:left :down] MOUSEEVENTF_LEFTDOWN
                       [:right :up] MOUSEEVENTF_RIGHTUP
                       [:right :down] MOUSEEVENTF_RIGHTDOWN
                       [:middle :up] MOUSEEVENTF_MIDDLEUP
                       [:middle :down] MOUSEEVENTF_MIDDLEDOWN
                       [:x [_xid :up]] MOUSEEVENTF_XUP
                       [:x [_xid :down]] MOUSEEVENTF_XDOWN
                       [:wheel _amount] MOUSEEVENTF_WHEEL
                       [:hwheel _amount] MOUSEEVENTF_HWHEEL
                       (errorf "invalid btn-name and btn-state combo: %n" combo))
         :mi.mouseData (match combo
                         [:wheel amount] amount
                         [:hwheel amount] amount
                         [:x [xid _state]] xid
                         0)
         :mi.dwExtraInfo ei-to-send))


(defn send-input [& input-list]
  (SendInput input-list))
