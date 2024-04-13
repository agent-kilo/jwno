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


(defn send-input [& input-list]
  (SendInput input-list))
