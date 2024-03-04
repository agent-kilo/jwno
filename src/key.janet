(use jw32/winuser)

(import ./log)


(defn ascii-key [ascii-str]
  (in ascii-str 0))


(defn key-struct [key &opt modifiers state]
  (default modifiers @[])
  (default state :down)
  {:key (int/u64 key)
   :state state
   :modifiers [;(sort modifiers)]})


(defn kbdllhs-to-key-struct [hook-struct modifiers]
  (key-struct (hook-struct :vkCode)
              modifiers
              (if (hook-struct :flags.up) :up :down)))


(defmacro- async-key-state-down? [vkey-code]
  ~(not= (,GetAsyncKeyState ,vkey-code) 0))


(defn key-states-to-key-struct [hook-struct]
  (def modifiers @[])
  (each [key-code key-sym]
      [[VK_LSHIFT :lshift]
       [VK_RSHIFT :rshift]
       [VK_LCONTROL :lctrl]
       [VK_RCONTROL :rctrl]
       [VK_LMENU :lalt]
       [VK_RMENU :ralt]
       [VK_LWIN :lwin]
       [VK_RWIN :rwin]]
    (when (async-key-state-down? key-code)
      (array/push modifiers key-sym)))
  (kbdllhs-to-key-struct hook-struct modifiers))


(defn get-key-binding [keymap key-struct]
  (in keymap key-struct))
