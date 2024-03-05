(use jw32/winuser)

(import ./log)


(defn ascii-key [ascii-str]
  (in ascii-str 0))


(defn key-struct [key &opt modifiers]
  (default modifiers @[])
  {:key (int/u64 key)
   :modifiers [;(sort modifiers)]})


(defn kbdllhs-to-key-struct [hook-struct modifiers]
  (key-struct (hook-struct :vkCode) modifiers))


(defmacro- async-key-state-down? [vkey-code]
  ~(not= (,GetAsyncKeyState ,vkey-code) 0))


(def MODIFIER-KEYS
  {(int/u64 VK_LSHIFT) :lshift
   (int/u64 VK_RSHIFT) :rshift
   (int/u64 VK_LCONTROL) :lctrl
   (int/u64 VK_RCONTROL) :rctrl
   (int/u64 VK_LMENU) :lalt
   (int/u64 VK_RMENU) :ralt
   (int/u64 VK_LWIN) :lwin
   (int/u64 VK_RWIN) :rwin})


(defn key-states-to-key-struct [hook-struct]
  (def modifiers @[])
  (each key-code
      [VK_LSHIFT
       VK_RSHIFT
       VK_LCONTROL
       VK_RCONTROL
       VK_LMENU
       VK_RMENU
       VK_LWIN
       VK_RWIN]
    (when (async-key-state-down? key-code)
      (array/push modifiers (in MODIFIER-KEYS (int/u64 key-code)))))
  (kbdllhs-to-key-struct hook-struct modifiers))


(defn define-keymap []
  (table/new 0))


(defn set-key-def [keymap key-struct command-or-keymap]
  (if (table? command-or-keymap)
    (let [sub-keymap command-or-keymap]
      (put sub-keymap :parent keymap)
      (put keymap key-struct sub-keymap))
    (let [command command-or-keymap]
      (put keymap key-struct command))))


(defn define-key [keymap key-seq command-or-keymap]
  (def cur-key (in key-seq 0))
  (def rest-keys (slice key-seq 1))
  (def cur-def (get keymap cur-key))
  (if (<= (length rest-keys) 0)
    (set-key-def keymap cur-key command-or-keymap)
    (let [sub-keymap (if (table? cur-def)
                       cur-def
                       (define-keymap))]
      (set-key-def keymap
                   cur-key
                   (define-key
                     (define-keymap)
                     rest-keys
                     command-or-keymap))
      keymap)))


(defn get-key-binding [keymap key-struct]
  (in keymap key-struct))


(defn get-root-keymap [keymap]
  (if-let [parent (in keymap :parent)]
    (get-root-keymap parent)
    keymap))


(defn handle-key-event [keymap hook-struct chan]
  (def key-struct (key-states-to-key-struct hook-struct))
  (log/debug "key-struct = %n" key-struct)

  (if-let [key-binding (get-key-binding keymap key-struct)]
    (if (hook-struct :flags.up)
      (if (table? key-binding)
        [true key-binding]
        [true (get-root-keymap keymap)])
      (do
        (if-not (table? key-binding)
          (ev/give chan [:key/key-event key-struct key-binding]))
        [true keymap]))

    (cond
      (has-key? MODIFIER-KEYS (hook-struct :vkCode))
      [false keymap]

      (nil? (in keymap :parent))
      [false keymap]

      true
      # Not a modifier key, and not using the root keymap
      [true (if (hook-struct :flags.up)
              (get-root-keymap keymap)
              keymap)])))
