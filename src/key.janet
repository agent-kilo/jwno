(use jw32/_winuser)

(use ./cmd)
(use ./input)

(import ./log)


(defn ascii [ascii-str]
  (in ascii-str 0))


(defn key [key &opt modifiers]
  (default modifiers @[])
  {:key key
   :modifiers [;(sort modifiers)]})


(defmacro- async-key-state-down? [vkey-code]
  ~(not= (,GetAsyncKeyState ,vkey-code) 0))


(def MODIFIER-KEYS
  {VK_LSHIFT :lshift
   VK_RSHIFT :rshift
   VK_LCONTROL :lctrl
   VK_RCONTROL :rctrl
   VK_LMENU :lalt
   VK_RMENU :ralt
   VK_LWIN :lwin
   VK_RWIN :rwin})


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
  (if-not (indexed? key-seq)
    (break (define-key keymap [key-seq] command-or-keymap)))

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
                     sub-keymap
                     rest-keys
                     command-or-keymap))
      keymap)))


(defn get-key-binding [keymap key-struct]
  (in keymap key-struct))


(defn get-root-keymap [keymap]
  (if-let [parent (in keymap :parent)]
    (get-root-keymap parent)
    keymap))


(defn keyboard-hook-handler-translate-key [self hook-struct]
  (def extra-info (in hook-struct :dwExtraInfo))
  (when (test-kei-flag KEI-FLAG-REMAPPED extra-info)
    # Already remapped
    (break nil))

  (if-let [binding (get-key-binding (in self :current-keymap)
                                    (key (hook-struct :vkCode)))]
    (match binding
      [:map-to new-key]
      new-key

      _
      nil)))


(defn keyboard-hook-handler-get-modifier-states [self hook-struct]
  (def current-kc (in hook-struct :vkCode))
  (def states @{})
  (each kc (keys MODIFIER-KEYS)
    (when (and (not= kc current-kc) # Special case when only modifiers are pressed
               (async-key-state-down? kc))
      (put states (in MODIFIER-KEYS kc) true)))
  states)


(defn keyboard-hook-handler-find-binding [self hook-struct mod-keys]
  (def mod-combinations-to-check @[mod-keys])
  (each [mod lmod rmod] [[:shift :lshift :rshift]
                         [:ctrl :lctrl :rctrl]
                         [:alt :lalt :ralt]
                         [:win :lwin :rwin]]
    (each state (slice mod-combinations-to-check)
      (when (or (in state lmod)
                (in state rmod))
        (def comb (table/clone state))
        (put comb lmod nil)
        (put comb rmod nil)
        (put comb mod true)
        (array/push mod-combinations-to-check comb))))

  (log/debug "mod-combinations-to-check = %n" mod-combinations-to-check)

  (var binding nil)
  (each comb mod-combinations-to-check
    (def key-struct (key (hook-struct :vkCode) (sort (keys comb))))
    (log/debug "Finding binding for key: %n" key-struct)
    (if-let [found (get-key-binding (in self :current-keymap) key-struct)]
      (match found
        [:map-to _]
        # handled in translate-key
        nil

        _
        (do
          (set binding found)
          (break)))))
  binding)


(defn keyboard-hook-handler-reset-keymap [self]
  (var cur-keymap (in self :current-keymap))
  (var parent-keymap (in cur-keymap :parent))
  (while parent-keymap
    (set cur-keymap parent-keymap)
    (set parent-keymap (in cur-keymap :parent)))
  (def old-keymap (in self :current-keymap))
  (put self :current-keymap cur-keymap)
  (not= old-keymap cur-keymap))


(defn keyboard-hook-handler-handle-binding [self hook-struct binding]
  (def key-up (hook-struct :flags.up))

  (when (table? binding)
    # It's a sub-keymap, activate it only on key-up
    (if key-up
      (do
        (put self :current-keymap binding)
        (break [:key/switch-keymap binding]))
      (break nil)))

  # It's a normal command, only fire on key-down, and
  # try to reset to root keymap when key-up
  (if key-up
    (when (keyboard-hook-handler-reset-keymap self)
      [:key/reset-keymap (in self :current-keymap)])
    [:key/command binding]))


(def- keyboard-hook-handler-proto
  @{:translate-key keyboard-hook-handler-translate-key
    :find-binding keyboard-hook-handler-find-binding
    :get-modifier-states keyboard-hook-handler-get-modifier-states
    :reset-keymap keyboard-hook-handler-reset-keymap
    :handle-binding keyboard-hook-handler-handle-binding})


(defn keyboard-hook-handler [keymap]
  (table/setproto
   @{:current-keymap keymap}
   keyboard-hook-handler-proto))
