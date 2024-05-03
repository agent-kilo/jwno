(use jw32/_winuser)

(use ./cmd)
(use ./input)

(import ./log)


(defn ascii [ascii-str]
  (in ascii-str 0))


(defn key [key &opt modifiers]
  (default modifiers [])

  (def normalized-key
    (cond
      (number? key)
      key

      (or (keyword? key) (symbol? key))
      (eval (symbol "VK_" (string/ascii-upper (string/replace-all "-" "_" key))))

      (string? key)
      (ascii (string/ascii-upper key))

      true
      (error (string/format "unknown key spec: %n" key))))

  {:key normalized-key
   :modifiers [;(sort (array ;modifiers))]})


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


(defn- set-key-def [keymap key-struct command-or-keymap]
  (if (table? command-or-keymap)
    (let [sub-keymap command-or-keymap]
      (put sub-keymap :parent keymap)
      (put keymap key-struct sub-keymap))
    (let [command command-or-keymap]
      (put keymap key-struct command))))


(var define-keymap nil) # Forward declaration


(defn keymap-parse-key [self key-def]
  (cond
    (struct? key-def)
    key-def

    (indexed? key-def)
    (key ;key-def)

    (or (keyword? key-def)
        (symbol? key-def)
        (string? key-def))
    (key key-def)

    true
    (error (string/format "unknown key def: %n" key-def))))


(defn keymap-define-key [self key-seq command-or-keymap]
  (cond
    (not (indexed? key-seq))
    # A single key struct
    (break (keymap-define-key self [key-seq] command-or-keymap))

    (or (keyword? (in key-seq 0))
        (symbol? (in key-seq 0))
        (string? (in key-seq 0)))
    # A single key struct in indexed form
    (break (keymap-define-key self [key-seq] command-or-keymap)))

  (def cur-key (keymap-parse-key self (in key-seq 0)))
  (def rest-keys (slice key-seq 1))
  (def cur-def (get self cur-key))

  (if (<= (length rest-keys) 0)
    (set-key-def self cur-key command-or-keymap)
    (let [sub-keymap (if (table? cur-def)
                       cur-def
                       (define-keymap))]
      (set-key-def self
                   cur-key
                   (keymap-define-key
                     sub-keymap
                     rest-keys
                     command-or-keymap))
      self)))


(defn keymap-get-key-binding [self key-struct]
  (in self key-struct))


(def- keymap-proto
  @{:define-key keymap-define-key
    :parse-key keymap-parse-key
    :get-key-binding keymap-get-key-binding})


(varfn define-keymap []
  (table/setproto (table/new 0) keymap-proto))


(defn keyboard-hook-handler-translate-key [self hook-struct]
  (def extra-info (in hook-struct :dwExtraInfo))
  (when (test-kei-flag KEI-FLAG-REMAPPED extra-info)
    # Already remapped
    (break nil))

  (if-let [binding (:get-key-binding (in self :current-keymap)
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
    (if-let [found (:get-key-binding (in self :current-keymap) key-struct)]
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
