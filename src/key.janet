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
      (error (string/format "unknown key: %n" key))))

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


(def key-name-to-code
  {"lwin" VK_LWIN
   "rwin" VK_RWIN
   "lalt" VK_LMENU
   "ralt" VK_RMENU
   "lctrl" VK_LCONTROL
   "rctrl" VK_RCONTROL
   "lshift" VK_LSHIFT
   "rshift" VK_RSHIFT

   "enter" VK_RETURN
   "esc"   VK_ESCAPE

   "a" (ascii "A")
   "b" (ascii "B")
   "c" (ascii "C")
   "d" (ascii "D")
   "e" (ascii "E")
   "f" (ascii "F")
   "g" (ascii "G")
   "h" (ascii "H")
   "i" (ascii "I")
   "j" (ascii "J")
   "k" (ascii "K")
   "l" (ascii "L")
   "m" (ascii "M")
   "n" (ascii "N")
   "o" (ascii "O")
   "p" (ascii "P")
   "q" (ascii "Q")
   "r" (ascii "R")
   "s" (ascii "S")
   "t" (ascii "T")
   "u" (ascii "U")
   "v" (ascii "V")
   "w" (ascii "W")
   "x" (ascii "X")
   "y" (ascii "Y")
   "z" (ascii "Z")
   "0" (ascii "0")
   "1" (ascii "1")
   "2" (ascii "2")
   "3" (ascii "3")
   "4" (ascii "4")
   "5" (ascii "5")
   "6" (ascii "6")
   "7" (ascii "7")
   "8" (ascii "8")
   "9" (ascii "9")

   "f1" VK_F1
   "f2" VK_F2
   "f3" VK_F3
   "f4" VK_F4
   "f5" VK_F5
   "f6" VK_F6
   "f7" VK_F7
   "f8" VK_F8
   "f9" VK_F9
   "f10" VK_F10
   "f11" VK_F11
   "f12" VK_F12
   "f13" VK_F13
   "f14" VK_F14
   "f15" VK_F15
   "f16" VK_F16
   "f17" VK_F17
   "f18" VK_F18
   "f19" VK_F19
   "f20" VK_F20
   "f21" VK_F21
   "f22" VK_F22
   "f23" VK_F23
   "f24" VK_F24

   "," VK_OEM_COMMA
   "." VK_OEM_PERIOD
   "=" VK_OEM_PLUS
   ";" VK_OEM_1
   "/" VK_OEM_2
  })


# Only matches lower case key names. Do string/ascii-lower before
# matching against this PEG.
(def key-spec-peg
  (peg/compile
   ~{:win "win"
     :alt "alt"
     :ctrl "ctrl"
     :shift "shift"
     :mod (choice :win :alt :ctrl :shift)
     :mod-with-sides (sequence (set "lr") :mod)
     :mod-capture (replace (capture (choice :mod :mod-with-sides))
                           ,(fn [mod-str] (keyword mod-str)))
     :mod-prefix (sequence :mod-capture :s* (choice "+" "-") :s*)

     :f-key (sequence "f"
                      (choice (sequence "2" (range "04"))
                              (sequence "1" (range "09"))
                              (range "19")))
     :non-ascii-key (choice "enter" "esc" :f-key)

     :punctuation-key (choice "," "." "=" ";" "/")
     :ascii-key (choice (range "az" "09") :punctuation-key)

     :trigger-key (choice :mod-with-sides :non-ascii-key :ascii-key) # TODO
     :trigger-capture (replace (capture :trigger-key)
                               ,(fn [trig-str]
                                  (if-let [code (in key-name-to-code trig-str)]
                                    code
                                    (error (string/format "unknown key name: %n" trig-str)))))

     :combo-capture (group (sequence (group (any :mod-prefix)) :trigger-capture))
     :main (sequence (any (sequence :combo-capture :s+)) :combo-capture :s* -1)
    }))


(defn keymap-parse-key [self key-spec]
  (cond
    (string? key-spec)
    (if-let [matched (peg/match key-spec-peg (string/ascii-lower key-spec))]
      (map |(let [[mods key-code] $]
              (key key-code mods))
           matched)
      (error (string/format "failed to parse key spec: %n" key-spec)))

    true
    (error (string/format "unknown key spec: %n" key-spec))))


(defn keymap-define-key [self key-seq command-or-keymap]
  (if-not (indexed? key-seq)
    (if (string? key-seq)
      (break (keymap-define-key self (keymap-parse-key self key-seq) command-or-keymap))
      # A single key spec
      (break (keymap-define-key self [key-seq] command-or-keymap))))

  (def cur-key (in key-seq 0))
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


(defn keymap-get-root [self]
  (if-let [parent (in self :parent)]
    (keymap-get-root parent)
    self))


(def- keymap-proto
  @{:define-key keymap-define-key
    :parse-key keymap-parse-key
    :get-key-binding keymap-get-key-binding
    :get-root keymap-get-root})


(varfn define-keymap []
  (table/setproto (table/new 0) keymap-proto))


(defn key-manager-new-keymap [self]
  (define-keymap))


(defn key-manager-set-keymap [self keymap]
  (:set-keymap (in self :ui-manager) keymap))


(def- key-manager-proto
  @{:new-keymap key-manager-new-keymap
    :set-keymap key-manager-set-keymap})


(defn key-manager [ui-man]
  (table/setproto
   @{:ui-manager ui-man}
   key-manager-proto))


(defn keyboard-hook-handler-set-keymap [self keymap]
  (def to-set 
    (if (nil? keymap)
      (define-keymap)
      keymap))
  (put self :current-keymap to-set)
  (put self :keymap-stack @[]))


(defn keyboard-hook-handler-push-keymap [self keymap]
  (def new-keymap 
    (if (nil? keymap)
      (define-keymap)
      keymap))
  (array/push (in self :keymap-stack) (in self :current-keymap))
  (put self :current-keymap new-keymap))


(defn keyboard-hook-handler-pop-keymap [self]
  (def old-keymap (in self :current-keymap))
  (def new-keymap
    (if-let [stack-top (array/pop (in self :keymap-stack))]
      stack-top
      (define-keymap)))
  (put self :current-keymap new-keymap)
  old-keymap)


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
  (def cur-keymap (in self :current-keymap))
  (def root-keymap (:get-root cur-keymap))
  (put self :current-keymap root-keymap)
  (not= root-keymap cur-keymap))


(defn keyboard-hook-handler-handle-binding [self hook-struct binding]
  (def key-up (hook-struct :flags.up))

  (when (table? binding)
    # It's a sub-keymap, activate it only on key-up
    (if key-up
      (do
        (put self :current-keymap binding)
        (break [:key/switch-keymap binding]))
      (break nil)))

  (match binding
    [:push-keymap keymap]
    (when key-up
      (keyboard-hook-handler-push-keymap self keymap)
      [:key/push-keymap (in self :current-keymap)])

    :pop-keymap
    (when key-up
      (keyboard-hook-handler-pop-keymap self)
      [:key/pop-keymap (in self :current-keymap)])

    _
    # It's a normal command, only fire on key-down, and
    # try to reset to root keymap when key-up
    (if key-up
      (when (keyboard-hook-handler-reset-keymap self)
        [:key/reset-keymap (in self :current-keymap)])
      [:key/command binding])))


(defn keyboard-hook-handler-handle-unbound [self hook-struct]
  (def key-up (hook-struct :flags.up))
  # Reset the keymap on key-up, even for key combos we don't recognize.
  # We don't want modifier keys to reset the keymap, since this
  # will prevent the next key combo from having different modifiers.
  (when (and key-up
             (not (in MODIFIER-KEYS (in hook-struct :vkCode))))
    (log/debug "Resetting keymap")
    (when (keyboard-hook-handler-reset-keymap self)
      [:key/reset-keymap (in self :current-keymap)])))


(def- keyboard-hook-handler-proto
  @{:set-keymap keyboard-hook-handler-set-keymap
    :push-keymap keyboard-hook-handler-push-keymap
    :pop-keymap keyboard-hook-handler-pop-keymap
    :translate-key keyboard-hook-handler-translate-key
    :find-binding keyboard-hook-handler-find-binding
    :get-modifier-states keyboard-hook-handler-get-modifier-states
    :reset-keymap keyboard-hook-handler-reset-keymap
    :handle-binding keyboard-hook-handler-handle-binding
    :handle-unbound keyboard-hook-handler-handle-unbound})


(defn keyboard-hook-handler [keymap]
  (table/setproto
   @{:keymap-stack @[]
     :current-keymap keymap}
   keyboard-hook-handler-proto))
