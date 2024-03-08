(use jw32/winuser)

(use ./cmd)

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


(defn key-states-to-key-struct [hook-struct key-states mods-to-check]
  (def modifiers (keys (in key-states :tracked-modifiers @{})))
  (def ev-key-code (hook-struct :vkCode))
  (each key-code mods-to-check
    (when (and (not= ev-key-code key-code)
               (async-key-state-down? key-code))
      (array/push modifiers (in MODIFIER-KEYS key-code))))
  (key ev-key-code modifiers))


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


# If the keymap used Win key as a modifier, don't tell the OS
# about Win key events, or the Start menu would spontaneously pop up
(defn inhibit-win-key? [keymap]
  (var inhibit false)
  (each ks (keys keymap)
    (when (find |(or (= $ :lwin) (= $ :rwin)) (in ks :modifiers))
      (set inhibit true)
      (break)))
  inhibit)


(defn track-modifiers [key-code up key-states]
  (let [tracked-modifiers (in key-states :tracked-modifiers @{})
        mod-key-sym (in MODIFIER-KEYS key-code)]
    (if up
      (put tracked-modifiers mod-key-sym nil)
      (put tracked-modifiers mod-key-sym true))
    (put key-states :tracked-modifiers tracked-modifiers)))


(defn dispatch-raw-key-event [hook-struct chan]
  (ev/give chan [:key/raw-key-event
                 (hook-struct :vkCode)
                 (if (hook-struct :flags.up) :up :down)]))


(defn dispatch-key-event [keymap hook-struct chan inhibit-win-key key-states]
  (def mods-to-check
    (if inhibit-win-key
      (filter |(not (or (= $ VK_LWIN) (= $ VK_RWIN)))
              (keys MODIFIER-KEYS))
      (keys MODIFIER-KEYS)))
  (def key-struct (key-states-to-key-struct hook-struct key-states mods-to-check))
  (def key-code (key-struct :key))
  (def key-up (hook-struct :flags.up))

  (log/debug "key-struct = %n" key-struct)
  (log/debug "key-states = %n" key-states)

  (if-let [key-binding (get-key-binding keymap key-struct)]
    (if key-up
      (if (table? key-binding)
        [true key-binding]
        (do
          (ev/give chan [:key/key-event key-struct :up key-binding])
          [true (get-root-keymap keymap)]))
      (do
        (if-not (table? key-binding)
          (ev/give chan [:key/key-event key-struct :down key-binding]))
        [true keymap]))

    (cond
      (and inhibit-win-key
           (or (= VK_LWIN key-code)
               (= VK_RWIN key-code)))
      (do
        (track-modifiers key-code key-up key-states)
        (log/debug "new key-states = %n" key-states)
        [true keymap])

      (has-key? MODIFIER-KEYS key-code)
      [false keymap]

      (nil? (in keymap :parent))
      [false keymap]

      true
      # Not a modifier key, and not using the root keymap
      [true (if key-up
              (get-root-keymap keymap)
              keymap)])))


(defn process-key-event [key-struct key-state cmd context]
  (log/debug "################## process-key-event ##################")
  (log/debug "key-struct = %n" key-struct)
  (log/debug "key-state = %n" key-state)
  (log/debug "cmd = %n" cmd)
  (dispatch-command cmd key-struct key-state context))


(defn process-raw-key-event [key-code key-state keymap key-states inhibit-win-key]
  (log/debug "################## process-raw-key-event ##################")
  (log/debug "key-code = %n" key-code)
  (log/debug "key-state = %n" key-state)

  (def keys-down (in key-states :down @{}))
  (case key-state
    :up
    (put keys-down key-code nil)
    :down
    (put keys-down key-code true))
  (put key-states :down keys-down)
  (log/debug "keys-down = %n" keys-down)

  (def mods-to-check
    (if inhibit-win-key
      (filter |(not (or (= $ VK_LWIN) (= $ VK_RWIN)))
              (keys MODIFIER-KEYS))
      (keys MODIFIER-KEYS)))
  (def modifiers (keys (in key-states :tracked-modifiers @{})))
  (each mod-kc mods-to-check
    (when (and (not= key-code mod-kc)
               (in keys-down mod-kc))
      (array/push modifiers (in MODIFIER-KEYS mod-kc))))
  (def key-struct (key key-code modifiers))

  (log/debug "key-struct = %n" key-struct)

  (def [ret-cmd ret-keymap]
    (if-let [key-binding (get-key-binding keymap key-struct)]
      (case key-state
        :up
        (if (table? key-binding)
          [nil key-binding]
          (if (in key-states :keymap-triggered false)
            (do
              (put key-states :keymap-triggered false)
              [key-binding (get-root-keymap keymap)])
            [key-binding keymap]))

        :down
        (if-not (table? key-binding)
          (do
            (put key-states :keymap-triggered true)
            [key-binding keymap])
          [nil keymap]))

      (cond
        (and inhibit-win-key
             (or (= VK_LWIN key-code)
                 (= VK_RWIN key-code)))
        (do
          (track-modifiers key-code (case key-state :up true :down false) key-states)
          (log/debug "new key-states = %n" key-states)
          [nil keymap])

        (has-key? MODIFIER-KEYS key-code)
        [[:map-to key-code] keymap]

        (nil? (in keymap :parent))
        [[:map-to key-code] keymap]

        true
        # Not a modifier key, and not using the root keymap
        (if (= key-state :up)
          [nil (get-root-keymap keymap)]
          [nil keymap]))))

  [key-struct ret-cmd ret-keymap])
