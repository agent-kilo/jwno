(use jw32/_winuser)
(use ../src/key)


(def empty-keymap (define-keymap))


(defn test-key-states []
  (def key-states @{})

  (process-raw-key-event (ascii "A") :down empty-keymap key-states false)
  (assert (get-in key-states [:down (ascii "A")]))

  (process-raw-key-event (ascii "A") :up empty-keymap key-states false)
  (assert (nil? (get-in key-states [:down (ascii "A")])))

  (process-raw-key-event VK_LCONTROL :down empty-keymap key-states false)
  (process-raw-key-event (ascii "A") :down empty-keymap key-states false)
  (assert (get-in key-states [:down VK_LCONTROL]))
  (assert (get-in key-states [:down (ascii "A")]))

  (process-raw-key-event (ascii "A") :up empty-keymap key-states false)
  (process-raw-key-event VK_LCONTROL :up empty-keymap key-states false)
  (assert (nil? (get-in key-states [:down VK_LCONTROL])))
  (assert (nil? (get-in key-states [:down (ascii "A")]))))


(defn test-tracked-modifiers []
  (def key-states @{})

  (track-modifiers VK_LWIN false key-states)
  (assert (get-in key-states [:tracked-modifiers :lwin]))

  (track-modifiers VK_LWIN true key-states)
  (assert (nil? (get-in key-states [:tracked-modifiers :lwin])))

  (track-modifiers VK_LCONTROL false key-states)
  (track-modifiers VK_LMENU false key-states)
  (track-modifiers VK_LSHIFT false key-states)
  (assert (get-in key-states [:tracked-modifiers :lctrl]))
  (assert (get-in key-states [:tracked-modifiers :lalt]))
  (assert (get-in key-states [:tracked-modifiers :lshift]))

  (track-modifiers VK_RCONTROL false key-states)
  (track-modifiers VK_RMENU false key-states)
  (track-modifiers VK_RSHIFT false key-states)
  (assert (get-in key-states [:tracked-modifiers :rctrl]))
  (assert (get-in key-states [:tracked-modifiers :ralt]))
  (assert (get-in key-states [:tracked-modifiers :rshift]))

  (track-modifiers VK_LCONTROL true key-states)
  (track-modifiers VK_LMENU true key-states)
  (track-modifiers VK_LSHIFT true key-states)
  (track-modifiers VK_RCONTROL true key-states)
  (track-modifiers VK_RMENU true key-states)
  (track-modifiers VK_RSHIFT true key-states)
  (assert (empty? (in key-states :tracked-modifiers))))


(defn test-inhibit-win-key []
  (def key-states @{})

  (def [key-struct cmd keymap]
    (process-raw-key-event VK_LWIN :down empty-keymap key-states true))
  (assert (nil? cmd))

  (def [key-struct cmd keymap]
    (process-raw-key-event VK_LWIN :up empty-keymap key-states true))
  (assert (nil? cmd))

  (def [key-struct cmd keymap]
    (process-raw-key-event VK_LWIN :down empty-keymap key-states false))
  (assert (= cmd [:map-to VK_LWIN]))

  (def [key-struct cmd keymap]
    (process-raw-key-event VK_LWIN :up empty-keymap key-states false))
  (assert (= cmd [:map-to VK_LWIN])))


(defn test-keymap []
  (var key-states @{})
  (def keymap (define-keymap))
  (define-key keymap
    (key (ascii "A") @[:lctrl])
    :dummy-command-a)
  (define-key keymap
    (key (ascii "B"))
    :dummy-command-b)
  (define-key keymap
    [(key (ascii "C")) (key (ascii "D"))]
    :dummy-command-c)

  # Single key down (no command)

  (def [key-struct cmd new-keymap]
    (process-raw-key-event (ascii "A") :down keymap key-states false))
  (assert (= key-struct (key (ascii "A"))))
  (assert (= cmd [:map-to (ascii "A")]))
  (assert (= new-keymap keymap))

  # Single key up (no command)

  (def [key-struct cmd new-keymap]
    (process-raw-key-event (ascii "A") :up keymap key-states false))
  (assert (= key-struct (key (ascii "A"))))
  (assert (= cmd [:map-to (ascii "A")]))
  (assert (= new-keymap keymap))

  # Down with modifier key (no command)

  (def [key-struct cmd new-keymap]
    (process-raw-key-event VK_LCONTROL :down keymap key-states false))
  (assert (= key-struct (key VK_LCONTROL)))
  (assert (= cmd [:map-to VK_LCONTROL]))
  (assert (= new-keymap keymap))
  (def [key-struct cmd new-keymap]
    (process-raw-key-event (ascii "B") :down keymap key-states false))
  (assert (= key-struct (key (ascii "B") @[:lctrl])))
  (assert (= cmd [:map-to (ascii "B")]))
  (assert (= new-keymap keymap))

  # Up with modifier key (no command)

  (def [key-struct cmd new-keymap]
    (process-raw-key-event (ascii "B") :up keymap key-states false))
  (assert (= key-struct (key (ascii "B") @[:lctrl])))
  (assert (= cmd [:map-to (ascii "B")]))
  (assert (= new-keymap keymap))
  (def [key-struct cmd new-keymap]
    (process-raw-key-event VK_LCONTROL :up keymap key-states false))
  (assert (= key-struct (key VK_LCONTROL)))
  (assert (= cmd [:map-to VK_LCONTROL]))
  (assert (= new-keymap keymap))

  # Single key down (with command)

  (def [key-struct cmd new-keymap]
    (process-raw-key-event (ascii "B") :down keymap key-states false))
  (assert (= key-struct (key (ascii "B"))))
  (assert (= cmd :dummy-command-b))
  (assert (= new-keymap keymap))

  # Single key up (with command)

  (def [key-struct cmd new-keymap]
    (process-raw-key-event (ascii "B") :up keymap key-states false))
  (assert (= key-struct (key (ascii "B"))))
  (assert (= cmd :dummy-command-b))
  (assert (= new-keymap keymap))

  # Down with modifier key (with command)

  (def [key-struct cmd new-keymap]
    (process-raw-key-event VK_LCONTROL :down keymap key-states false))
  (assert (= key-struct (key VK_LCONTROL)))
  (assert (= cmd [:map-to VK_LCONTROL]))
  (assert (= new-keymap keymap))
  (def [key-struct cmd new-keymap]
    (process-raw-key-event (ascii "A") :down keymap key-states false))
  (assert (= key-struct (key (ascii "A") @[:lctrl])))
  (assert (= cmd :dummy-command-a))
  (assert (= new-keymap keymap))

  # Up with modifier key (with command)

  (def [key-struct cmd new-keymap]
    (process-raw-key-event (ascii "A") :up keymap key-states false))
  (assert (= key-struct (key (ascii "A") @[:lctrl])))
  (assert (= cmd :dummy-command-a))
  (assert (= new-keymap keymap))
  (def [key-struct cmd new-keymap]
    (process-raw-key-event VK_LCONTROL :up keymap key-states false))
  (assert (= key-struct (key VK_LCONTROL)))
  (assert (= cmd [:map-to VK_LCONTROL]))
  (assert (= new-keymap keymap))

  # Bound to a sub-keymap

  (def [key-struct cmd new-keymap]
    (process-raw-key-event (ascii "C") :down keymap key-states false))
  (assert (= key-struct (key (ascii "C"))))
  (assert (nil? cmd))
  (assert (= new-keymap keymap))

  (def [key-struct cmd sub-keymap]
    (process-raw-key-event (ascii "C") :up keymap key-states false))
  (assert (= key-struct (key (ascii "C"))))
  (assert (nil? cmd))
  (assert (= sub-keymap (get-key-binding keymap key-struct)))
  (assert (= (in sub-keymap :parent) keymap))

  # Key in sub-keymap

  (def [key-struct cmd new-keymap]
    (process-raw-key-event (ascii "D") :down sub-keymap key-states false))
  (assert (= key-struct (key (ascii "D"))))
  (assert (= cmd :dummy-command-c))
  (assert (= new-keymap sub-keymap))

  (def [key-struct cmd new-keymap]
    (process-raw-key-event (ascii "D") :up sub-keymap key-states false))
  (assert (= key-struct (key (ascii "D"))))
  (assert (= cmd :dummy-command-c))
  (assert (= new-keymap keymap))

  # Key binding not found in sub-keymap
  
  (def [key-struct cmd new-keymap]
    (process-raw-key-event (ascii "C") :down keymap key-states false))
  (def [key-struct cmd sub-keymap]
    (process-raw-key-event (ascii "C") :up keymap key-states false))
  (assert (= sub-keymap (get-key-binding keymap key-struct)))
  (assert (= (in sub-keymap :parent) keymap))

  (def [key-struct cmd new-keymap]
    (process-raw-key-event (ascii "E") :down sub-keymap key-states false))
  (assert (= key-struct (key (ascii "E"))))
  (assert (nil? cmd))
  (assert (= new-keymap sub-keymap))

  (def [key-struct cmd new-keymap]
    (process-raw-key-event (ascii "E") :up sub-keymap key-states false))
  (assert (= key-struct (key (ascii "E"))))
  (assert (nil? cmd))
  (assert (= new-keymap keymap)))


(defn main [&]
  (test-key-states)
  (test-tracked-modifiers)
  (test-inhibit-win-key)
  (test-keymap))
