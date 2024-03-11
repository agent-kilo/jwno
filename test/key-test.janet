(use jw32/winuser)
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


(defn main [&]
  (test-key-states)
  (test-tracked-modifiers)
  (test-inhibit-win-key))
