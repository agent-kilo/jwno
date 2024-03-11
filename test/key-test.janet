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


(defn main [&]
  (test-key-states))
