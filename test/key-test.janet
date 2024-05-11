(use jw32/_winuser)
(use ../src/key)


(def empty-keymap (define-keymap))


(defn test-find-binding []
  (def keymap (define-keymap))
  (:define-key keymap
    (key (ascii "A") @[:lctrl])
    :dummy-command-a)
  (:define-key keymap
    (key (ascii "B") @[:ctrl])
    :dummy-command-b)
  (:define-key keymap
    (key (ascii "C"))
    :dummy-command-c)

  (def sub-keymap (define-keymap))
  (:define-key sub-keymap
    (key (ascii "E"))
    :dummy-command-e)

  (:define-key keymap
    (key (ascii "D"))
    sub-keymap)

  (:define-key keymap
    (key (ascii "F") @[:lctrl :lalt])
    :dummy-command-f)
  (:define-key keymap
    (key (ascii "G") @[:ctrl :alt])
    :dummy-command-g)

  (def handler (keyboard-hook-handler keymap))

  (def dummy-hook-struct-a {:vkCode (ascii "A")})
  (assert (= :dummy-command-a (:find-binding handler dummy-hook-struct-a @{:lctrl true})))
  (assert (nil? (:find-binding handler dummy-hook-struct-a @{:rctrl true})))
  (assert (nil? (:find-binding handler dummy-hook-struct-a @{})))

  (def dummy-hook-struct-b {:vkCode (ascii "B")})
  (assert (= :dummy-command-b (:find-binding handler dummy-hook-struct-b @{:lctrl true})))
  (assert (= :dummy-command-b (:find-binding handler dummy-hook-struct-b @{:rctrl true})))
  (assert (nil? (:find-binding handler dummy-hook-struct-b @{})))

  (def dummy-hook-struct-c {:vkCode (ascii "C")})
  (assert (= :dummy-command-c (:find-binding handler dummy-hook-struct-c @{})))
  (assert (nil? (:find-binding handler dummy-hook-struct-c @{:lctrl true})))

  (def dummy-hook-struct-d {:vkCode (ascii "D")})
  (assert (= sub-keymap (:find-binding handler dummy-hook-struct-d @{})))

  (def dummy-hook-struct-f {:vkCode (ascii "F")})
  (assert (= :dummy-command-f (:find-binding handler dummy-hook-struct-f @{:lctrl true :lalt true})))
  (assert (nil? (:find-binding handler dummy-hook-struct-f @{:lctrl true :ralt true})))

  (def dummy-hook-struct-g {:vkCode (ascii "G")})
  (assert (= :dummy-command-g (:find-binding handler dummy-hook-struct-g @{:lctrl true :lalt true})))
  (assert (= :dummy-command-g (:find-binding handler dummy-hook-struct-g @{:rctrl true :lalt true})))
  (assert (= :dummy-command-g (:find-binding handler dummy-hook-struct-g @{:lctrl true :ralt true})))
  (assert (= :dummy-command-g (:find-binding handler dummy-hook-struct-g @{:rctrl true :ralt true}))))


(defn main [&]
  (test-find-binding))
