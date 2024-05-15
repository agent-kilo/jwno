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


(defn test-keymap-parse-key []
  (def keymap (define-keymap))
  (assert (deep= (:parse-key keymap "win+a")
                 @[(key (ascii "A") [:win])]))
  (assert (deep= (:parse-key keymap "Win+a")
                 @[(key (ascii "A") [:win])]))
  (assert (deep= (:parse-key keymap "WIN+a")
                 @[(key (ascii "A") [:win])]))
  (assert (deep= (:parse-key keymap "wIn+a")
                 @[(key (ascii "A") [:win])]))
  (assert (deep= (:parse-key keymap "win+A")
                 @[(key (ascii "A") [:win])]))
  (assert (deep= (:parse-key keymap "win + a")
                 @[(key (ascii "A") [:win])]))
  (assert (deep= (:parse-key keymap "win+ a")
                 @[(key (ascii "A") [:win])]))
  (assert (deep= (:parse-key keymap "win-a")
                 @[(key (ascii "A") [:win])]))
  (assert (deep= (:parse-key keymap "lwin+a")
                 @[(key (ascii "A") [:lwin])]))
  (assert (deep= (:parse-key keymap "lwin+rwin+a")
                 @[(key (ascii "A") [:lwin :rwin])]))
  (assert (deep= (:parse-key keymap "win+ctrl+a")
                 @[(key (ascii "A") [:ctrl :win])]))
  (assert (deep= (:parse-key keymap "win+ctrl+shift+a")
                 @[(key (ascii "A") [:ctrl :win :shift])]))
  (assert (deep= (:parse-key keymap "win+ctrl+shift+alt+a")
                 @[(key (ascii "A") [:ctrl :win :shift :alt])]))
  (assert (deep= (:parse-key keymap "win+enter")
                 @[(key VK_RETURN [:win])]))
  (assert (deep= (:parse-key keymap "win+ctrl+enter")
                 @[(key VK_RETURN [:win :ctrl])]))
  (assert (deep= (:parse-key keymap "win+Enter")
                 @[(key VK_RETURN [:win])]))
  (assert (deep= (:parse-key keymap "win+ENTER")
                 @[(key VK_RETURN [:win])]))
  (assert (deep= (:parse-key keymap "win+eNTER")
                 @[(key VK_RETURN [:win])]))
  (assert (deep= (:parse-key keymap "win+f1")
                 @[(key VK_F1 [:win])]))
  (assert (deep= (:parse-key keymap "win+F1")
                 @[(key VK_F1 [:win])]))
  (assert (deep= (:parse-key keymap "win+f10")
                 @[(key VK_F10 [:win])]))
  (assert (deep= (:parse-key keymap "win+f19")
                 @[(key VK_F19 [:win])]))
  (assert (deep= (:parse-key keymap "win+f20")
                 @[(key VK_F20 [:win])]))
  (assert (deep= (:parse-key keymap "win+f24")
                 @[(key VK_F24 [:win])]))

  (var err nil)
  (try
    (:parse-key keymap "win+f25")
    ((e f) (set err e)))
  (assert (and err
               (string/has-prefix? "failed to parse key spec: " err)))

  (set err nil)
  (try
    (:parse-key keymap "win+dummy")
    ((e f) (set err e)))
  (assert (and err
               (string/has-prefix? "failed to parse key spec: " err)))

  (assert (deep= (:parse-key keymap "win+a win+b")
                 @[(key (ascii "A") [:win])
                   (key (ascii "B") [:win])]))
  (assert (deep= (:parse-key keymap "win+a  win+b")
                 @[(key (ascii "A") [:win])
                   (key (ascii "B") [:win])]))
  (assert (deep= (:parse-key keymap "win+a\twin+b")
                 @[(key (ascii "A") [:win])
                   (key (ascii "B") [:win])]))
  (assert (deep= (:parse-key keymap "win+a\nwin+b")
                 @[(key (ascii "A") [:win])
                   (key (ascii "B") [:win])]))

  (assert (deep= (:parse-key keymap "a")
                 @[(key (ascii "A") [])]))
  (assert (deep= (:parse-key keymap "enter")
                 @[(key VK_RETURN [])]))
  (assert (deep= (:parse-key keymap "e enter")
                 @[(key (ascii "E") [])
                   (key VK_RETURN [])]))
  (assert (deep= (:parse-key keymap "enter e")
                 @[(key VK_RETURN [])
                   (key (ascii "E") [])]))
  )


(defn main [&]
  (test-find-binding)
  (test-keymap-parse-key))
