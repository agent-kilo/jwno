(use jw32/_winuser)
(use jw32/_util)
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
  (assert (= :dummy-command-a (in (:find-binding handler dummy-hook-struct-a @{:lctrl true}) :cmd)))
  (assert (nil? (:find-binding handler dummy-hook-struct-a @{:rctrl true})))
  (assert (nil? (:find-binding handler dummy-hook-struct-a @{})))

  (def dummy-hook-struct-b {:vkCode (ascii "B")})
  (assert (= :dummy-command-b (in (:find-binding handler dummy-hook-struct-b @{:lctrl true}) :cmd)))
  (assert (= :dummy-command-b (in (:find-binding handler dummy-hook-struct-b @{:rctrl true}) :cmd)))
  (assert (nil? (:find-binding handler dummy-hook-struct-b @{})))

  (def dummy-hook-struct-c {:vkCode (ascii "C")})
  (assert (= :dummy-command-c (in (:find-binding handler dummy-hook-struct-c @{}) :cmd)))
  (assert (nil? (:find-binding handler dummy-hook-struct-c @{:lctrl true})))

  (def dummy-hook-struct-d {:vkCode (ascii "D")})
  (assert (= sub-keymap (:find-binding handler dummy-hook-struct-d @{})))

  (def dummy-hook-struct-f {:vkCode (ascii "F")})
  (assert (= :dummy-command-f
             (in (:find-binding handler dummy-hook-struct-f @{:lctrl true :lalt true}) :cmd)))
  (assert (nil? (:find-binding handler dummy-hook-struct-f @{:lctrl true :ralt true})))

  (def dummy-hook-struct-g {:vkCode (ascii "G")})
  (assert (= :dummy-command-g
             (in (:find-binding handler dummy-hook-struct-g @{:lctrl true :lalt true}) :cmd)))
  (assert (= :dummy-command-g
             (in (:find-binding handler dummy-hook-struct-g @{:rctrl true :lalt true}) :cmd)))
  (assert (= :dummy-command-g
             (in (:find-binding handler dummy-hook-struct-g @{:lctrl true :ralt true}) :cmd)))
  (assert (= :dummy-command-g
             (in (:find-binding handler dummy-hook-struct-g @{:rctrl true :ralt true}) :cmd))))


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

  (assert (deep= (:parse-key keymap "Alt-Backspace")
                 @[(key VK_BACK [:alt])]))
  (assert (deep= (:parse-key keymap "Alt-Tab")
                 @[(key VK_TAB [:alt])]))
  (assert (deep= (:parse-key keymap "Alt-Pause")
                 @[(key VK_PAUSE [:alt])]))
  (assert (deep= (:parse-key keymap "Alt-CapsLock")
                 @[(key VK_CAPITAL [:alt])]))
  (assert (deep= (:parse-key keymap "Alt-Esc")
                 @[(key VK_ESCAPE [:alt])]))
  (assert (deep= (:parse-key keymap "Alt-Space")
                 @[(key VK_SPACE [:alt])]))
  (assert (deep= (:parse-key keymap "Alt-PageUp")
                 @[(key VK_PRIOR [:alt])]))
  (assert (deep= (:parse-key keymap "Alt-PageDown")
                 @[(key VK_NEXT [:alt])]))
  (assert (deep= (:parse-key keymap "Alt-End")
                 @[(key VK_END [:alt])]))
  (assert (deep= (:parse-key keymap "Alt-Home")
                 @[(key VK_HOME [:alt])]))
  (assert (deep= (:parse-key keymap "Alt-Home")
                 @[(key VK_HOME [:alt])]))
  (assert (deep= (:parse-key keymap "Alt-Left")
                 @[(key VK_LEFT [:alt])]))
  (assert (deep= (:parse-key keymap "Alt-ScrollLock")
                 @[(key VK_SCROLL [:alt])]))

  (assert (deep= (:parse-key keymap "Alt-NumPad0")
                 @[(key VK_NUMPAD0 [:alt])]))
  (assert (deep= (:parse-key keymap "Alt-NumPad9")
                 @[(key VK_NUMPAD9 [:alt])]))
  (assert (deep= (:parse-key keymap "Alt-NumPad*")
                 @[(key VK_MULTIPLY [:alt])]))
  (assert (deep= (:parse-key keymap "Alt-NumPad+")
                 @[(key VK_ADD [:alt])]))
  (assert (deep= (:parse-key keymap "Alt-NumPad-")
                 @[(key VK_SUBTRACT [:alt])]))
  (assert (deep= (:parse-key keymap "Alt-NumPad.")
                 @[(key VK_DECIMAL [:alt])]))
  (assert (deep= (:parse-key keymap "Alt-NumPad/")
                 @[(key VK_DIVIDE [:alt])]))

  (assert (deep= (:parse-key keymap "Alt-NumPad+ a")
                 @[(key VK_ADD [:alt])
                   (key (ascii "A"))]))
  (assert (deep= (:parse-key keymap "Alt-NumPad- a")
                 @[(key VK_SUBTRACT [:alt])
                   (key (ascii "A"))]))
  (assert (deep= (:parse-key keymap "NumPad+ a")
                 @[(key VK_ADD)
                   (key (ascii "A"))]))
  (assert (deep= (:parse-key keymap "NumPad+")
                 @[(key VK_ADD)]))

  (assert (deep= (:parse-key keymap "Win+,")
                 @[(key VK_OEM_COMMA [:win])]))
  (assert (deep= (:parse-key keymap "Win+.")
                 @[(key VK_OEM_PERIOD [:win])]))
  (assert (deep= (:parse-key keymap "Win+=")
                 @[(key VK_OEM_PLUS [:win])]))
  (assert (deep= (:parse-key keymap "Win+-")
                 @[(key VK_OEM_MINUS [:win])]))
  (assert (deep= (:parse-key keymap "Win+;")
                 @[(key VK_OEM_1 [:win])]))
  (assert (deep= (:parse-key keymap "Win+/")
                 @[(key VK_OEM_2 [:win])]))
  (assert (deep= (:parse-key keymap "Win+`")
                 @[(key VK_OEM_3 [:win])]))
  (assert (deep= (:parse-key keymap "Win+[")
                 @[(key VK_OEM_4 [:win])]))
  (assert (deep= (:parse-key keymap "Win+\\")
                 @[(key VK_OEM_5 [:win])]))
  (assert (deep= (:parse-key keymap "Win+]")
                 @[(key VK_OEM_6 [:win])]))
  (assert (deep= (:parse-key keymap "Win+'")
                 @[(key VK_OEM_7 [:win])]))

  (var err nil)
  (try
    (:parse-key keymap "NumPad+a")
    ((e f) (set err e)))
  (assert (= err "unknown key name: \"numpad+a\""))

  (set err nil)
  (try
    (:parse-key keymap "win+f25")
    ((e f) (set err e)))
  (assert (= err "unknown key name: \"f25\""))

  (set err nil)
  (try
    (:parse-key keymap "win+dummy")
    ((e f) (set err e)))
  (assert (= err "unknown key name: \"dummy\""))

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

  (assert (deep= (:parse-key keymap @"enter e")
                 @[(key VK_RETURN [])
                   (key (ascii "E") [])]))
  )


(defn test-key-manager-set-keymap []
  (var keymap-buf-ptr nil)

  (def dummy-ui-man
    @{:set-keymap (fn [self ptr] (set keymap-buf-ptr ptr))})
  (def dummy-hook-man
    @{:add-hook (fn [self name hook-fn] :nop)})
  (def key-man (key-manager dummy-ui-man dummy-hook-man))

  (def keymap (:new-keymap key-man))
  (def dummy-fn  (fn [] :fn))
  (def dummy-fn2 (fn [] :fn2))
  (def dummy-fn3 (fn [] :fn3))
  (def trans-keymap (:new-keymap key-man))

  (:define-key keymap "A"       [:dummy-command dummy-fn])
  (:define-key keymap "B Enter" [:dummy-command2 dummy-fn2])
  (:define-key keymap "C"       [:push-keymap trans-keymap])
  (:define-key trans-keymap "D" [:dummy-command3 dummy-fn3])

  # main thread -> ui thread
  (:set-keymap key-man keymap)

  (def reverse-lookup (in key-man :keymap-fn-reverse-lookup))
  (assert (= 3 (length reverse-lookup)))

  (def fn-sym  (in reverse-lookup dummy-fn))
  (def fn-sym2 (in reverse-lookup dummy-fn2))
  (def fn-sym3 (in reverse-lookup dummy-fn3))

  (assert (= 3 (length reverse-lookup)))
  (assert (not (nil? fn-sym)))
  (assert (not (nil? fn-sym2)))
  (assert (not (nil? fn-sym3)))
  (assert (not= fn-sym fn-sym2))
  (assert (not= fn-sym fn-sym3))
  (assert (not= fn-sym2 fn-sym3))

  (def hook-handler (keyboard-hook-handler nil))
  (:set-keymap hook-handler keymap-buf-ptr)

  (assert (= (length reverse-lookup) (length (in hook-handler :keymap-sym-lookup))))
  (def handler-keymap (get-in hook-handler [:current-keymap 0]))
  (assert (deep= (sort (keys keymap)) (sort (keys handler-keymap))))

  (eachp [k v] handler-keymap
    (when (and (struct? k) (table? v))
      (def cmd (in v :cmd))
      (if (keymap? cmd)
        (assert (= [:dummy-command2 fn-sym2]
                   (get-in cmd [(first (keys cmd)) :cmd])))
        # else
        (match cmd
          [:dummy-command dfn]
          (do
            (assert (= dfn fn-sym)))

          [:push-keymap tkm]
          (assert (= [:dummy-command3 fn-sym3]
                     (get-in tkm [(first (keys tkm)) :cmd])))))))

  # ui thread -> main thread
  (set keymap-buf-ptr
       (:marshal-keymap hook-handler
                        (get-in hook-handler [:current-keymap 0])))
  (def unmarshaled-keymap (:unmarshal-keymap key-man keymap-buf-ptr))
  (assert (deep= keymap unmarshaled-keymap)))


(defn main [&]
  (test-find-binding)
  (test-keymap-parse-key)
  (test-key-manager-set-keymap))
