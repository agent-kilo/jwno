(log/info "++++++++ HELLO THERE ++++++++")
(log/info "++++++++ Keys in jwno-context: %n ++++++++" (keys jwno-context))


(import spork/httpf)


(def key-man (in jwno-context :key-manager))
(def command-man (in jwno-context :command-manager))
(def window-man (in jwno-context :window-manager))


(:add-command command-man :close-current-window-or-frame
   (fn []
     (if-let [cur-win (:get-current-window (in window-man :layout))]
       (:close-hwnd window-man (in cur-win :hwnd))
       (let [layout (in window-man :layout)
             cur-frame (:get-current-frame layout)]
         (:close-frame layout cur-frame)
         (:retile window-man)
         (:activate window-man (:get-current-window layout))))))


(:add-command command-man :split-and-move-current-window
   (fn [dir]
     (def layout (in window-man :layout))
     (def cur-frame (:get-current-frame layout))
     (def cur-win (:get-current-window cur-frame))
     (def win-count (length (in cur-frame :children)))
     (:split cur-frame dir 2 [0.5])
     (def last-new-frame (last (in cur-frame :children)))
     (when (>= win-count 2)
       # Only move the current window when there's at least
       # another window in the old frame
       (:add-child last-new-frame cur-win))
     (:retile window-man cur-frame)
     (:activate window-man last-new-frame)))


(defmacro k [key-seq cmd]
  ~(:define-key keymap ,key-seq ,cmd))


(def resize-mode-keymap
  (let [keymap (:new-keymap key-man)]
    (k "n" [:resize-current-frame 0 100])
    (k "e" [:resize-current-frame 0 -100])
    (k "m" [:resize-current-frame -100 0])
    (k "i" [:resize-current-frame 100 0])
    (k "=" :balance-frames)
    (k "z" [:focus-mode 0.7])
    (k "enter" :pop-keymap)
    keymap))


(def move-mode-keymap
  (let [keymap (:new-keymap key-man)]
    (k "n" [:move-current-window :down])
    (k "e" [:move-current-window :up])
    (k "m" [:move-current-window :left])
    (k "i" [:move-current-window :right])
    (k "enter" :pop-keymap)
    keymap))


(def alpha-mode-keymap
  (let [keymap (:new-keymap key-man)]
    (k "n" [:change-current-window-alpha -25])
    (k "e" [:change-current-window-alpha 25])
    (k "enter" :pop-keymap)))


(defn build-keymap [key-man]
  (def keymap (:new-keymap key-man))

  (k "win + shift + q" :quit)
  (k "win + t" :retile)

  (k "win + shift + c" :close-current-window-or-frame)
  (k "win + shift + f" :close-current-frame)
  (k "win + ctrl + f" :flatten-parent)

  (k "win + ," [:split-and-move-current-window :horizontal])
  (k "win + ." [:split-and-move-current-window :vertical])
  (k "win + =" :balance-frames)
  (k "win + z" [:focus-mode 0.7])

  (k "win + n" [:enum-frame :next])
  (k "win + e" [:enum-frame :prev])
  (k "win + i" :next-window-in-frame)
  (k "win + m" :prev-window-in-frame)

  (k "win + ctrl + n" [:adjacent-frame :down])
  (k "win + ctrl + e" [:adjacent-frame :up])
  (k "win + ctrl + m" [:adjacent-frame :left])
  (k "win + ctrl + i" [:adjacent-frame :right])

  (k "win + shift + n" [:move-current-window :down])
  (k "win + shift + e" [:move-current-window :up])
  (k "win + shift + m" [:move-current-window :left])
  (k "win + shift + i" [:move-current-window :right])

  (k "win + s" [:push-keymap resize-mode-keymap])
  (k "win + g" [:push-keymap move-mode-keymap])

  (k "win + shift + s" :frame-to-current-window-size)

  (k "win + a" [:push-keymap alpha-mode-keymap])

  # XXX: If a remapped key is used to trigger keymap switching, and
  # the switched keymap doesn't have the same remap, the translated key
  # will be stuck down.
  (k "ralt" [:map-to VK_RWIN])

  (log/debug "keymap = %n" keymap)
  keymap)


(:set-keymap key-man (build-keymap key-man))
