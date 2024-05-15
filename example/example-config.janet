(log/info "++++++++ HELLO THERE ++++++++")
(log/info "++++++++ Keys in jwno-context: %n ++++++++" (keys jwno-context))


(import spork/httpf)


(def key-man (in jwno-context :key-manager))
(def command-man (in jwno-context :command-manager))


(def resize-mode-keymap
  (let [keymap (:new-keymap key-man)]
    (:define-key keymap
      "n"
      [:resize-current-frame 0 100])
    (:define-key keymap
      "e"
      [:resize-current-frame 0 -100])
    (:define-key keymap
      "m"
      [:resize-current-frame -100 0])
    (:define-key keymap
      "i"
      [:resize-current-frame 100 0])
    (:define-key keymap
      "enter"
      :pop-keymap)
    keymap))


(def move-mode-keymap
  (let [keymap (:new-keymap key-man)]
    (:define-key keymap
      "n"
      [:move-current-window :down])
    (:define-key keymap
      "e"
      [:move-current-window :up])
    (:define-key keymap
      "m"
      [:move-current-window :left])
    (:define-key keymap
      "i"
      [:move-current-window :right])
    (:define-key keymap
      "enter"
      :pop-keymap)
    keymap))


(defn build-keymap [key-man]
  (def keymap (:new-keymap key-man))
  (def k
    (fn [key-seq cmd]
      (:define-key keymap key-seq cmd)))

  (k "win+q" :quit)

  (:define-key keymap
    "win+r"
    :retile)

  (:define-key keymap
    "win+, win+,"
    [:split :horizontal 2 [0.5] 1 0])

  (:define-key keymap
    "win+, 3"
    [:split :horizontal 3 [0.2 0.6 0.2] 0 1])

  (:define-key keymap
    "win+. win+."
    [:split :vertical 2 [0.5] 1 0])

  (:define-key keymap
    "win+/"
    :flatten-parent)

  (:define-key keymap
    "win+n"
    [:enum-frame :next])

  (:define-key keymap
    "win+e"
    [:enum-frame :prev])

  (:define-key keymap
    "win+i"
    :next-window-in-frame)

  (:define-key keymap
    "win+m"
    :prev-window-in-frame)

  (:define-key keymap
    "win+ctrl+n"
    [:adjacent-frame :down])
  (:define-key keymap
    "win+ctrl+e"
    [:adjacent-frame :up])
  (:define-key keymap
    "win+ctrl+m"
    [:adjacent-frame :left])
  (:define-key keymap
    "win+ctrl+i"
    [:adjacent-frame :right])

  (:define-key keymap
    "win+shift+n"
    [:move-current-window :down])
  (:define-key keymap
    "win+shift+e"
    [:move-current-window :up])
  (:define-key keymap
    "win+shift+m"
    [:move-current-window :left])
  (:define-key keymap
    "win+shift+i"
    [:move-current-window :right])
  (:define-key keymap
    "win+w"
    [:push-keymap move-mode-keymap])

  (:define-key keymap
    "win+s win+n"
    [:resize-current-frame 0 100])
  (:define-key keymap
    "win+s win+e"
    [:resize-current-frame 0 -100])
  (:define-key keymap
    "win+s win+m"
    [:resize-current-frame -100 0])
  (:define-key keymap
    "win+s win+i"
    [:resize-current-frame 100 0])
  (:define-key keymap
    "win+s win+s"
    [:push-keymap resize-mode-keymap])

  (:define-key keymap
    "win+f"
    [:focus-mode 0.7])

  (:define-key keymap
    "win+="
    :balance-frames)

  (:define-key keymap
    "win+ctrl+s"
    :frame-to-current-window-size)

  (:define-key keymap
    "win+shift+c"
    :close-current-window)

  (:define-key keymap
    "win+t win+n"
    [:change-current-window-alpha -25])
  (:define-key keymap
    "win+t win+e"
    [:change-current-window-alpha 25])

  (:define-key keymap
    "rwin f"
    [:focus-mode 0.7])

  # XXX: If a remapped key is used to trigger keymap switching, and
  # the switched keymap doesn't have the same remap, the translated key
  # will be stuck down.
  (:define-key keymap
    "ralt"
    [:map-to VK_RWIN])

  (log/debug "keymap = %n" keymap)
  keymap)


(:push-keymap key-man (build-keymap key-man))
