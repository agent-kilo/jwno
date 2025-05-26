(import jwno/indicator)

(def current-frame-area
  (indicator/current-frame-area jwno/context))
(:enable current-frame-area)

(def root-keymap (:new-keymap (in jwno/context :key-manager)))

(:define-key root-keymap "Win + Shift + Q" :quit)

(:define-key root-keymap "Win + R" :retile)

(:define-key root-keymap "Win + ," [:split-frame :horizontal])
(:define-key root-keymap "Win + ." [:split-frame :vertical])

(:define-key root-keymap "Win + Shift + C" :close-window-or-frame)
(:define-key root-keymap "Win + Shift + F" :close-frame)

(:define-key root-keymap "Win + U" [:enum-frame :next])
(:define-key root-keymap "Win + I" [:enum-frame :prev])

(:define-key root-keymap "Win + Y" [:enum-window-in-frame :prev])
(:define-key root-keymap "Win + O" [:enum-window-in-frame :next])

(:define-key root-keymap "Win + Shift + Y" [:move-window :left])
(:define-key root-keymap "Win + Shift + U" [:move-window :down])
(:define-key root-keymap "Win + Shift + I" [:move-window :up])
(:define-key root-keymap "Win + Shift + O" [:move-window :right])

(:set-keymap (in jwno/context :key-manager) root-keymap)
