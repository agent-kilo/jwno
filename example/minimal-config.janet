#
# minimal-config.janet
#
# This is a minimal example config file for jwno (https://github.com/agent-kilo/jwno).
# Only basic window/frame operations are bound in the root keymap.
# No extra features are enabled, except current-frame-area shown
# below. You can take this config as a "baseline", and use it to
# explore Jwno's default behavior.
#
# It assumes you have the US QWERTY keyboard layout.
#
# Please see the docs for details on a command:
# https://agent-kilo.github.io/jwno/ref/built-in-commands/index.html
#
# To try it out, download this file, then drag-n-drop it to jwno.exe.
#


(import jwno/indicator)

#
# The current-frame-area indicator highlights the active frame,
# if it's empty.
#
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
