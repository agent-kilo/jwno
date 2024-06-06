(log/info "++++++++ HELLO THERE ++++++++")
(log/info "++++++++ Keys in jwno-context: %n ++++++++" (keys jwno-context))


# When running from a compiled binary, this needs the Janet
# env be set up properly beforehand
#(import spork/httpf)


#
# A convenience provided by this config to set the navigation keys
# according to the keyboard layout. You can copy this config file,
# change the layout into something you prefer, and off you go.
# Check dir-keys below for supported layouts.
#
(def keyboard-layout :colemak-dh)


(def dir-keys
  (case keyboard-layout
    :qwerty
    # Sorry, no HJKL for QWERTY, since Win+L means "Lock the Screen", and it
    # cannot be overridden. You can use HJKL if a modifier key other
    # than the Win key is used, though.
    {:left "y"
     :down "u"
     :up "i"
     :right "o"}

    :colemak
    {:left "h"
     :down "n"
     :up "e"
     :right "i"}

    :colemak-dh
    {:left "m"
     :down "n"
     :up "e"
     :right "i"}

    :dvorak
    {:left "d"
     :down "h"
     :up "t"
     :right "n"}

    (errorf "unsupported layout: %n" keyboard-layout)))


(def {:key-manager key-man
      :command-manager command-man
      :window-manager window-man
      :hook-manager hook-man}
  jwno-context)


(defn cascade-windows [&opt cur-win]
  (default cur-win (:get-current-window (in window-man :root)))

  (unless (nil? cur-win)
    (def cur-frame (in cur-win :parent))
    (def cur-rect (struct/to-table (in cur-frame :rect)))
    (def all-wins (in cur-frame :children))

    (def dx 32)
    (def dy 32)

    (var next-win (:get-next-child cur-frame cur-win))

    (with-dyns [:jwno-no-hooks true]
      (while (not= cur-win next-win)
        (:transform next-win cur-rect)
        (:activate window-man next-win)
        (put cur-rect :left (+ (in cur-rect :left) dx))
        (put cur-rect :top (+ (in cur-rect :top) dy))
        (set next-win (:get-next-child cur-frame next-win)))
      (:transform cur-win cur-rect)
      (:activate window-man cur-win))))


(defmacro k [key-seq cmd]
  ~(:define-key keymap ,key-seq ,cmd))


(def resize-mode-keymap
  (let [keymap (:new-keymap key-man)]
    (k (in dir-keys :down) [:resize-frame 0 -100])
    (k (in dir-keys :up) [:resize-frame 0 100])
    (k (in dir-keys :left) [:resize-frame -100 0])
    (k (in dir-keys :right) [:resize-frame 100 0])
    (k "=" :balance-frames)
    (k ";" [:zoom-in 0.7])
    (k "enter" :pop-keymap)
    keymap))


(def yank-mode-keymap
  (let [keymap (:new-keymap key-man)]
    (each dir [:down :up :left :right]
      (k (in dir-keys dir) [:move-window dir]))
    (k "enter" :pop-keymap)
    keymap))


(def alpha-mode-keymap
  (let [keymap (:new-keymap key-man)]
    (k (in dir-keys :down) [:change-window-alpha -25])
    (k (in dir-keys :up) [:change-window-alpha 25])
    (k "enter" :pop-keymap)
    keymap))


(defn move-window-after-split [frame]
  (def all-sub-frames (in frame :children))
  (def all-wins (in (first all-sub-frames) :children))
  (def move-to-frame (in all-sub-frames 1))
  (when (>= (length all-wins) 2)
    (:add-child move-to-frame (:get-current-window frame)))
  (:activate move-to-frame))


(defn build-keymap [key-man]
  (let [keymap (:new-keymap key-man)]

    (k "win + shift + q" :quit)
    (k "win + r" :retile)

    (k "win + shift + c" :close-window-or-frame)
    (k "win + shift + f" :close-frame)
    (k "win + ctrl + f" :flatten-parent)

    (k "win + ," [:split-frame :horizontal 2 [0.5] move-window-after-split])
    (k "win + ." [:split-frame :vertical 2 [0.5] move-window-after-split])
    (k "win + =" :balance-frames)
    (k "win + ;" [:zoom-in 0.7])
    (k "win + f" :fill-monitor)

    (k "win + p" :peek-frame)

    (k (string "win + " (in dir-keys :down)) [:enum-frame :next])
    (k (string "win + " (in dir-keys :up)) [:enum-frame :prev])
    (k (string "win + " (in dir-keys :left)) [:enum-window-in-frame :prev])
    (k (string "win + " (in dir-keys :right)) [:enum-window-in-frame :next])

    (each dir [:down :up :left :right]
      (k (string "win + ctrl + " (in dir-keys dir)) [:adjacent-frame dir])
      (k (string "win + shift + " (in dir-keys dir)) [:move-window dir]))

    (k "win + s" [:push-keymap resize-mode-keymap])
    (k "win + k" [:push-keymap yank-mode-keymap])

    (k "win + shift + s" :frame-to-window-size)

    (k "win + a" [:push-keymap alpha-mode-keymap])

    # XXX: If a remapped key is used to trigger keymap switching, and
    # the switched keymap doesn't have the same remap, the translated key
    # will be stuck down.
    (k "ralt" [:map-to (:get-key-code key-man "rwin")])

    (log/debug "keymap = %n" keymap)
    keymap))


(:set-keymap key-man (build-keymap key-man))


(:add-hook hook-man :filter-window
   (fn [_hwnd uia-win exe-path desktop-info]
     (def name (:get_CachedName uia-win))
     (def class-name (:get_CachedClassName uia-win))
     (def desktop-name (in desktop-info :name))

     # Excluded windows
     (cond
       (= "Desktop 2" desktop-name)
       # A "floating" virtual desktop
       false

       # The invisible pseudo window from the Terminal app.
       (= "PseudoConsoleWindow" class-name)
       false

       # The hidden window created when the Bluetooth icon
       # was right-clicked
       (= "BluetoothNotificationAreaIconWindowClass" class-name)
       false

       # The hidden window created when the PowerToys icon
       # was right-clicked
       (= "PToyTrayIconWindow" class-name)
       false

       #  Hidden window from AMD Software notify icon
       (= "AMD:CCC-AEMCapturingWindow" name)
       false

       (= "Mozilla_firefox_default_RemoteWindow" class-name)
       false

       # The hidden "Edit" window from Acrobat
       (and (string/has-suffix? "Acrobat.exe" exe-path)
            (= "Edit" class-name))
       false

       # Steam notify icon menu
       (and (string/has-suffix? "steamwebhelper.exe" exe-path)
            (= "Menu" name))
       false

       true)))

(:add-hook hook-man :window-created
   (fn [win uia-win _exe-path _desktop-info]
     (def class-name (:get_CachedClassName uia-win))
     (cond
       (= "Emacs" class-name)
       (:set-alpha win (math/floor (* 256 0.9)))

       (= "#32770" class-name) # Dialog window class
       (put (in win :tags) :no-expand true))))

(:add-hook hook-man :window-removed
   (fn [dead-win]
     (def parent (in dead-win :parent))
     (when (empty? (in parent :children))
       (:close parent)
       (:retile window-man))))


(:add-command command-man :peek-frame
   (fn [] (cascade-windows)))

(:add-command command-man :fill-monitor
   (fn []
     (def cur-win (:get-current-window (in window-man :root)))
     (when cur-win
       (def cur-frame (in cur-win :parent))
       (def mon-frame (:get-top-frame cur-frame))
       (def rect (in mon-frame :rect))
       (:transform cur-win rect))))
