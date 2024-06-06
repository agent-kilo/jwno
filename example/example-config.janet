#
# One may call (log/<level> "format string" arg0 arg1 ...) to generate logs.
# To see the logs in a console, run Jwno with the `--log-level <level>` flag.
# Supported levels are: debug, info, warning, error, quiet.
# Jwno will start with the log level set to "quiet" by default.
#
(log/info "++++++++ HELLO THERE ++++++++")
(log/info "++++++++ Keys in jwno-context: %n ++++++++" (keys jwno-context))


#
# A convenience provided by this config to set the navigation keys
# according to the keyboard layout. You can copy this config file,
# change the keyboard-layout into something you prefer, and off you
# go. Check dir-keys below for layouts supported by this config.
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


#
# Most of Jwno's APIs are exported as methods in these "manager" objects.
# You can inspect them in the Jwno REPL by looking into their prototypes.
# For example: `(table/getproto (in jwno-context :window-manager))`
#
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


#
# A macro to simplify key map definitions. Of course you can call
# :define-key method from the keymap object directly instead.
#
(defmacro k [key-seq cmd]
  ~(:define-key keymap ,key-seq ,cmd))


#
# A transient key map for resizing frames, so that we don't have
# to hold the modifier keys all the time.
# Transient key maps are activated by :push-keymap commands, and
# deactivated by :pop-keymap commands. See the definition for
# Win+S key combo below.
#
(def resize-mode-keymap
  (let [keymap (:new-keymap key-man)]
    (k (in dir-keys :down) [:resize-frame 0 -100])
    (k (in dir-keys :up) [:resize-frame 0 100])
    (k (in dir-keys :left) [:resize-frame -100 0])
    (k (in dir-keys :right) [:resize-frame 100 0])
    (k "=" :balance-frames)
    (k ";" [:zoom-in 0.7])
    #
    # In a transient key map, make sure a :pop-keymap binding is defined,
    # or there will be no way to deactivate this key map.
    #
    (k "enter" :pop-keymap)
    keymap))


#
# A transient key map for moving windows around.
# See the definition for Win+K key combo below.
#
(def yank-mode-keymap
  (let [keymap (:new-keymap key-man)]
    (each dir [:down :up :left :right]
      (k (in dir-keys dir) [:move-window dir]))
    (k "enter" :pop-keymap)
    keymap))


#
# A transient key map for adjusting transparency for the
# current window. See the definition for Win+A key combo
# below.
#
(def alpha-mode-keymap
  (let [keymap (:new-keymap key-man)]
    (k (in dir-keys :down) [:change-window-alpha -25])
    (k (in dir-keys :up) [:change-window-alpha 25])
    (k "enter" :pop-keymap)
    keymap))


#
# Jwno commands can accept closures/functions as arguments.
# For example, the :split-frame command accepts a function
# to adjust windows/frames after the splitting is done. Below
# is such a function to move the activated window into the
# new empty frame, and activate (move focus to) that frame.
# See the definitions for Win+, and Win+. key combos below.
#
(defn move-window-after-split [frame]
  (def all-sub-frames (in frame :children))
  (def all-wins (in (first all-sub-frames) :children))
  (def move-to-frame (in all-sub-frames 1))
  (when (>= (length all-wins) 2)
    (:add-child move-to-frame (:get-current-window frame)))
  (:activate move-to-frame))


#
# We build our main key map below. Make sure to call the :set-keymap
# method from the key-manager object with the new key map, or Jwno
# will not respond to any key events at all.
#
# The most straightforward way to understand Jwno commands is to
# simply try out the bindings below. Some commands need more than
# one window or frame to have any effect, though.
#
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


#
# You can easily define your own command. When defining key maps,
# use `[:command-name arg0 arg1 ...]` to invoke commands that
# require arguments, or simply `:command-name` for commands without
# any argument.
#
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
