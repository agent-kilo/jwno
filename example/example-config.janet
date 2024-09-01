#
# Jwno has a bunch of built-in modules:
#   jwno/* modules are high-level APIs provided by Jwno, and
#   jw32/* modules are low-level APIs from the Jw32 package.
#
# See the source code in `mod.janet` for a complete list.
#
(import jwno/util)
(import jwno/log)

#
# Separate modules placed alongside the config file can be
# imported directly. See `wallpaper-manager.janet`.
#
(import wallpaper-manager)


#
# One may call (log/<level> "format string" arg0 arg1 ...) to generate logs.
# To see the logs in a console, run Jwno with the `--log-level <level>` flag.
# Supported levels are: debug, info, warning, error, quiet.
# Jwno will start with the log level set to "quiet" by default.
#
(log/info "++++++++ HELLO THERE ++++++++")
(log/info "++++++++ Keys in jwno/context: %n ++++++++" (keys jwno/context))


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
# For example: `(table/getproto (in jwno/context :window-manager))`
#
(def {:key-manager key-man
      :command-manager command-man
      :window-manager window-man
      :ui-manager ui-man
      :hook-manager hook-man
      :repl-manager repl-man}
  jwno/context)


(def wallpaper-man (wallpaper-manager/wallpaper-manager jwno/context))
#
# When you want something available in an REPL, export it like this:
#
# 1. Retrieve the REPL server you want to modify.
(def repl-server
  (or (:get-default-server repl-man)
      (:start-server repl-man)))
# 2. Do the export for that server.
(util/export-to-repl repl-server wallpaper-man)


#
# Jwno uses different types of tooltip windows to show info or
# get your attention. The :current-frame tooltip is used to show
# which frame is activated. The duration they stay on the screen
# can be set separately. A timeout value of zero means that the
# tooltip should not automatically disappear.
#
(:set-tooltip-timeout ui-man :current-frame 1500) # In milliseconds


#
# A macro to simplify key map definitions. Of course you can call
# :define-key method from the keymap object directly instead.
#
(defmacro k [key-seq cmd &opt doc]
  ~(:define-key keymap ,key-seq ,cmd ,doc))


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
    (k "shift + ;" [:zoom-in 0.3])
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


(defn match-exe-name [exe-name]
  (fn [win]
    (def win-exe (:get-exe-path win))
    (string/has-suffix? (string "\\" exe-name) win-exe)))


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

    (k "win + shift + /" :show-root-keymap)
    (k "win + shift + q" :quit)
    (k "win + r" :retile)

    (k "win + enter  t" [:summon
                         (match-exe-name "WindowsTerminal.exe")
                         true
                         "wt.exe"]
       "Summon Terminal")
    # Some programs (such as Emacs here) would keep the input/output
    # pipes open, blocking Jwno when it exits. Use powershell or cmd
    # to launch the program indirectly in this case.
    (k "win + enter  e" [:summon
                         (match-exe-name "emacs.exe")
                         true
                         "pwsh.exe"
                         "-Command"
                         "Start-Process runemacs.exe"]
       "Summon Emacs")
    (k "win + enter  f" [:summon (match-exe-name "firefox.exe")]
       "Summon Firefox")
    (k "win + enter  d" [:exec
                         true
                         "wt.exe"
                         "pwsh.exe"
                         "-NoExit"
                         "-Command"
                         "& \"$Env:VS_TOOLS_DIR\\Launch-VsDevShell.ps1\" -Arch amd64 -SkipAutomaticLocation"]
       "Launch VS Dev Shell")
    (k "win + enter  r" [:repl true "127.0.0.1" 9999]
       "Launch Jwno REPL")

    (k "win + shift + c" :close-window-or-frame)
    (k "win + shift + f" :close-frame)
    (k "win + ctrl + f" :flatten-parent)

    (k "win + ," [:split-frame :horizontal 2 [0.5] move-window-after-split]
       "Split current frame horizontally")
    (k "win + ." [:split-frame :vertical 2 [0.5] move-window-after-split]
       "Split current frame vertically")
    (k "win + =" :balance-frames)
    (k "win + ;" [:zoom-in 0.7])
    (k "win + shift + ;" [:zoom-in 0.3])
    (k "win + f" :fill-monitor)

    (k "win + p" :cascade-windows-in-frame)

    (k (string "win + " (in dir-keys :down)) [:enum-frame :next])
    (k (string "win + " (in dir-keys :up)) [:enum-frame :prev])
    (k (string "win + " (in dir-keys :left)) [:enum-window-in-frame :prev])
    (k (string "win + " (in dir-keys :right)) [:enum-window-in-frame :next])

    (each dir [:down :up :left :right]
      (k (string "win + ctrl + " (in dir-keys dir)) [:adjacent-frame dir])
      (k (string "win + shift + " (in dir-keys dir)) [:move-window dir]))

    (k "win + s" [:push-keymap resize-mode-keymap]
       "Resize mode")
    (k "win + k" [:push-keymap yank-mode-keymap]
       "Yank mode")

    (k "win + shift + s" :frame-to-window-size)

    (k "win + a" [:push-keymap alpha-mode-keymap]
       "Alpha mode")

    (k "win + b" :describe-window)

    # XXX: If a remapped key is used to trigger keymap switching, and
    # the switched keymap doesn't have the same remap, the translated key
    # will be stuck down.
    (k "ralt" [:map-to (:get-key-code key-man "rwin")])

    (log/debug "keymap = %n" keymap)
    keymap))


(def root-keymap (build-keymap key-man))
(:set-keymap key-man root-keymap)


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

       true)))

(:add-hook hook-man :window-created
   (fn [win uia-win _exe-path _desktop-info]
     (put (in win :tags) :anchor :center)
     (put (in win :tags) :margin 10)

     (def class-name (:get_CachedClassName uia-win))
     (cond
       (find |(= $ class-name)
             ["Emacs"
              "ConsoleWindowClass"
              "CASCADIA_HOSTING_WINDOW_CLASS"])
       (:set-alpha win (math/floor (* 256 0.9)))

       (= "#32770" class-name) # Dialog window class
       (put (in win :tags) :no-expand true))))

(:add-hook hook-man :window-removed
   (fn [dead-win]
     (def parent (in dead-win :parent))
     (when (empty? (in parent :children))
       (util/with-activation-hooks window-man
         (:close parent))
       (:retile window-man))))

(:add-hook hook-man :monitor-updated
   (fn [frame]
     (put (in frame :tags) :padding 10)))


#
# You can easily define your own command. When defining key maps,
# use `[:command-name arg0 arg1 ...]` to invoke commands that
# require arguments, or simply `:command-name` for commands without
# any argument.
#
# Command documentation can be found by evaluating
#
#   (:print-doc command-manager :command-name)
#
# in the REPL.
#
(:add-command command-man :fill-monitor
   (fn []
     (def cur-win (:get-current-window (in window-man :root)))
     (when cur-win
       (def cur-frame (in cur-win :parent))
       (def mon-frame (:get-top-frame cur-frame))
       (def rect (:get-padded-rect mon-frame))
       (:transform cur-win rect)))
   ```
   (:fill-monitor)

   Resizes the focused window, so that it fills the whole work
   area of the current monitor.
   ```)

(:add-command command-man :show-root-keymap
   (fn []
     (:show-tooltip
        ui-man
        :show-root-keymap
        (:format root-keymap)
        nil
        nil
        10000
        :center))
   ```
   (:show-root-keymap)

   Shows the root keymap defined in the config file.
   ```)
