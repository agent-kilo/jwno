#
# example-config.janet
#
# This is an example config file for Jwno (https://github.com/agent-kilo/jwno).
# The features introduced here are quite exhaustive, you may
# want to customize/simplify it a bit before adopting it for
# daily use. Specifically, these variables (in the Global Definitions
# section) are meant to be changed by the user, according to
# their preferences:
#
#   * mod-key
#   * keyboard-layout
#
# Depending on your mod-key setting, You can press
# `Win + Shift + /` (the slash key) or `Alt + Shift + /` to
# show all defined key bindings.
#
# The code is heavily commented, you can skim through the
# comments and then focus on the parts you're interested in.
#
# Features enabled here:
#
#   * Most of the built-in commands are mapped to key bindings.
#     (See the "Key Bindings (Keymaps)" section below.)
#   * auto-layout/close-empty-frame:  Automatically closes an empty frame when a window is closed.
#   * indicator/current-frame-area:   Highlights an empty frame.
#   * ui-hint/ui-hint:                Interacts with UI elements using the keyboard.
#   * scratch-pad/scratch-pad:        Hides windows and summons them later.
#   * layout-history/layout-history:  Saves and restores frame layouts.
#   * Some demo code showcasing hook usage.
#     (See the "Using Hooks" section.)
#   * Some demo code showcasing custom commands.
#     (See the "Custom Commands" section.)
#
# Some quirks of this example config that may surprise you:
#
#   * It ignores all windows coming from a virtual desktop named "Desktop 2".
#     (See the user-window-filter function.)
#   * It sets an alpha value to Emacs, Windows Terminal, and console windows,
#     making them transparent. (See the :window-created hook below.)
#   * It treats an application from a certain French game company specially.
#     (See the user-forced-window-filter function.)
#   * When mod-key is set to "Win" (the default), it binds all ui-hint features
#     to the `RAlt` key, but your keyboard may have an `AltGr` key on the right
#     instead. In that case, you need to change the hint-key variable manually
#     to choose another leading key.
#   * It sometimes grants you infinite lives. Please don't take it seriously
#     and do anything dangerous.
#
# To try out this example config, download this file, make adjustments as
# needed, then drag-n-drop it to jwno.exe.
#


#===========#
#           #
#  Imports  #
#           #
#===========#

#
# Jwno has a bunch of built-in modules:
#   jwno/* modules are high-level APIs provided by Jwno, and
#   jw32/* modules are low-level APIs from the Jw32 package.
#
# See the source code in `mod.janet` for a complete list.
#
(import jwno/auto-layout)
(import jwno/indicator)
(import jwno/ui-hint)
(import jwno/scratch-pad)
(import jwno/layout-history)
(import jwno/util)
(import jwno/log)

(use jw32/_uiautomation) # For UIA_* constants


#==========#
#          #
#  Basics  #
#          #
#==========#

#
# One may call (log/<level> "format string" arg0 arg1 ...) to generate logs.
# To see the logs in a console, run Jwno with the `--log-level <level>` flag.
# Supported levels are: debug, info, warning, error, quiet.
# Jwno will start with the log level set to "quiet" by default.
#
(log/info "++++++++ HELLO THERE ++++++++")


#======================#
#                      #
#  Global Definitions  #
#                      #
#======================#

#
# A convenience provided by this config, to set the primary modifier
# key. Most of the key bindings defined in the root keymap contains
# the mod-key as a modifier. Only "Win" and "Alt" are supported here.
#
# Note that Jwno tries to treat all modifier keys equally, and has
# no concept of a "mod key". All mod-key related code is implemented
# in this config file only. We use ${MOD} as a placeholder for the
# mod-key here.
#
(def mod-key "Win")

(unless (find |(= $ (string/ascii-lower mod-key)) ["win" "alt"])
  (errorf "unsupported mod key: %n" mod-key))

#
# A convenience provided by this config to set the navigation keys
# according to the keyboard layout. Check dir-keys below for layouts
# supported here.
#
(def keyboard-layout :qwerty)

(def dir-keys
  (case keyboard-layout
    :qwerty
    (case (string/ascii-lower mod-key)
      "win"
      # Sorry, no HJKL for QWERTY, when mod-key is set to "Win", since
      # `Win + L` means "Lock the Screen", and it cannot be overridden.
      # If you really want to use `Win + L`, check out
      # https://agent-kilo.github.io/jwno/cookbook/bind-win-l-to-something-else.html
      {:left  "Y"
       :down  "U"
       :up    "I"
       :right "O"}

      "alt"
      {:left  "H"
       :down  "J"
       :up    "K"
       :right "L"})

    :colemak
    {:left  "H"
     :down  "N"
     :up    "E"
     :right "I"}

    :colemak-dh
    {:left  "M"
     :down  "N"
     :up    "E"
     :right "I"}

    :dvorak
    {:left  "D"
     :down  "H"
     :up    "T"
     :right "N"}

    (errorf "unsupported layout: %n" keyboard-layout)))

(def hint-key
  (case (string/ascii-lower mod-key)
    "win" "RAlt"
    "alt" "RWin"))

(def hint-key-list
  (case keyboard-layout
    :qwerty     "fjdksleiwocmxz"
    :colemak    "tnserifuwyvmcx"
    :colemak-dh "tnserifuwydhcx"
    :dvorak     "uhetoncrjmqvdi"
    # default
    (errorf "unsupported layout: %n" keyboard-layout)))

#
# Most of Jwno's APIs are exported as methods in these "manager" objects.
# You can inspect them in the Jwno REPL by looking into their prototypes.
# For example, to see all methods available in the window manager object:
#
#     (keys (table/proto-flatten (table/getproto (in jwno/context :window-manager))))
#
(def {:key-manager key-man
      :command-manager command-man
      :window-manager window-man
      :ui-manager ui-man
      :hook-manager hook-man
      :repl-manager repl-man}
  jwno/context)

#
# Replace the key placeholders with the actual key names
#
(defmacro $ [str]
  ~(->> ,str
        (peg/replace-all "${MOD}"  ,mod-key)
        (peg/replace-all "${HINT}" ,hint-key)
        (string)))

#
# A macro to simplify key map definitions. Of course you can call
# :define-key method from the keymap object directly instead.
#
(defmacro k [key-seq cmd &opt doc]
  ~(:define-key keymap ($ ,key-seq) ,cmd ,doc))


#==============================#
#                              #
#  Enabling Optional Features  #
#                              #
#==============================#

#
# The auto-layout module has some code to help you manage the frame
# layout automatically. For example, with the code snippet below, Jwno
# will check for empty frames and close them, when a window is removed.
#
(def auto-close-empty-frame
  (auto-layout/close-empty-frame jwno/context))
(:enable auto-close-empty-frame)

#
# The indicator module provides some visual hints about the current
# frame. The current-frame-area object created here highlights an
# empty active frame by drawing a rectangle in it.
#
(def current-frame-area
  (indicator/current-frame-area jwno/context))
# The margin area reserved around the rectangle. Should match your
# window margin settings.
(put current-frame-area :margin 10)
(:enable current-frame-area)

#
# And if you prefer the old behavior, which shows a simple tooltip for
# every activated frame instead, use the current-frame-tooltip object:
#
#(def current-frame-tooltip
#  (indicator/current-frame-tooltip jwno/context))
#(:enable current-frame-tooltip)

#
# The ui-hint module provides support for the :ui-hint command, which
# is a powerful way to interact with GUIs using your keyboard. See
# the UI Hint Keys section in Key Bindings below.
#
(def ui-hint (ui-hint/ui-hint jwno/context))
(:enable ui-hint)

#
# The scratch-pad module implements a simple cache where we can hide
# our windows, and summon them later as needed. See the Scratch Pad Keys
# section in Key Bindings below.
#
(def scratch-pad (scratch-pad/scratch-pad jwno/context))
(:enable scratch-pad)

#
# The layout-history module provides support for saving and restoring
# layout history. See the Layout History Keys section in Key Bindings
# below.
#
(def layout-history (layout-history/layout-history jwno/context))
#
# If you want Jwno to remember layouts across restarts, uncomment the
# following line to use a backing file. The backing file will be saved
# alongside this config file by default.
#
#(put layout-history :backing-file "layout-history.jdn")
(:enable layout-history true true)


#==========================#
#                          #
#  Key Bindings (Keymaps)  #
#                          #
#==========================#

#
# A transient keymap for resizing frames, so that we don't have
# to hold the modifier keys all the time.
#
# Transient keymaps are activated by :push-keymap commands, and
# deactivated by :pop-keymap commands. See the definition for
# `${MOD} + S` key binding below.
#
(def resize-mode-keymap
  (let [keymap (:new-keymap key-man)]
    (k (in dir-keys :down)  [:resize-frame 0 -100])
    (k (in dir-keys :up)    [:resize-frame 0 100])
    (k (in dir-keys :left)  [:resize-frame -100 0])
    (k (in dir-keys :right) [:resize-frame 100 0])

    (k "="         :balance-frames)
    (k ";"         [:zoom-in 0.7])
    (k "Shift + ;" [:zoom-in 0.3])
    #
    # In a transient keymap, make sure a :pop-keymap binding is defined,
    # or there will be no way to deactivate this keymap.
    #
    (k "Enter" :pop-keymap)
    (k "Esc"   :pop-keymap)

    keymap))

#
# A transient keymap for moving windows around. See the
# definition for `${MOD} + C` key binding below.
#
(def yank-mode-keymap
  (let [keymap (:new-keymap key-man)]
    (each dir [:down :up :left :right]
      (k (in dir-keys dir) [:move-window dir]))

    (k "Enter" :pop-keymap)
    (k "Esc"   :pop-keymap)

    keymap))

#
# A transient keymap for adjusting transparency for the
# current window. See the definition for `${MOD} + A` key
# binding below.
#
(def alpha-mode-keymap
  (let [keymap (:new-keymap key-man)]
    (k (in dir-keys :down) [:change-window-alpha -25])
    (k (in dir-keys :up)   [:change-window-alpha 25])

    (k "Enter" :pop-keymap)
    (k "Esc"   :pop-keymap)

    keymap))

#
# Jwno commands can accept closures/functions as arguments.
# For example, the :split-frame command accepts a function
# to adjust windows/frames after the splitting is done. Below
# is such a function to move the activated window into the
# new empty frame, and activate (move focus to) that frame.
# See the definitions for `${MOD} + ,` and `${MOD} + .` key bindings
# below.
#
(defn move-window-after-split [frame]
  (def all-sub-frames (in frame :children))
  (def all-wins (in (first all-sub-frames) :children))
  (def move-to-frame (in all-sub-frames 1))
  (when (>= (length all-wins) 2)
    (:add-child move-to-frame (:get-current-window frame)))
  (:activate move-to-frame))

#
# Here's another function to automatically move the focused
# window to a new frame, after :insert-frame command. See the
# definitions for `${MOD} + Q  I` and `${MOD} + Q  Shift + I` key
# bindings below.
#
(defn move-window-after-insert [dir frame]
  (def sibling
    (case dir
      :before (:get-next-sibling frame)
      :after  (:get-prev-sibling frame)))
  (def all-wins (in sibling :children))
  (when (>= (length all-wins) 2)
    (:add-child frame (:get-current-window sibling)))
  (:activate frame))

#
# Used in the :summon command to match a window by its
# executable (.exe) file name.
#
(defn match-exe-name [exe-name]
  (fn [win]
    (if-let [win-exe (:get-exe-path win)]
      (string/has-suffix? (string "\\" exe-name) win-exe)
      false)))

#
# We build our root keymap below. Make sure to call the :set-keymap
# method from the key-manager object with the new keymap, or Jwno
# will not respond to any key events at all.
#
# The most straightforward way to understand Jwno commands is to
# simply try out the bindings below. Some commands need more than
# one window or frame to have any effect, though.
#
(defn build-keymap [key-man]
  (let [keymap (:new-keymap key-man)]

    #-----------------#
    #  Basic Commands #
    #-----------------#

    (k "${MOD} + Shift + /" :show-root-keymap)
    (k "${MOD} + Shift + Q" :quit)
    (k "${MOD} + R"         :retile)

    #-------------------------------#
    #  Window And Frame Operations  #
    #-------------------------------#

    (k "${MOD} + Shift + C" :close-window-or-frame)
    (k "${MOD} + Shift + F" :close-frame)

    (k "${MOD} + ," [:split-frame :horizontal 2 [0.5] move-window-after-split]
       "Split current frame horizontally")
    (k "${MOD} + ." [:split-frame :vertical   2 [0.5] move-window-after-split]
       "Split current frame vertically")
    (k "${MOD} + =" :balance-frames)
    (k "${MOD} + ;"         [:zoom-in 0.7])
    (k "${MOD} + Shift + ;" [:zoom-in 0.3])
    (k "${MOD} + F" :fill-monitor)

    (k "${MOD} + P" :cascade-windows-in-frame)

    (k (string "${MOD} + " (in dir-keys :down))  [:enum-frame :next])
    (k (string "${MOD} + " (in dir-keys :up))    [:enum-frame :prev])
    (k (string "${MOD} + " (in dir-keys :left))  [:enum-window-in-frame :prev])
    (k (string "${MOD} + " (in dir-keys :right)) [:enum-window-in-frame :next])

    (each dir [:down :up :left :right]
      (k (string "${MOD} + Ctrl + "  (in dir-keys dir)) [:adjacent-frame dir])
      (k (string "${MOD} + Shift + " (in dir-keys dir)) [:move-window dir]))

    (k "${MOD} + S" [:push-keymap resize-mode-keymap]
       "Resize mode")
    (k "${MOD} + C" [:push-keymap yank-mode-keymap]
       "Yank mode")

    (k "${MOD} + Shift + S" :frame-to-window-size)

    (k "${MOD} + A" [:push-keymap alpha-mode-keymap]
       "Alpha mode")

    #
    # Below are less frequently used commands, grouped by prefix keys
    #
    # Window-specific commands start with `${MOD} + W`
    #
    (k "${MOD} + W  Esc"   :nop "Cancel")
    (k "${MOD} + W  Enter" :nop "Cancel")
    (k "${MOD} + W  D" :describe-window)
    (k "${MOD} + W  M" :manage-window)
    (k "${MOD} + W  I" :ignore-window)

    #
    # Frame-specific commands start with `${MOD} + Q`
    #
    (k "${MOD} + Q  Esc"   :nop "Cancel")
    (k "${MOD} + Q  Enter" :nop "Cancel")
    (k "${MOD} + Q  C"     :close-frame)
    (k "${MOD} + Q  F"     :flatten-parent)
    (k "${MOD} + Q  I"         [:insert-frame :after  (fn [fr] (move-window-after-insert :after fr))]
       "Insert a new frame after the current frame")
    (k "${MOD} + Q  Shift + I" [:insert-frame :before (fn [fr] (move-window-after-insert :before fr))]
       "Insert a new frame before the current frame")
    (k "${MOD} + Q  R"         :rotate-sibling-frames
       "Rotate sibling frames")
    (k "${MOD} + Q  Shift + R" [:rotate-sibling-frames nil nil 0]
       "Rotate monitors")
    (k "${MOD} + Q  V"         :reverse-sibling-frames
       "Reverse sibling frames")
    (k "${MOD} + Q  Shift + V" [:reverse-sibling-frames 0]
       "Reverse monitors")
    (k "${MOD} + Q  D"         :toggle-parent-direction
       "Toggle parent direction")
    (k "${MOD} + Q  Shift + D" [:toggle-parent-direction true 1]
       "Toggle monitor direction (flip layout)")

    #----------------------------#
    #  Launching External Tools  #
    #----------------------------#

    #
    # When a series of keys are specified, sub-keymaps are automatically
    # defined. They can be used as multi-level menus. Press the first
    # key combo, and the keys defined in the next level will be shown in
    # the top-left corner of your current monitor by default.
    #
    # Here we're using `${MOD} + Enter` as a "launcher prefix", to group all
    # our tool-launching key bindings together.
    #

    #
    # The :nop command does... nothing. It's usually used to cancel a
    # multi-level keymap.
    #
    (k "${MOD} + Enter  Esc" :nop
       "Cancel")
    (k "${MOD} + Enter  Enter" :nop
       "Cancel")

    #
    # The :summon command first searches for an existing window that
    # matches the rules we specified. It launches a program by running
    # the provided command line if no matching window can be found.
    #
    (k "${MOD} + Enter  T" [:summon
                            (match-exe-name "WindowsTerminal.exe")
                            true
                            "wt.exe"]
       "Summon Terminal")

    #
    # Some programs (such as Emacs here) would keep the input/output
    # pipes open, blocking Jwno when it exits. Use powershell or cmd
    # to launch the program indirectly in this case.
    #
    (def emacs-cmd
      ["pwsh.exe" "-Command" "Start-Process runemacs.exe"])
    (k "${MOD} + Enter  E" [:summon
                            (match-exe-name "emacs.exe")
                            true
                            ;emacs-cmd]
       "Summon Emacs")

    #
    # When no command line is provided, :summon only searches for the
    # window, and shows a message if no matching window can be found.
    #
    (k "${MOD} + Enter  F" [:summon (match-exe-name "firefox.exe")]
       "Summon Firefox")

    #
    # The :exec command always runs the provided command line.
    #
    (def dev-shell-cmd
      ["wt.exe" "pwsh.exe" "-NoExit" "-Command" "& \"$Env:VS_TOOLS_DIR\\Launch-VsDevShell.ps1\" -Arch amd64 -SkipAutomaticLocation"])
    (k "${MOD} + Enter  D" [:exec
                            true
                            ;dev-shell-cmd]
       "Launch VS Dev Shell")

    (k "${MOD} + Enter  R" [:repl true "127.0.0.1" 9999]
       "Launch Jwno REPL")

    #
    # Ahem, nothing interesting here. Move on.
    #
    (let [win-enter-key (first (:parse-key keymap ($ "${MOD} + Enter")))
          win-enter-map (:get-key-binding keymap win-enter-key)]
      (:define-key win-enter-map
                   "Up Up Down Down Left Right Left Right B A"
                   :grant-lives))

    #--------------------#
    #  Scratch Pad Keys  #
    #--------------------#

    #
    # The scratch pad's :summon-to command works like the :summon
    # command above, but it puts the summoned window into the scratch
    # pad.
    #
    # There can be multiple instances of scratch pads, so we use the
    # scratch pad's :command-name method here to get the unique command
    # for manipulating this specific scratch pad instance.
    #
    (k "${MOD} + Enter  N" [(:command-name scratch-pad :summon-to)
                            (fn [_hwnd _uia exe]
                              (string/has-suffix? "\\notepad.exe" exe))
                            10
                            "notepad.exe"]
       "Summon Notepad to scratch_ pad")

    #
    # Here's a more complex example. Edge web app windows show different
    # titles before and after the page is fully loaded, so we need slightly
    # more complex matching rules.
    #
    (def google-translate-match-fn
      (fn [_hwnd uia exe]
        (def name (:get_CachedName uia))
        (and
          (or
            (string/has-prefix? "translate.google.com" name)
            (= name "Google Translate"))
          (string/has-suffix? "\\msedge.exe" exe))))
    (def google-translate-cmd
      ["C:\\Program Files (x86)\\Microsoft\\Edge\\Application\\msedge.exe" "--app=https://translate.google.com"])
    (k "${MOD} + Enter  G" [(:command-name scratch-pad :summon-to)
                            google-translate-match-fn
                            10
                            ;google-translate-cmd]
       "Summon Google Translate to scratch pad")

    #
    # More key bindings for scratch-pad
    #
    (k "${MOD} + Enter  H"    (:command-name scratch-pad :hide)
       "Hide scratch pad")
    (k "${MOD} + Enter  S  S" (:command-name scratch-pad :show)
       "Show scratch pad")
    (k "${MOD} + Enter  S  N" [(:command-name scratch-pad :show) :next]
       "Next window in scratch pad")
    (k "${MOD} + Enter  S  P" [(:command-name scratch-pad :show) :prev]
       "Previous window in scratch pad")
    (k "${MOD} + Enter  S  H" (:command-name scratch-pad :hide)
       "Hide scratch pad")
    (k "${MOD} + Enter  S  T" (:command-name scratch-pad :toggle)
       "Toggle scratch pad")
    (k "${MOD} + Enter  S  A" (:command-name scratch-pad :add-to)
       "Add current window to scratch pad")
    (k "${MOD} + Enter  S  R" (:command-name scratch-pad :remove-from)
       "Remove the first window from scratch pad")
    (k "${MOD} + Enter  S  Shift + R" (:command-name scratch-pad :remove-all-from)
       "Remove all windows from scratch pad")
    (k "Win + Enter  S  Esc"   :nop "Cancel")
    (k "Win + Enter  S  Enter" :nop "Cancel")

    #----------------#
    #  UI Hint Keys  #
    #----------------#

    #
    # The default :ui-hint command shows all interactable UI elements
    #
    (k "${HINT}  ${HINT}"
       [:ui-hint hint-key-list]
       "Show all interactable elements")

    #
    # You can pass simple rules to match properties
    # (https://learn.microsoft.com/en-us/windows/win32/winauto/uiauto-automation-element-propids),
    # or any nested :and, :or, :not combinations of them.
    #
    # Here we check the control type (UIA_ControlTypePropertyId), and we
    # want only Button (UIA_ButtonControlTypeId) and CheckBox (UIA_CheckBoxControlTypeId)
    # elements.
    #
    (k "${HINT}  B"
       [:ui-hint
        hint-key-list
        (ui-hint/uia-hinter
         :condition [:or
                     [:property UIA_ControlTypePropertyId UIA_ButtonControlTypeId]
                     [:property UIA_ControlTypePropertyId UIA_CheckBoxControlTypeId]])]
       "Show all buttons")

    #
    # You can also choose what to do with the selected element. Here
    # we simply :click on it instead of invoking its default action.
    #
    (k "${HINT}  C"
       [:ui-hint
        hint-key-list
        (ui-hint/uia-hinter
         :action :click)]
       "Show all interactable elements, and click on the selected one")

    (k "${HINT}  D"
       [:ui-hint
        hint-key-list
        (ui-hint/uia-hinter
         :action :double-click)]
       "Show all interactable elements, and double-click on the selected one")

    #
    # More complex property-matching
    #
    (k "${HINT}  E"
       [:ui-hint
        hint-key-list
        (ui-hint/uia-hinter
         :condition [:and
                     [:or
                      [:property UIA_ControlTypePropertyId UIA_EditControlTypeId]
                      [:property UIA_ControlTypePropertyId UIA_ComboBoxControlTypeId]]
                     [:property UIA_IsKeyboardFocusablePropertyId true]])]
       "Show all editable fields")

    (k "${HINT}  F"
       [:ui-hint
        hint-key-list
        (ui-hint/uia-hinter
         :condition [:property UIA_IsKeyboardFocusablePropertyId true]
         :action :focus)]
       "Show all focusable elements, and set input focus to the selected one")

    (k "${HINT}  I"
       [:ui-hint
        hint-key-list
        (ui-hint/uia-hinter
         :condition [:property UIA_ControlTypePropertyId UIA_ListItemControlTypeId])]
       "Show all list item elements")

    (k "${HINT}  L"
       [:ui-hint
        hint-key-list
        (ui-hint/uia-hinter
         :condition [:property UIA_ControlTypePropertyId UIA_HyperlinkControlTypeId])]
       "Show all hyperlinks")

    (k "${HINT}  M"
       [:ui-hint
        hint-key-list
        (ui-hint/uia-hinter
         :action :middle-click)]
       "Show all interactable elements, and middle-click on the selected one")

    (k "${HINT}  Shift + M"
       [:ui-hint
        hint-key-list
        (ui-hint/uia-hinter
         :action :move-cursor)]
       "Show all interactable elements, and move cursor to the selected one")

    (k "${HINT}  R"
       [:ui-hint
        hint-key-list
        (ui-hint/uia-hinter
         :action :right-click)]
       "Show all interactable elements, and right-click on the selected one")

    (k "${HINT}  T"
       [:ui-hint
        hint-key-list
        (ui-hint/uia-hinter
         :condition [:property UIA_ControlTypePropertyId UIA_TreeItemControlTypeId])]
       "Show all tree item elements")

    #
    # We can also display hints for other entities besides UI Automation
    # elements. Here we use a differen hinter (ui-hint/frame-hinter) to
    # show all (leaf) frames, and activate the selected one.
    #
    (k "${HINT}  N"
       [:ui-hint
        hint-key-list
        (ui-hint/frame-hinter)]
       "Show all frames")

    #
    # Different hinters allow different sets of customization options.
    # For example, frame-hinter allows us to specify what we want to
    # do with the selected frame. Here we choose to close it, along
    # with all the windows inside. And we give the labels a bright orange
    # color, to indicate this is a potentially dangerous operation.
    #
    (k "${HINT}  Shift + N"
       [:ui-hint
        hint-key-list
        (ui-hint/frame-hinter
         :action-fn (fn [fr]
                      (def wm (:get-window-manager fr))
                      (each w (in fr :children)
                        (:close w))
                      (:close fr)
                      (:retile wm (in fr :parent)))
         # The label color, 0xBBGGRR
         :color 0x00a1ff)]
       "Show all frames, and close the selected one")

    (k "${HINT}  G"
       [:ui-hint
        hint-key-list
        (ui-hint/gradual-uia-hinter
         :show-highlights true)]
       "Gradually walk the UI tree")

    (k "${HINT}  Esc"   :nop "Cancel")
    (k "${HINT}  Enter" :nop "Cancel")

    #-----------------------#
    #  Layout History Keys  #
    #-----------------------#

    (k "${MOD} + Z  U" :undo-layout-history)
    (k "${MOD} + Z  R" :redo-layout-history)
    (k "${MOD} + Z  P" :push-layout-history)
    (k "${MOD} + Z  Esc"   :nop "Cancel")
    (k "${MOD} + Z  Enter" :nop "Cancel")

    keymap))


(def root-keymap (build-keymap key-man))
(:set-keymap key-man root-keymap)


#===============#
#               #
#  Using Hooks  #
#               #
#===============#

#
# Some windows declare their abilities incorrectly, and Jwno will not
# manage those windows by default. For example, some windows can be moved,
# but they declared otherwise. In that case you'll need to use this hook
# to match those windows and force Jwno to manage them.
#
# The uia-win object used here is a UIAutomation element, you can do much
# more with it, besides checking the name of the window. See docs from
# Microsoft for details:
#
#     https://learn.microsoft.com/en-us/windows/win32/api/uiautomationclient/nn-uiautomationclient-iuiautomationelement
#
# It's recommended to give a name to the function registered to a filter
# hook, so that we can see in the logs exactly what checks failed when
# debugging.
#
(:add-hook hook-man :filter-forced-window
   (fn user-forced-window-filter [_hwnd uia-win _exe-path _desktop-info]
     (or
       (= "Ubisoft Connect" (:get_CachedName uia-win))
       # Add your own rules here
       )))
#
# When you don't want certain windows to be managed by Jwno, use this
# hook to ignore them.
#
(:add-hook hook-man :filter-window
   (fn user-window-filter [_hwnd uia-win exe-path desktop-info]
     (def desktop-name (in desktop-info :name))

     # Excluded windows
     (not (or
            # This means ALL windows on (virtual) Desktop 2 will be ignored,
            # essentially creating a "floating desktop" that Jwno will NOT
            # manage.
            (= "Desktop 2" desktop-name)
            # Add your own rules here
            ))))

#
# This hook is called when a new window gets managed by Jwno.
#
(:add-hook hook-man :window-created
   (fn [win uia-win _exe-path _desktop-info]
     (put (in win :tags) :anchor :center)
     (put (in win :tags) :margin 10)

     (def class-name (:get_CachedClassName uia-win))
     (cond
       #
       # Here we make some windows transparent, filtering by their
       # class names. You can see a window's class name using the
       # `${MOD} + W  D` key binding (if you are using this example
       # config, see the key binding for :describe-window command
       # above).
       #
       (find |(= $ class-name)
             [# The OS that lacks a decent text editor
              "Emacs"
              # The good old console window (cmd.exe and Jwno's REPL window, etc.)
              "ConsoleWindowClass"
              # Windows Terminal (wt.exe)
              "CASCADIA_HOSTING_WINDOW_CLASS"])
       (:set-alpha win (math/floor (* 256 0.9)))

       (= "#32770" class-name) # Dialog window class
       #
       # Tell Jwno to NOT expand these dialog windows, so that
       # they won't cover the whole parent frame (shrinking can
       # still happen though).
       #
       (put (in win :tags) :no-expand true))))

#
# This hook is called when a monitor's settings get updated. It
# can be triggered by change of display resolution, scale setting
# (DPI), screen work area, or monitor arrangement.
#
(:add-hook hook-man :monitor-updated
   (fn [frame]
     #
     # We don't actually need to set the padding value each time
     # the monitor gets updated, but this is currently the only
     # hook that gets called when a new monitor is connected,
     # so it may be considered as a place to put our init code.
     #
     (put (in frame :tags) :padding 10)))


#===================#
#                   #
#  Custom Commands  #
#                   #
#===================#

#
# You can easily define your own commands. When defining key maps,
# use `[:command-name arg0 arg1 ...]` to invoke commands that
# require arguments, or simply `:command-name` for commands without
# any argument.
#
# Command documentation can be found by evaluating
#
#   (:print-doc (in jwno/context :command-manager) :command-name)
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

(:add-command command-man :grant-lives
   (fn []
     (:show-tooltip
        ui-man
        :grant-lives
        "Congratulations! You've been granted infinite lives ;)"
        nil
        nil
        5000
        :center)))
