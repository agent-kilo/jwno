(log/info "++++++++ HELLO THERE ++++++++")
(log/info "++++++++ Keys in jwno-context: %n ++++++++" (keys jwno-context))


# When running from a compiled binary, this needs the Janet
# env be set up properly beforehand
#(import spork/httpf)


(def key-man (in jwno-context :key-manager))
(def command-man (in jwno-context :command-manager))
(def window-man (in jwno-context :window-manager))
(def hook-man (in jwno-context :hook-manager))


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
        (:transform-window window-man next-win cur-rect)
        (:activate window-man next-win)
        (put cur-rect :left (+ (in cur-rect :left) dx))
        (put cur-rect :top (+ (in cur-rect :top) dy))
        (set next-win (:get-next-child cur-frame next-win)))
      (:transform-window window-man cur-win cur-rect)
      (:activate window-man cur-win))))


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
  (k "win + o" [:focus-mode 0.7])

  (k "win + p" :peek-frame)

  (k "win + n" [:enum-frame :next])
  (k "win + e" [:enum-frame :prev])
  (k "win + i" [:enum-window-in-frame :next])
  (k "win + m" [:enum-window-in-frame :prev])

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
  (k "ralt" [:map-to (:get-key-code key-man "rwin")])

  (log/debug "keymap = %n" keymap)
  keymap)


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
       (:set-hwnd-alpha window-man (in win :hwnd) (math/floor (* 256 0.9)))

       (= "#32770" class-name) # Dialog window class
       (put (in win :tags) :no-expand true))))

(:add-hook hook-man :window-removed
   (fn [dead-win]
     (def parent (in dead-win :parent))
     (when (empty? (in parent :children))
       (:close parent)
       (:retile window-man))))


(:add-command command-man :close-current-window-or-frame
   (fn []
     (def cur-frame (:get-current-frame (in window-man :root)))
     # cur-win will be nil if the current frame is empty.
     (if-let [cur-win (:get-current-window cur-frame)]
       (:close-hwnd window-man (in cur-win :hwnd))
       (do
         (:close cur-frame)
         (:retile window-man)
         (:activate window-man (:get-current-window cur-frame))))))

(:add-command command-man :split-and-move-current-window
   (fn [dir]
     (def root (in window-man :root))
     (def cur-frame (:get-current-frame root))
     (def cur-win (:get-current-window cur-frame))
     (def win-count (length (in cur-frame :children)))
     # Existing windows are all moved to the first new frame
     # after splitting.
     (:split cur-frame dir 2 [0.5])
     (def last-new-frame (last (in cur-frame :children)))
     # Only move the current window away when there's at
     # least another window in the old frame
     (when (>= win-count 2)
       # The child will be automatically removed from its
       # former parent.
       (:add-child last-new-frame cur-win))
     (:retile window-man cur-frame)
     (:activate window-man last-new-frame)))

(:add-command command-man :peek-frame
   (fn [] (cascade-windows)))
