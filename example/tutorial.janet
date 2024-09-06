(use jw32/_winuser)
(use jw32/_uiautomation)
(use jw32/_dwmapi)
(use jw32/_util)
(use jwno/util)
(import jwno/log)


(def {:window-manager window-man
      :ui-manager ui-man
      :key-manager key-man
      :command-manager command-man
      :hook-manager hook-man}
  jwno/context)

(def continue-key "Space")


(defn get-first-frame []
  (def cur-frame (:get-current-frame (in window-man :root)))
  (def top-frame (:get-top-frame cur-frame))
  (:get-first-frame top-frame))


(defn calc-rect-center [rect]
  (def [w h] (rect-size rect))
  [(math/round (+ (in rect :left) (/ w 2)))
   (math/round (+ (in rect :top) (/ h 2)))])


(defn slide-name-to-cmd [name]
  (keyword :slide- name))


(defn format-slide-text [slide-id total-slides text]
  (string/format "Jwno Tutorial %n/%n\n\n%s\n\nPress %s to continue."
                 slide-id
                 total-slides
                 text
                 continue-key))


(defn cmd-show-keymap [keymap]
  (:show-tooltip
     ui-man
     :show-keymap
     (:format keymap)
     nil nil
     5000
     :center))
(:add-command command-man :show-keymap cmd-show-keymap)


(def common-keymap
  (let [keymap (:new-keymap key-man)]
    (:define-key keymap
                 "win + q"
                 :quit
                 "Stop this tutorial and quit Jwno")
    (:define-key keymap
                 "win + shift + r"
                 [:repl true "127.0.0.1" 9999])
    keymap))


(def acc-keymap (:new-keymap key-man common-keymap))


(defn build-slide-keymap []
  (let [keymap (:new-keymap key-man acc-keymap)]
    (:define-key keymap
                 "win + enter"
                 [:show-keymap keymap]
                 "Show current keymap")
    keymap))


(defn build-slide-list []
  (def slide-list @[])
  (def env (curenv))
  (def slide-defs
    (filter (fn[[k v]]
              (and (table? v)
                   (in v :slide)))
            (pairs (table/proto-flatten env))))

  (sort slide-defs
        (fn [[k1 v1] [k2 v2]]
          (< (in v1 :slide) (in v2 :slide))))

  (def total-slides (length slide-defs))

  (def first-keymap (build-slide-keymap))
  (var cur-keymap first-keymap)

  (for i 0 total-slides
    (def cur-slide (in slide-defs i))
    (def next-slide (if (>= i (- total-slides 1))
                      nil
                      (in slide-defs (+ i 1))))

    (def [cur-slide-name
          {:slide cur-slide-id
           :value cur-slide-fn}]
      cur-slide)

    # For passing into the command closure below
    (def cur-slide-keymap cur-keymap)
    (:add-command command-man
                  (slide-name-to-cmd cur-slide-name)
                  (fn []
                    (cur-slide-fn cur-slide-id
                                  total-slides
                                  cur-slide-keymap)))

    (when next-slide
      (def [next-slide-name _] next-slide)
      (def next-slide-keymap (build-slide-keymap))
      (:define-key cur-keymap
                   continue-key
                   (slide-name-to-cmd next-slide-name)
                   "Continue")
      (set cur-keymap next-slide-keymap)))

  (def [[first-slide-name & _] & _] slide-defs)
  [(slide-name-to-cmd first-slide-name) first-keymap slide-defs])


(defn negative-window-filter [&]
  false)


(defn show-command-filter [cmd args]
  (def show?
    (find |(= $ cmd)
          [:adjacent-frame
           :move-window
           :resize-frame
           :split-frame
           :enum-window-in-frame]))
  (when show?
    (:show-tooltip
       ui-man
       :show-command
       (string/format "[%n %s]"
                      cmd
                      (string/join (map |(if (function? $)
                                           "..."
                                           (string/format "%n" $)) args)
                                   " "))
       nil nil
       5000
       :top-left))
  true)


(defn after-split-fn [fr]
  (def w (:get-current-window fr))
  (when w
    (:add-child (get-in fr [:children 1]) w)))


(defn intro
  {:slide 1}
  [id total keymap]

  (:set-keymap key-man keymap)

  # Pause window management
  (:add-hook hook-man :filter-window negative-window-filter)

  # Show important commands
  (:add-hook hook-man :filter-command show-command-filter)

  (:show-tooltip
     ui-man
     :tutorial
     (format-slide-text
      id total
      ```
      Hello there! Welcome to this little Jwno tutorial!

      Jwno is a keyboard-centric tiling window manager, we'll be using your keyboard
      a lot, so keep it handy ;)

      Note that Jwno has no key bindings defined by default, the keys we'll use here
      are specifically defined for this tutorial.

      Press Win + Q or right click on Jwno's system tray icon at any time to stop this
      tutorial and exit.
      ```)
     nil nil
     0
     :center))


(defn before-cleanup-desktop
  {:slide 2}
  [id total keymap]

  (:set-keymap key-man keymap)

  (:show-tooltip
     ui-man
     :tutorial
     (format-slide-text
      id total
      ```
      We'll be walking through some of Jwno's window management features.

      To do that, I'll first try to minimize all your windows to make us a clean
      desktop. You can also manually close them before we continue.
      ```)
     nil nil
     0
     :center))


(defn cleanup-desktop
  {:slide 3}
  [id total keymap]

  (:set-keymap key-man keymap)

  (EnumChildWindows
   nil
   (fn [hwnd]
     (try
       (do
         (with-uia [uia-win (:get-hwnd-uia-element window-man hwnd)]
           (with-uia [win-pat (:GetCurrentPatternAs uia-win
                                                    UIA_WindowPatternId
                                                    IUIAutomationWindowPattern)]
             (when (and (= true (:filter-hwnd window-man hwnd uia-win))
                        (not= 0 (:get_CurrentCanMinimize win-pat)))
               (:SetWindowVisualState win-pat WindowVisualState_Minimized)
               (log/info "Minimized window %n" hwnd)))))
       ((err fib)
        (log/info "Failed to minimize window %n: %n\n%s"
                  hwnd
                  err
                  (get-stack-trace fib))))
     TRUE))

  (:show-tooltip
     ui-man
     :tutorial
     (format-slide-text
      id total
      ```
      Next, I'll spawn some Notepad windows for us to manage.
      ```)
     nil nil
     0
     :center))


(defn spawn-windows
  {:slide 4}
  [id total keymap]

  (:set-keymap key-man keymap)

  # Enable window management
  (:remove-hook hook-man :filter-window negative-window-filter)

  (for i 0 3
    (:call-command command-man :exec true "notepad.exe"))

  # ev/spawn so that the event loop has a chance to manage
  # spawned windows before the cascade command is called.
  (ev/spawn
   (ev/sleep 1)
   (:call-command command-man :cascade-windows-in-frame)
   (def w (:get-top-window (:get-current-frame (in window-man :root))))
   (def w-rect (DwmGetWindowAttribute (in w :hwnd) DWMWA_EXTENDED_FRAME_BOUNDS))
   (:show-tooltip
      ui-man
      :tutorial
      (format-slide-text
       id total
       ```
       With our windows in place, let's tile them.
       ```)
      (+ 100 (in w-rect :left)) (+ 100 (in w-rect :top))
      0
      :top-left)))


(defn tile-windows-in-top-frame
  {:slide 5}
  [id total keymap]

  (:set-keymap key-man keymap)

  (:call-command command-man :retile)

  (:show-tooltip
     ui-man
     :tutorial
     (format-slide-text
      id total
      ```
      Jwno calls the screen areas it manages "frames." It will allocate a top-level
      frame that covers the whole desktop area for each monitor by default.

      The area now occupied by the Notepad window is our top frame for this monitor.

      Note that Jwno will show a gap of 20 virtual pixels around the windows in this
      tutorial, so that we can easily visualize frame borders.
      ```)
     nil nil
     0
     :center))


(defn split-top-frame
  {:slide 6}
  [id total keymap]

  (:set-keymap key-man keymap)

  (:call-command command-man :split-frame :horizontal nil nil after-split-fn)

  (:show-tooltip
     ui-man
     :tutorial
     (format-slide-text
      id total
      ```
      Every frame can be split into multiple child frames.

      We just called the :split-frame command and have the top frame split into two
      child frames. This is called a "horizontal split," since after the split, child frames
      are lined up horizontally.

      Don't worry about the colon(:) in front of the command name. Just knowing that
      some names in Jwno start with a colon is sufficient for this tutorial.
      ```)
     nil nil
     0
     :center))


(defn split-more-frames
  {:slide 7}
  [id total keymap]

  (:set-keymap key-man keymap)

  (def first-frame (get-first-frame))
  (:activate window-man first-frame)

  (:call-command command-man :split-frame :vertical nil nil after-split-fn)

  (:show-tooltip
     ui-man
     :tutorial
     (format-slide-text
      id total
      ```
      And we just did a vertical split.
      ```)
     ;(calc-rect-center (in first-frame :rect))
     0
     :center))


(defn about-active-frames
  {:slide 8}
  [id total keymap]

  (:set-keymap key-man keymap)

  (:show-tooltip
     ui-man
     :tutorial
     (format-slide-text
      id total
      ```
      Jwno has the concept of "active frames." An active frame is a frame that
      currently has the input focus. New windows will snap into the active frame,
      and all frame selection commands are relative to the active frame.

      A Frame can be activated in one of these ways:

      1. Switch focus to one of the windows it contains;
      2. Use frame selection commands;
      3. Programmatically call the activation API. 
      ```)
     nil nil
     0
     :center))


(defn moving-focus-across-frames
  {:slide 9}
  [id total keymap]

  (each dir [:left :up :right :down]
    (:define-key acc-keymap (string dir) [:adjacent-frame dir]))
  (:set-keymap key-man keymap)

  (:show-tooltip
     ui-man
     :tutorial
     (format-slide-text
      id total
      ```
      The arrow keys on your keyboard are now bound to frame selection commands.

      Use the arrow keys, or click on the Notepad windows, to observe the switching
      of active frames. There will be a small tooltip appearing in the center of the
      activated frame.

      If you're curious about what command the arrow keys actually called, have a
      look at the top-left corner of your current monitor.
      ```)
     nil nil
     0
     :center))


(defn about-multi-monitor-support
  {:slide 10}
  [id total keymap]

  (:set-keymap key-man keymap)

  (def layout (:get-layout (:get-current-frame (in window-man :root))))
  (def text
    (if (> (length (in layout :children)) 1)
      ```
      I see that you have multiple monitors. Jwno has excellent multi-monitor support,
      and knows about your monitor arrangement, so the frame selection commands also
      work across different monitors, as you may have noticed.

      In other words, you can generally treat your monitors as normal frames.
      ```
      # else
      ```
      Jwno has excellent multi-monitor support, and knows about your monitor
      arrangement. So if you have multiple monitors, the frame selection commands
      would also work across all of them.

      In other words, you can generally treat your monitors as normal frames.
      ```
      ))

  (:show-tooltip
     ui-man
     :tutorial
     (format-slide-text
      id total
      text)
     nil nil
     0
     :center))


(defn split-even-more-frames
  {:slide 11}
  [id total keymap]

  (:set-keymap key-man keymap)

  (def first-frame (get-first-frame))
  (:activate window-man first-frame)
  (:call-command command-man :split-frame :horizontal nil nil after-split-fn)

  (:show-tooltip
     ui-man
     :tutorial
     (format-slide-text
      id total
      ```
      We just called the :split-frame command and did a horizontal split again.
      ```)
     ;(calc-rect-center (in first-frame :rect))
     0
     :center))


(defn about-empty-frames
  {:slide 12}
  [id total keymap]

  (:set-keymap key-man keymap)

  (def first-frame (get-first-frame))
  (:show-tooltip
     ui-man
     :tutorial
     (format-slide-text
      id total
      ```
      As you can see, a frame can be empty.

      And if we open a new window... (I'll spawn an Explorer window for you.)
      ```)
     ;(calc-rect-center (in first-frame :rect))
     0
     :center))


(defn spawn-window-into-empty-frame
  {:slide 13}
  [id total keymap]

  (:set-keymap key-man keymap)

  (def first-frame (get-first-frame))
  (:activate window-man first-frame)    

  (:call-command command-man :exec true "explorer.exe")

  # ev/spawn so that the event loop has a chance to manage
  # the spawned window.
  (ev/spawn
   (:show-tooltip
      ui-man
      :tutorial
      (format-slide-text
       id total
       ```
       The new window will snap into the active frame.
       ```)
      ;(calc-rect-center (in first-frame :rect))
      0
      :center)))


(defn about-enforcing-window-geometries
  {:slide 14}
  [id total keymap]

  (:set-keymap key-man keymap)

  (:show-tooltip
     ui-man
     :tutorial
     (format-slide-text
      id total
      ```
      But Jwno does not enforce window geometries. Instead, it only resizes and
      relocates windows in these situations:

      1. A new window is opened;
      2. The input focus is moved to a window that's not already managed;
      3. The frame layout is changed;
      4. You explicitly request resizing/relocation/retiling.

      You can now freely move or resize any managed windows on the screen. I'll
      retile them for you in the next step.
      ```)
     nil nil
     0
     :center))


(defn about-retiling
  {:slide 15}
  [id total keymap]

  (:define-key acc-keymap "r" :retile)
  (:set-keymap key-man keymap)

  (:call-command command-man :retile)

  (:show-tooltip
     ui-man
     :tutorial
     (format-slide-text
      id total
      ```
      There! The windows are back to where they should be.

      We just called the :retile command. When you temporarily altered the layout
      of your windows, or want to deal with some weird windows that did not snap
      to the frames cleanly, this command may come in handy.

      From now on in this tutorial, you can press the R key if you ever messed up the
      window layout and want to retile.
      ```)
     nil nil
     0
     :center))


(defn about-moving-windows
  {:slide 16}
  [id total keymap]

  (each dir [:left :up :right :down]
    (:define-key acc-keymap (string "shift + " dir) [:move-window dir]))
  (:set-keymap key-man keymap)

  (:show-tooltip
     ui-man
     :tutorial
     (format-slide-text
      id total
      ```
      Now it's time to move our windows around.

      Select a window, and try the arrow keys while holding down Shift ;)

      The Shift + <arrow key> combinations are now bound to window movement commands.
      Likewise, the command that is actually called will be shown in the top-left corner.

      These commands also work across monitors, like the frame selection commands.
      ```)
     nil nil
     0
     :center))


(defn about-overlapping-windows
  {:slide 17}
  [id total keymap]

  (:define-key acc-keymap "n" [:enum-window-in-frame :next])
  (:define-key acc-keymap "p" [:enum-window-in-frame :prev])
  (:set-keymap key-man keymap)

  (:show-tooltip
     ui-man
     :tutorial
     (format-slide-text
      id total
      ```
      But what happened to the windows in a frame when a new window moved in?

      They're still there, but covered by the new window, as you would have expected.
      Alt + Tab to them, and you'll see they still occupy the same frame area.

      There's an :enum-window-in-frame command to flip through these overlapped
      windows.

      Let's try it out - move multiple windows into one frame with Shift + <arrow key>,
      then imagine that frame is a book. The N key will bring you to the next page,
      and P key to the previous page.
      ```)
     nil nil
     0
     :center))


(defn about-cascading-windows-in-frame
  {:slide 18}
  [id total keymap]

  (:define-key acc-keymap "c" :cascade-windows-in-frame)
  (:set-keymap key-man keymap)

  (:show-tooltip
     ui-man
     :tutorial
     (format-slide-text
      id total
      ```
      Sometimes it's too slow to flip through a thick book. Luckily we have the
      :cascade-windows-in-frame command.

      Activate a frame with multiple windows, then press the C key to see it in
      action ;)

      To fit the windows to their frames again, press R to :retile.
      ```)
     nil nil
     0
     :center))


(defn about-resizing-windows
  {:slide 19}
  [id total keymap]

  (:define-key acc-keymap "ctrl + left" [:resize-frame -100 0])
  (:define-key acc-keymap "ctrl + up" [:resize-frame 0 100])
  (:define-key acc-keymap "ctrl + right" [:resize-frame 100 0])
  (:define-key acc-keymap "ctrl + down" [:resize-frame 0 -100])
  (:set-keymap key-man keymap)

  (:show-tooltip
     ui-man
     :tutorial
     (format-slide-text
      id total
      ```
      Of course, Jwno has commands to resize windows too.

      Try Ctrl + <arrow key> combinations ;)

      But note that, when you're resizing a window, you're actually resizing the
      frame it belongs to, and all other windows in the same frame, so that our
      layout stays clean and tidy. You can see from the top-left corner of your
      screen when resizing, that the command is actually called :resize-frame.
      ```)
     nil nil
     0
     :center))


(defn about-window-minimum-size
  {:slide 20}
  [id total keymap]

  (:set-keymap key-man keymap)

  (:show-tooltip
     ui-man
     :tutorial
     (format-slide-text
      id total
      ```
      You may have noticed, some windows have minimum size requirements, but Jwno's
      frames don't have that limitation. So sometimes windows will "overflow" from the
      designated frame area. Unfortunately Jwno has no control over this.
      ```)
     nil nil
     0
     :center))


(def [first-cmd first-keymap slide-defs]
  (build-slide-list))


# ------------------ #

(:add-hook hook-man :window-created
   (fn [win _uia-win _exe-path _desktop-info]
     (put (in win :tags) :margin 10)))

(:add-hook hook-man :monitor-updated
   (fn [frame]
     (put (in frame :tags) :padding 10)))

(:call-command command-man first-cmd)
