(use jw32/_winuser)
(use jw32/_uiautomation)
(use jw32/_dwmapi)
(use jw32/_util)
(use jwno/util)
(import jwno/log)
(import jwno/auto-layout)
(import jwno/indicator)


(def {:window-manager window-man
      :ui-manager ui-man
      :key-manager key-man
      :command-manager command-man
      :hook-manager hook-man}
  jwno/context)


(def current-frame-tooltip (indicator/current-frame-tooltip jwno/context))
(:enable current-frame-tooltip)


(def *auto-close-empty-frame*
  (auto-layout/close-empty-frame jwno/context))


(def CONTINUE-KEY "Space")
(def *allowed-commands*
  @{:nop true
    :quit true
    :exec true
    :cascade-windows-in-frame true
    :retile true})
(var *next-slide-cmd* nil)


(defn enable-command [cmd &opt pred]
  (default pred true)
  (put *allowed-commands* cmd pred))


(defn disable-command [cmd]
  (put *allowed-commands* cmd nil))


(defn disable-continue-key [keymap]
  (def [next-key] (:parse-key keymap CONTINUE-KEY))
  (def next-slide-binding (:get-key-binding keymap next-key))
  (put keymap next-key nil)
  [next-key (in next-slide-binding :cmd)])


(defn arm-next-slide-trigger [trigger-cmd next-cmd &opt pred]
  (if pred
    (set *next-slide-cmd* [next-cmd pred])
    (set *next-slide-cmd* [next-cmd (fn [cmd _args] (= cmd trigger-cmd))])))


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


(defn format-slide-text [slide-id total-slides text &opt show-continue?]
  (default show-continue? true)

  (string/format "Jwno Tutorial %n/%n\n\n%s%s"
                 slide-id
                 total-slides
                 text
                 (if show-continue?
                   (string/format "\n\nPress %s to continue." CONTINUE-KEY)
                   "")))


(defn cmd-show-keymap [keymap]
  (:show-tooltip
     ui-man
     :show-keymap
     (:format keymap)
     nil nil
     5000
     :center))
(:add-command command-man :show-keymap cmd-show-keymap)


(def *common-keymap*
  (let [keymap (:new-keymap key-man)]
    (:define-key keymap
                 "Win + Shift + Q"
                 :quit
                 "Stop this tutorial and quit Jwno")

    (:define-key keymap
                 "Win + Enter Enter"
                 :nop
                 "Cancel")
    (:define-key keymap
                 "Win + Enter Esc"
                 :nop
                 "Cancel")
    (:define-key keymap
                 "Win + Enter R"
                 [:repl true "127.0.0.1" 9999]
                 "Launch Jwno REPL")
    keymap))


(def *acc-keymap* (:new-keymap key-man *common-keymap*))


(defn build-slide-keymap []
  (let [keymap (:new-keymap key-man *acc-keymap*)]
    (:define-key keymap
                 "Win + Shift + /"
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
    (enable-command (slide-name-to-cmd cur-slide-name))

    (when next-slide
      (def [next-slide-name _] next-slide)
      (def next-slide-keymap (build-slide-keymap))
      (:define-key cur-keymap
                   CONTINUE-KEY
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
          [:enum-frame
           :adjacent-frame
           :retile
           :move-window
           :resize-frame
           :frame-to-window-size
           :split-frame
           :enum-window-in-frame
           :cascade-windows-in-frame]))
  (when show?
    (:show-tooltip
       ui-man
       :show-command
       (if (and args (> (length args) 0))
         (string/format "[%n %s]"
                        cmd
                        (string/join (map |(if (function? $)
                                             "..."
                                             (string/format "%n" $)) args)
                                     " "))
         (string/format "%n" cmd))
       nil nil
       5000
       :top-left))
  true)


(defn demo-command-filter [cmd args]
  (def allowed?
    (if-let [pred (in *allowed-commands* cmd)]
      (if (function? pred)
        (pred cmd args)
        pred)
      false))

  (when allowed?
    (match *next-slide-cmd*
      nil
      :noop

      [next-cmd pred]
      (when (pred cmd args)
        (set *next-slide-cmd* nil)
        # Put it in the event queue
        (ev/spawn
         (:call-command command-man next-cmd)))

      next-cmd
      (do
        (set *next-slide-cmd* nil)
        # Put it in the event queue
        (ev/spawn
         (:call-command command-man next-cmd)))))

  (unless allowed?
    (:show-tooltip ui-man :command-disabled "Command disabled ;)"))

  allowed?)


(defn move-window-after-split [frame]
  (def all-sub-frames (in frame :children))
  (def all-wins (in (first all-sub-frames) :children))
  (def move-to-frame (in all-sub-frames 1))
  (when (>= (length all-wins) 2)
    (:add-child move-to-frame (:get-current-window frame)))
  (:activate move-to-frame))


# ================== Slides goes here ==================


(defn intro
  {:slide 1}
  [id total keymap]

  (:set-keymap key-man keymap)

  # Pause window management
  (:add-hook hook-man :filter-window negative-window-filter)

  (:show-tooltip
     ui-man
     :tutorial
     (format-slide-text
      id total

```Hello there! Welcome to this little Jwno tutorial!

I'm Agent Kilo, the guy who created Jwno. But you can call me anything you want, I can't hear you anyway.

Jwno is a keyboard-centric tiling window manager, we'll be using your keyboard a lot, so keep it handy ;) Here I assume that you use the US Qwerty keyboard layout.

Note that Jwno has no key bindings defined by default. The keys we'll use here are the same keys defined in example-config.janet. After going through this tutorial, you should be able to use that config file directly, or as a starting point for your own config.

Press Win + Shift + Q or right click on Jwno's system tray icon at any time to stop this tutorial and exit.```

      )
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

```We'll be walking through some of Jwno's window management features.

But I'll try to minimize all your windows first, to make us a clean desktop. You can also manually close them before we continue.```

      )
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

```And... let there be Notepad!```
      
      )
     nil nil
     0
     :center))


(defn spawn-windows
  {:slide 4}
  [id total keymap]

  (:set-keymap key-man keymap)

  # Enable window management
  (:remove-hook hook-man :filter-window negative-window-filter)

  (for i 0 2
    (:call-command command-man :exec true "notepad.exe"))

  # ev/spawn so that the event loop has a chance to manage
  # spawned windows before the cascade command is called.
  (ev/spawn
   (ev/sleep 1)
   # Wait for the windows we spawned
   (while (or (nil? (:get-current-frame (in window-man :root)))
              (< (length (:get-all-windows (in window-man :root))) 2))
     (ev/sleep 1))
   (:call-command command-man :cascade-windows-in-frame)
   (def w (:get-top-window (:get-current-frame (in window-man :root))))
   (def w-rect (DwmGetWindowAttribute (in w :hwnd) DWMWA_EXTENDED_FRAME_BOUNDS))
   (:show-tooltip
      ui-man
      :tutorial
      (format-slide-text
       id total

```Notepad is good. It comes with Windows™, it's slim and fast... Oh, right, we were talking about Jwno.```

       )
      (+ 100 (in w-rect :left)) (+ 100 (in w-rect :top))
      0
      :top-left)))


(defn tile-windows-in-top-frame
  {:slide 5}
  [id total keymap]

  # We'll activate the next slide automatically, so disable the continue key
  (def [next-key next-cmd] (disable-continue-key keymap))
  (:define-key *acc-keymap* "win + ," [:split-frame :horizontal 2 [0.5] move-window-after-split])
  (:set-keymap key-man keymap)

  (:call-command command-man :retile)

  (enable-command :split-frame (fn [_cmd [dir & _rest]] (= dir :horizontal)))
  # Automatically call the next slide after :split-frame command
  (arm-next-slide-trigger :split-frame next-cmd)

  (:show-tooltip
     ui-man
     :tutorial
     (format-slide-text
      id total

```Jwno calls the screen areas it manages "frames." A top-level frame that covers the whole desktop area is allocated for each monitor by default.

The area now occupied by the Notepad window is our top frame for this monitor.

To help us visualize frame borders, I configured Jwno to show a gap of 20 pixels around the windows in this tutorial.

Try Pressing Win + , (the comma key) on your keyboard ;)```

      false)
     nil nil
     0
     :center))


(defn split-top-frame
  {:slide 6}
  [id total keymap]

  (disable-command :split-frame)

  (def [next-key next-cmd] (disable-continue-key keymap))
  (:define-key *acc-keymap* "win + ." [:split-frame :vertical 2 [0.5] move-window-after-split])
  (:set-keymap key-man keymap)

  (enable-command :split-frame (fn [_cmd [dir & _rest]] (= dir :vertical)))
  (arm-next-slide-trigger :split-frame next-cmd)

  (:show-tooltip
     ui-man
     :tutorial
     (format-slide-text
      id total

```We just called the :split-frame command and have the top frame split into two child frames.

Every frame can be split into multiple children. And the children can be split again. And again. And again if you like. Just don't tell anyone we're splitting children in half.

...Aaaanyway, this is called a "horizontal split," since after the split, the child frames are lined up horizontally.

(Don't worry about the colon(:) in front of the command name. Just knowing that some names in Jwno start with a colon is sufficient for this tutorial.)

Now try Win + . (the period key).```

      false)
     nil nil
     0
     :center))


(defn about-vertical-split-and-empty-frames
  {:slide 7}
  [id total keymap]

  (disable-command :split-frame)

  (:set-keymap key-man keymap)

  (def cur-frame (:get-current-frame (in window-man :root)))

  (:show-tooltip
     ui-man
     :tutorial
     (format-slide-text
      id total

```We just did a vertical split. Notice that a frame can be empty.

And if we open a new window... (I'll spawn an Explorer window for you.)```

      )
     ;(calc-rect-center (in cur-frame :rect))
     0
     :center))


(defn spawn-window-into-empty-frame
  {:slide 8}
  [id total keymap]

  (:set-keymap key-man keymap)

  (:call-command command-man :exec true "explorer.exe")

  # ev/spawn so that the event loop has a chance to manage
  # the spawned window.
  (ev/spawn
   (def cur-frame (:get-current-frame (in window-man :root)))
   (:show-tooltip
      ui-man
      :tutorial
      (format-slide-text
       id total
       
```The new window will snap into the active frame.```
       
       )
      ;(calc-rect-center (in cur-frame :rect))
      0
      :center)))


(defn about-active-frames
  {:slide 9}
  [id total keymap]

  (:set-keymap key-man keymap)

  (:show-tooltip
     ui-man
     :tutorial
     (format-slide-text
      id total

```An active frame is a frame that currently has the input focus. New windows will snap into the active frame, and all frame selection commands are relative to that frame too.

A frame can be activated in one of these ways:

1. Switch focus to one of the windows it contains;
2. Use frame selection commands;
3. Programmatically call the activation API.```

       )
     nil nil
     0
     :center))


(defn about-enumerating-frames
  {:slide 10}
  [id total keymap]

  (:set-keymap key-man keymap)

  (:show-tooltip
     ui-man
     :tutorial
     (format-slide-text
      id total

```There're two main frame selection commands, the first one is :enum-frame.

Internally, frames are organized in a tree-like structure, and :enum-frame can help you traverse the leaf frames in order. If that doesn't make sense to you, just know that :enum-frame can generally switch to the next frame from top to bottom, and from left to right, or in the opposite direction.```

      )
     nil nil
     0
     :center))


(defn enumerating-frames
  {:slide 11}
  [id total keymap]

  (enable-command :enum-frame)

  (:define-key *acc-keymap* "Win + U" [:enum-frame :next])
  (:define-key *acc-keymap* "Win + I" [:enum-frame :prev])
  (:set-keymap key-man keymap)

  (:show-tooltip
     ui-man
     :tutorial
     (format-slide-text
      id total

```You can now use Win + U and Win + I to call the :enum-frame command and activate the next/prev frame. The actual commands we bound to these keys will be shown in the top-left corner of your current monitor.

Use those keys, or click on the managed windows, to observe the activation of frames. There will be a small tooltip appearing in the center of the activated frame.

After activating a frame, you can restore your minimized windows, or open new windows, to see how Jwno takes them under management.```

      )
     nil nil
     0
     :center))


(defn about-adjacent-frames
  {:slide 12}
  [id total keymap]

  (:set-keymap key-man keymap)
  
  (:show-tooltip
     ui-man
     :tutorial
     (format-slide-text
      id total

```And here comes the second frame selection command, :adjacent-frame.

It just does what it says: activating an adjacent frame. You can specify a direction for it to know where to go next.```

      )
     nil nil
     0
     :center))


(defn moving-to-adjacent-frames
  {:slide 13}
  [id total keymap]

  (enable-command :show-keymap)
  (enable-command :adjacent-frame)

  (:define-key *acc-keymap* "Win + Ctrl + Y" [:adjacent-frame :left])
  (:define-key *acc-keymap* "Win + Ctrl + U" [:adjacent-frame :down])
  (:define-key *acc-keymap* "Win + Ctrl + I" [:adjacent-frame :up])
  (:define-key *acc-keymap* "Win + Ctrl + O" [:adjacent-frame :right])
  (:set-keymap key-man keymap)
  
  (:show-tooltip
     ui-man
     :tutorial
     (format-slide-text
      id total

```The Win + Ctrl + Y/U/I/O keys are now bound to moving the focus left/down/up/right.

Please try them out, and maybe compare them with Win + U and Win + I.

Now that we have a bunch of keys to remember, you can press Win + Shift + / (the slash key) to see a list of all defined keys. But some of those commands are now disabled to keep us oriented.```

      )
     nil nil
     0
     :center))


(defn about-multi-monitor-support
  {:slide 14}
  [id total keymap]

  (:set-keymap key-man keymap)

  (def layout (:get-layout (:get-current-frame (in window-man :root))))
  (def text
    (if (> (length (in layout :children)) 1)

```I see that you have multiple monitors. You may have noticed, the frame selection commands also work across different monitors.

In other words, you can generally treat your monitors as normal frames.```

      # else

```Jwno has excellent multi-monitor support, and knows about your monitor arrangement. So if you have multiple monitors, the frame selection commands would also work across all of them.

In other words, you can generally treat your monitors as normal frames.```

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


(defn about-enforcing-window-geometries
  {:slide 15}
  [id total keymap]

  (:set-keymap key-man keymap)

  (:show-tooltip
     ui-man
     :tutorial
     (format-slide-text
      id total

```An important feature of Jwno is that, it does not always enforce window geometries. Instead, it only resizes and relocates windows in these situations:

1. A new window is opened;
2. The input focus is moved to a window that's not already managed;
3. The frame layout is changed;
4. You explicitly request resizing/relocation/retiling.

In other words, after you manually moved/resized your windows, they will stay the way they are, until you change the frame layout or tell Jwno to retile them. This will be handy when you want to temporarily resize or rearrange some windows, for example.

You can now freely move or resize any managed windows on the screen. I'll retile them for you in the next step.```

      )
     nil nil
     0
     :center))


(defn about-retiling
  {:slide 16}
  [id total keymap]

  (enable-command :retile)

  (:define-key *acc-keymap* "Win + R" :retile)
  (:set-keymap key-man keymap)

  (:call-command command-man :retile)

  (:show-tooltip
     ui-man
     :tutorial
     (format-slide-text
      id total

```There! The windows are back to where they should be.

We just called the :retile command. From now on, you can press Win + R if the window layout got messed up and you want to retile.```

      )
     nil nil
     0
     :center))


(defn about-moving-windows
  {:slide 17}
  [id total keymap]

  (enable-command :move-window)

  (:define-key *acc-keymap* "Win + Shift + Y" [:move-window :left])
  (:define-key *acc-keymap* "Win + Shift + U" [:move-window :down])
  (:define-key *acc-keymap* "Win + Shift + I" [:move-window :up])
  (:define-key *acc-keymap* "Win + Shift + O" [:move-window :right])
  (:set-keymap key-man keymap)

  (:show-tooltip
     ui-man
     :tutorial
     (format-slide-text
      id total

```Now it's time to move our windows around.

Select a window, and try Win + Shift + Y/U/I/O ;)

These keys are now bound to window movement commands. They also work across monitors, like the frame selection commands.```

      )
     nil nil
     0
     :center))


(defn about-overlapping-windows
  {:slide 18}
  [id total keymap]

  (:set-keymap key-man keymap)

  (:show-tooltip
     ui-man
     :tutorial
     (format-slide-text
      id total

```But what happened to the windows in a frame when a new window moved in?

They're still there, but covered by the new window, as you would have expected. We have the :enum-window-in-frame command to flip through these overlapped windows.```

      )
     nil nil
     0
     :center))


(defn trying-out-enum-window-in-frame
  {:slide 19}
  [id total keymap]

  (enable-command :enum-window-in-frame)

  (:define-key *acc-keymap* "Win + O" [:enum-window-in-frame :next])
  (:define-key *acc-keymap* "Win + Y" [:enum-window-in-frame :prev])
  (:set-keymap key-man keymap)

  (:show-tooltip
     ui-man
     :tutorial
     (format-slide-text
      id total

```Let's try it out - move multiple windows into one frame with the window movement keys, or simply open some new windows, then imagine that frame is a book. Win + O will bring you to the next page, and Win + Y to the previous page.

Remember, you can still use Win + U or Win + I to switch the "book" you want to read.

(This book analogy is the main reason these keys are arranged like this. You don't like it? No worries! Jwno is quite customizable, just define your own keys.)```

      )
     nil nil
     0
     :center))


(defn about-cascading-windows-in-frame
  {:slide 20}
  [id total keymap]

  (enable-command :cascade-windows-in-frame)

  (:define-key *acc-keymap* "Win + P" :cascade-windows-in-frame)
  (:set-keymap key-man keymap)

  (:show-tooltip
     ui-man
     :tutorial
     (format-slide-text
      id total

```Sometimes it's too slow to flip through a thick book. Luckily we have the :cascade-windows-in-frame command.

Activate a frame containing multiple windows, then press Win + P to see it in action ;)

To fit the windows to their frames again, press Win + R to :retile.```

      )
     nil nil
     0
     :center))


(defn transient-keymaps-and-resizing-windows
  {:slide 21}
  [id total keymap]

  (enable-command :resize-frame)

  (def [next-key next-cmd] (disable-continue-key keymap))
  # Special case: go to the next slide when the resize kemap is popped
  (var resize-keymap-popped-hook nil)
  (set resize-keymap-popped-hook
    (fn [_km]
      (:remove-hook hook-man :keymap-popped resize-keymap-popped-hook)
      # ev/spawn to put it in the event queue
      (ev/spawn (:call-command command-man next-cmd))))
  (:add-hook hook-man :keymap-popped resize-keymap-popped-hook)

  (def resize-mode-keymap
    (let [km (:new-keymap key-man)]
      (:define-key km "Y" [:resize-frame -100 0])
      (:define-key km "U" [:resize-frame 0 -100])
      (:define-key km "I" [:resize-frame 0 100])
      (:define-key km "O" [:resize-frame 100 0])
      (:define-key km "Enter" :pop-keymap)
      (:define-key km "Esc" :pop-keymap)
      km))
  (:define-key *acc-keymap* "Win + S" [:push-keymap resize-mode-keymap] "Resize mode")

  (:set-keymap key-man keymap)

  (:show-tooltip
     ui-man
     :tutorial
     (format-slide-text
      id total

```And of course, we have commands to resize windows too. Here we defined our window resizing keys in a Transient Keymap.

Jwno's keymaps work like a stack. When you want to temporarily switch to another keymap, use the special :push-keymap command. After you're done with the transient keymap, use the :pop-keymap command to bring back the normal keymap.

When a transient keymap is in effect, Jwno will automatically show the keys defined in the top-left corner.

Please try this out. First select a window you want to resize, then press Win + S. After that you can resize the window with only the Y/U/I/O keys. Press the Enter key when you're done.```

      false)
     nil nil
     0
     :center))


(defn more-about-resizing-windows
  {:slide 22}
  [id total keymap]

  (enable-command :frame-to-window-size)

  (:define-key *acc-keymap* "Win + Shift + S" :frame-to-window-size)
  (:set-keymap key-man keymap)

  (:show-tooltip
     ui-man
     :tutorial
     (format-slide-text
      id total

```When you're resizing a window, you're actually resizing the frame it belongs to, and all other windows in the same frame. You can see that from the command's name: it's actually called :resize-frame.

Some windows have minimum size requirements, but Jwno's frames don't have that limitation. So sometimes windows will "overflow" from the designated frame area. Unfortunately Jwno has no control over this. In that case, you can switch to the overflowing window, and use Win + Shift + S to make its parent frame large enough to contain it.```

      )
     nil nil
     0
     :center))


(defn closing-windows-and-frames
  {:slide 23}
  [id total keymap]

  # Start removing empty frames
  (:enable *auto-close-empty-frame*)

  (enable-command :close-window-or-frame)

  (:define-key *acc-keymap* "Win + Shift + C" :close-window-or-frame)
  (:set-keymap key-man keymap)

  (:show-tooltip
     ui-man
     :tutorial
     (format-slide-text
      id total

```To close a window, use the :close-window-or-frame command.

If there's a focused window, that command will close it. Otherwise, if the active frame is empty, it'll close the frame instead.

Note that the example config added a hook to automatically close empty frames after closing a window, so we'll include the same behavior here, but it's not Jwno's default behavior.

To try it out, first activate the window or frame you wan to close, then press Win + Shift + C.```

      )
     nil nil
     0
     :center))


(defn concluding
  {:slide 24}
  [id total keymap]

  # Enable all disabled commands
  (enable-command :split-frame)
  (enable-command :repl)

  (:set-keymap key-man keymap)

  (:show-tooltip
     ui-man
     :tutorial
     (format-slide-text
      id total

```Alright! We have walked through Jwno's most important commands. Now I have enabled them all, and they're yours to play with ;)

Or, you can check out example-config.janet and try to launch Jwno with it. There're extra goodies there. 

Remember to use Win + Shift + / (the slash key) to see the list of defined keys.```

      )
     nil nil
     0
     :center))


(defn cleanup-tutorial
  {:slide 25}
  [id total keymap]

  (:set-keymap key-man keymap)

  (:show-tooltip
     ui-man
     :tutorial
     (format-slide-text
      id total

```Please try out the keys/commands.

Win + Shift + / to show all keys.

Win + Shift + Q to exit Jwno.```

      false)
     nil nil
     0
     :bottom-left))


# ================== Slides ends here ==================


(def [first-cmd first-keymap slide-defs]
  (build-slide-list))

(:set-tooltip-max-width ui-man :tutorial 450)

(:add-hook hook-man :window-created
   (fn [win _uia-win _exe-path _desktop-info]
     (put (in win :tags) :margin 10)))

(:add-hook hook-man :monitor-updated
   (fn [frame]
     (put (in frame :tags) :padding 10)))

(enable-command first-cmd)
# Trigger the next slide when a command is called
(:add-hook hook-man :filter-command demo-command-filter)

# Start showing important commands
(:add-hook hook-man :filter-command show-command-filter)

(:call-command command-man first-cmd)
