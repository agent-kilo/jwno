(use jw32/_winuser)
(use jw32/_libloaderapi)
(use jw32/_combaseapi)
(use jw32/_uiautomation)

(use ./key)
(use ./cmd)
(use ./win)
(use ./util)

(import ./ui)
(import ./uia)
(import ./repl)
(import ./const)
(import ./log)


(defn main-loop [context]
  (forever
   (def event (ev/select ;(in context :event-sources)))

   (match event
     [:take chan msg]
     (match msg
       [:ui/initialized thread-id msg-hwnd]
       (:initialized (in context :ui) thread-id msg-hwnd)

       :ui/exit
       (break)

       [:uia/window-opened hwnd]
       (:window-opened (in context :wm) hwnd)

       :uia/focus-changed
       (:focus-changed (in context :wm))

       [:key/key-event key key-state cmd]
       (process-key-event key key-state cmd context)

       [:key/raw-key-event key-code key-state]
       (let [keymap (in context :current-keymap)
             key-states (in context :key-states)
             inhibit-win-key (in context :inhibit-win-key)]
         (def [key-struct cmd new-keymap]
           (process-raw-key-event key-code key-state keymap key-states inhibit-win-key))
         (log/debug "new-keymap after process-raw-key-event: %n" new-keymap)
         (if-not (nil? cmd)
           (dispatch-command cmd key-struct key-state context))
         (put context :current-keymap new-keymap))

       _
       (log/warning "Unknown message: %n" msg))
     _
     (log/warning "Unhandled ev/select event: %n" event))))


(defn build-testing-keymap []
  (def keymap (define-keymap))

  (define-key keymap
    (key (ascii "A"))
    [:map-to (ascii "B")])
  (define-key keymap
    (key (ascii "B"))
    [:map-to (ascii "A")]))


(defn build-keymap []
  (def keymap (define-keymap))

  (define-key keymap
    (key (ascii "Q") @[:lwin])
    :quit)

  (define-key keymap
    (key (ascii "R") @[:lwin])
    :retile)

  (define-key keymap
    [(key VK_OEM_COMMA @[:lwin])
     (key VK_OEM_COMMA @[:lwin])]
    [:split :horizontal 2 [0.5] 1 0])

  (define-key keymap
    [(key VK_OEM_COMMA @[:lwin])
     (key (ascii "3"))]
    [:split :horizontal 3 [0.2 0.6 0.2] 0 1])

  (define-key keymap
    [(key VK_OEM_PERIOD @[:lwin])
     (key VK_OEM_PERIOD @[:lwin])]
    [:split :vertical 2 [0.5] 1 0])

  (define-key keymap
    (key VK_OEM_2 @[:lwin])
    :flatten-parent)

  (define-key keymap
    (key (ascii "N") @[:lwin])
    [:enum-frame :next])

  (define-key keymap
    (key (ascii "E") @[:lwin])
    [:enum-frame :prev])

  (define-key keymap
    (key (ascii "I") @[:lwin])
    :next-window-in-frame)

  (define-key keymap
    (key (ascii "M") @[:lwin])
    :prev-window-in-frame)

  (define-key keymap
    (key (ascii "N") @[:lwin :lctrl])
    [:adjacent-frame :down])
  (define-key keymap
    (key (ascii "E") @[:lwin :lctrl])
    [:adjacent-frame :up])
  (define-key keymap
    (key (ascii "M") @[:lwin :lctrl])
    [:adjacent-frame :left])
  (define-key keymap
    (key (ascii "I") @[:lwin :lctrl])
    [:adjacent-frame :right])

  (define-key keymap
    (key (ascii "N") @[:lwin :lshift])
    [:move-current-window :down])
  (define-key keymap
    (key (ascii "E") @[:lwin :lshift])
    [:move-current-window :up])
  (define-key keymap
    (key (ascii "M") @[:lwin :lshift])
    [:move-current-window :left])
  (define-key keymap
    (key (ascii "I") @[:lwin :lshift])
    [:move-current-window :right])

  (define-key keymap
    [(key (ascii "S") @[:lwin]) (key (ascii "N") @[:lwin])]
    [:resize-current-frame 0 100])
  (define-key keymap
    [(key (ascii "S") @[:lwin]) (key (ascii "E") @[:lwin])]
    [:resize-current-frame 0 -100])
  (define-key keymap
    [(key (ascii "S") @[:lwin]) (key (ascii "M") @[:lwin])]
    [:resize-current-frame -100 0])
  (define-key keymap
    [(key (ascii "S") @[:lwin]) (key (ascii "I") @[:lwin])]
    [:resize-current-frame 100 0])

  (define-key keymap
    (key (ascii "F") @[:lwin])
    [:focus-mode 0.7])

  (define-key keymap
    (key VK_OEM_PLUS @[:lwin])
    :balance-frames)

  (define-key keymap
    (key (ascii "S") @[:lwin :lctrl])
    :frame-to-current-window-size)

  # XXX: The argument of :map-to command can only be VK_*WIN or other
  # normal keys. If other modifiers (e.g. CTRL or ALT) are specified,
  # that modifier would be stuck in the :down state.
  (define-key keymap
    (key VK_RMENU)
    [:map-to VK_RWIN])

  (log/debug "keymap = %n" keymap)
  keymap)


(defn main [& args]
  (def log-chan (log/init :debug))
  (log/debug "in main")

  (def uia
    (try
      #(uia/init)
      # XXX: placeholder when testing keymaps
      @{:chan (ev/thread-chan)
        :destroy (fn [&])}
      ((err fib)
       (show-error-and-exit err 1))))

  (def wm
    (try
      (window-manager uia)
      ((err fib)
       (show-error-and-exit err 1))))

  (def h-inst (GetModuleHandle nil))
  (def keymap (build-testing-keymap))
  (def ui (ui/init h-inst (in args 0) keymap))

  (def context
    @{:h-instance h-inst
      :wm wm
      :ui ui
      :uia uia
      :event-sources [(in uia :chan)
                      (in ui :chan)]

      :current-keymap keymap
      :inhibit-win-key (inhibit-win-key? keymap)
      :key-states @{}})

  (def repl-server (repl/start-server context))
  (put context :repl repl-server)

  (main-loop context)

  (repl/stop-server repl-server)
  (:destroy uia)
  (log/deinit))
