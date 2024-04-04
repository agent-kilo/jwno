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
(import ./log)


(def DEFAULT-CHAN-LIMIT 65536)


(defn main-loop [context]
  (forever
   (def event (ev/select ;(in context :event-sources)))

   (match event
     [:take chan msg]
     (match msg
       [:ui/initialized thread-id msg-hwnd]
       (do
         (put context :ui-thread thread-id)
         (put context :msg-hwnd msg-hwnd))

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


(defn main [& args]
  (def log-chan (log/init :debug))
  (log/debug "in main")

  (def hInstance (GetModuleHandle nil))

  (def uia-chan (ev/thread-chan DEFAULT-CHAN-LIMIT))
  (def uia-context
    (try
      (uia/uia-init uia-chan)
      ((err fib)
       (show-error-and-exit err 1))))

  (def wm
    (try
      (window-manager uia-context)
      ((err fib)
       (show-error-and-exit err 1))))

  (def ui-chan (ev/thread-chan DEFAULT-CHAN-LIMIT))

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

  # XXX: The argument of :map-to command can only be VK_*WIN or other
  # normal keys. If other modifiers (e.g. CTRL or ALT) are specified,
  # that modifier would be stuck in the :down state.
  (define-key keymap
    (key VK_RMENU)
    [:map-to VK_RWIN])

  (log/debug "keymap = %n" keymap)

  (ev/spawn-thread
   (ui/ui-thread hInstance (args 0) keymap ui-chan))

  (def context
    @{:hInstance hInstance
      :uia-context uia-context
      :event-sources [ui-chan uia-chan]

      :current-keymap keymap
      :inhibit-win-key (inhibit-win-key? keymap)
      :key-states @{}

      :wm wm

      :ui-thread nil
      :msg-hwnd nil})

  (def repl-server (repl/start-server context))

  (main-loop context)

  (repl/stop-server repl-server)
  (uia/uia-deinit uia-context)
  (log/deinit))
