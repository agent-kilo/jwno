(use jw32/_winuser)
(use jw32/_libloaderapi)
(use jw32/_combaseapi)
(use jw32/_uiautomation)

(use ./key)
(use ./cmd)
(use ./win)
(use ./hook)
(use ./ui)
(use ./uia)
(use ./config)
(use ./util)

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
       (:initialized (in context :ui-manager) thread-id msg-hwnd)

       :ui/exit
       (break)

       [:uia/window-opened hwnd]
       (:window-opened (in context :window-manager) hwnd)

       :uia/focus-changed
       (:focus-changed (in context :window-manager))

       [:key/command cmd]
       (:dispatch-command (in context :command-manager) cmd)

       _
       (log/warning "Unknown message: %n" msg))
     _
     (log/warning "Unhandled ev/select event: %n" event))))


(defn build-keymap []
  (def keymap (define-keymap))

  (define-key keymap
    (key (ascii "Q") @[:win])
    :quit)

  (define-key keymap
    (key (ascii "R") @[:win])
    :retile)

  (define-key keymap
    [(key VK_OEM_COMMA @[:win])
     (key VK_OEM_COMMA @[:win])]
    [:split :horizontal 2 [0.5] 1 0])

  (define-key keymap
    [(key VK_OEM_COMMA @[:win])
     (key (ascii "3"))]
    [:split :horizontal 3 [0.2 0.6 0.2] 0 1])

  (define-key keymap
    [(key VK_OEM_PERIOD @[:win])
     (key VK_OEM_PERIOD @[:win])]
    [:split :vertical 2 [0.5] 1 0])

  (define-key keymap
    (key VK_OEM_2 @[:win])
    :flatten-parent)

  (define-key keymap
    (key (ascii "N") @[:win])
    [:enum-frame :next])

  (define-key keymap
    (key (ascii "E") @[:win])
    [:enum-frame :prev])

  (define-key keymap
    (key (ascii "I") @[:win])
    :next-window-in-frame)

  (define-key keymap
    (key (ascii "M") @[:win])
    :prev-window-in-frame)

  (define-key keymap
    (key (ascii "N") @[:win :ctrl])
    [:adjacent-frame :down])
  (define-key keymap
    (key (ascii "E") @[:win :ctrl])
    [:adjacent-frame :up])
  (define-key keymap
    (key (ascii "M") @[:win :ctrl])
    [:adjacent-frame :left])
  (define-key keymap
    (key (ascii "I") @[:win :ctrl])
    [:adjacent-frame :right])

  (define-key keymap
    (key (ascii "N") @[:win :shift])
    [:move-current-window :down])
  (define-key keymap
    (key (ascii "E") @[:win :shift])
    [:move-current-window :up])
  (define-key keymap
    (key (ascii "M") @[:win :shift])
    [:move-current-window :left])
  (define-key keymap
    (key (ascii "I") @[:win :shift])
    [:move-current-window :right])

  (define-key keymap
    [(key (ascii "S") @[:win]) (key (ascii "N") @[:win])]
    [:resize-current-frame 0 100])
  (define-key keymap
    [(key (ascii "S") @[:win]) (key (ascii "E") @[:win])]
    [:resize-current-frame 0 -100])
  (define-key keymap
    [(key (ascii "S") @[:win]) (key (ascii "M") @[:win])]
    [:resize-current-frame -100 0])
  (define-key keymap
    [(key (ascii "S") @[:win]) (key (ascii "I") @[:win])]
    [:resize-current-frame 100 0])

  (define-key keymap
    (key (ascii "F") @[:win])
    [:focus-mode 0.7])

  (define-key keymap
    (key VK_OEM_PLUS @[:win])
    :balance-frames)

  (define-key keymap
    (key (ascii "S") @[:win :ctrl])
    :frame-to-current-window-size)

  (define-key keymap
    (key (ascii "C") @[:win :shift])
    :close-current-window)

  (define-key keymap
    [(key (ascii "T") @[:win])
     (key (ascii "N") @[:win])]
    [:change-current-window-alpha -25])
  (define-key keymap
    [(key (ascii "T") @[:win])
     (key (ascii "E") @[:win])]
    [:change-current-window-alpha 25])

  (define-key keymap
    (key VK_RMENU)
    [:map-to VK_RWIN])

  (log/debug "keymap = %n" keymap)
  keymap)


(defn main [& args]
  (def cli-args (parse-command-line))
  (when (nil? cli-args)
    (os/exit 1))

  (let [log-path (in cli-args "log-file")
        loggers (if (nil? log-path)
                  [log/print-logger]
                  [log/print-logger (fn [] (log/file-logger log-path))])]
    (try
      (log/init (in cli-args "log-level") ;loggers)
      ((err fib)
       (show-error-and-exit err 1))))

  (log/debug "in main")
  (log/debug "cli-args = %n" cli-args)

  (def uia-man
    (try
      (uia-manager)
      ((err fib)
       (show-error-and-exit err 1))))

  (def hook-man (hook-manager))

  (def window-man
    (try
      (window-manager uia-man hook-man)
      ((err fib)
       (show-error-and-exit err 1))))

  (def h-inst (GetModuleHandle nil))
  (def keymap (build-keymap))
  (def ui-man (ui-manager h-inst (in args 0) keymap))

  (def command-man (command-manager))

  (def context
    @{:h-instance h-inst
      :window-manager window-man
      :ui-manager ui-man
      :uia-manager uia-man
      :hook-manager hook-man
      :command-manager command-man
      :event-sources [(in uia-man :chan)
                      (in ui-man :chan)]})

  (add-default-commands command-man context)

  (def repl-server (repl/start-server context))
  (put context :repl repl-server)

  (def config-env (load-config-file [(in cli-args "config")] context))
  (log/debug "config-env = %n" config-env)

  (main-loop context)

  (repl/stop-server repl-server)
  (:destroy uia-man)
  (log/deinit))
