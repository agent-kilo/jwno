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


(defn main-loop [cli-args context]
  (forever
   (def event (ev/select ;(in context :event-sources)))

   (match event
     [:take chan msg]
     (match msg
       [:ui/initialized thread-id msg-hwnd]
       (do
         (:initialized (in context :ui-manager) thread-id msg-hwnd)
         (def config-env (load-config-file [(in cli-args "config")] context))
         (log/debug "config-env = %n" config-env))

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
  (def ui-man (ui-manager h-inst (in args 0) @{}))

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

  (main-loop cli-args context)

  (repl/stop-server repl-server)
  (:destroy uia-man)
  (log/deinit))
