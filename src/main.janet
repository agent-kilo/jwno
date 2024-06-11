(import spork/netrepl)

(use jw32/_winuser)
(use jw32/_libloaderapi)
(use jw32/_combaseapi)
(use jw32/_util)

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
       (let [config-file-name (in cli-args "config")
             ui-man (in context :ui-manager)]
         (:initialized ui-man thread-id msg-hwnd)
         (def config-env
           (try
             (load-config-file [config-file-name] context)
             ((err fib)
              (:show-error-and-exit
                 ui-man
                 (string/format "Failed to load config file: %s\n\n%s\n%s"
                                config-file-name
                                err
                                (get-stack-trace fib)))
              nil)))
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
  (def out-buf @"")
  (def cli-args
    (try
      (with-dyns [:out out-buf]
        (parse-command-line))
      ((err fib)
       (show-error-and-exit err 1 (get-stack-trace fib)))))

  (when (nil? cli-args)
    (show-error-and-exit out-buf 1))

  (when (in cli-args "client")
    (def repl-addr (in cli-args "repl"))
    (when (nil? repl-addr)
      (show-error-and-exit "Jwno is started in client mode, but `--repl` is not specified." 1))
    (alloc-console-and-reopen-streams)
    (try
      (netrepl/client ;repl-addr)
      ((err fib)
       (show-error-and-exit err 1 (get-stack-trace fib))))
    (os/exit 0))

  (let [log-path (in cli-args "log-file")
        loggers (if (nil? log-path)
                  [log/print-logger]
                  [log/print-logger (fn [] (log/file-logger log-path))])]
    (try
      (log/init (in cli-args "log-level") ;loggers)
      ((err fib)
       (show-error-and-exit err 1 (get-stack-trace fib)))))

  (log/debug "in main")
  (log/debug "cli-args = %n" cli-args)

  (try
    (CoInitializeEx nil COINIT_MULTITHREADED)
    ((err fib)
     (show-error-and-exit err 1 (get-stack-trace fib))))

  (def context @{}) # Forward declaration

  (def hook-man (hook-manager))

  (def command-man (command-manager))

  (def ui-man (ui-manager (GetModuleHandle nil) (in args 0) nil))

  (def uia-man
    (try
      (uia-manager)
      ((err fib)
       (show-error-and-exit err 1 (get-stack-trace fib)))))

  (def key-man (key-manager ui-man))

  (def window-man
    (try
      (window-manager uia-man ui-man hook-man)
      ((err fib)
       (show-error-and-exit err 1 (get-stack-trace fib)))))

  (def repl-server
    (if-let [repl-addr (in cli-args "repl")]
      # context will only get referenced after the main-loop is running
      # and when the first REPL client connects.
      (repl/start-server context ;repl-addr)
      nil))

  (put context :hook-manager hook-man)
  (put context :command-manager command-man)
  (put context :ui-manager ui-man)
  (put context :uia-manager uia-man)
  (put context :event-sources [(in uia-man :chan) (in ui-man :chan)])
  (put context :key-manager key-man)
  (put context :window-manager window-man)
  (put context :repl repl-server)

  (add-default-commands command-man context)

  (main-loop cli-args context)

  (when repl-server
    (repl/stop-server repl-server))
  (:destroy window-man)
  (:destroy uia-man)
  (CoUninitialize)
  (log/deinit))
