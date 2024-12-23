(import spork/netrepl)
(import spork/path)

(use jw32/_winuser)
(use jw32/_libloaderapi)
(use jw32/_combaseapi)
(use jw32/_util)

(use ./repl)
(use ./key)
(use ./cmd)
(use ./win)
(use ./hook)
(use ./ui)
(use ./uia)
(use ./mod)
(use ./config)
(use ./util)

(import ./resource)
(import ./const)
(import ./log)


(defn get-config-file-paths [cli-args]
  (def default-config-file-path
    (string (get-exe-dir) const/DEFAULT-CONFIG-FILE-NAME))
  (def paths
    (or (in cli-args "config")
        (in cli-args :default)))
  (if (or (nil? paths)
          (empty? paths))
    [default-config-file-path]
    paths))


(defn get-mod-paths [cli-args config-file-path]
  (def ret
    (if-let [paths (in cli-args "mod-path")]
      paths
      @[]))
  (array/push ret (path/dirname config-file-path))
  ret)


(defn late-init [cli-args context]
  (def config-file-paths (get-config-file-paths cli-args))
  (def config-found (look-for-config-file config-file-paths))

  (if config-found
    (do
      (:register-loader (in context :module-manager)
                        ;(get-mod-paths cli-args config-found))

      (def config-env
        (try
          (load-config-file config-found context)
          ((err fib)
           (:show-error-and-exit
              (in context :ui-manager)
              (string/format "Failed to load config file: %s\n\n%s\n%s"
                             config-found
                             err
                             (get-stack-trace fib)))
           nil)))
      (when config-env
        # Only proceed after the config file is successfully loaded
        (:init-event-handlers (in context :uia-manager))))

    # config file not found
    (:show-error-and-exit
       (in context :ui-manager)
       (string/format "No config file found in these locations:\n%s\n"
                      (string/join config-file-paths "\n")))))


(defn handle-display-changed [context]
  (def wm (in context :window-manager))
  (def root (in wm :root))
  (def layouts (in root :children))

  (def mon-info
    (try
      (:enumerate-monitors wm)
      ((err fib)
       (log/debug ":enumerate-monitors failed: %s\n%s"
                  err
                  (get-stack-trace fib))
       # The enumeration may fail if all the monitors are turned
       # off. We just ignore this case and wait for the next
       # :ui/display-changed event triggered by turning on the
       # monitors again.
       nil)))

  (when mon-info
    (def [monitors _main-idx] mon-info)
    (with-activation-hooks wm
      (each lo layouts
        (:update-work-areas lo monitors wm))
      (:retile wm)
      (when-let [cur-frame (:get-current-frame root)]
        (:set-focus wm cur-frame)))))


(defn handle-launch-repl [context]
  (def command-man (in context :command-manager))
  (def ui-man (in context :ui-manager))
  (def host const/DEFAULT-REPL-HOST)
  (def port const/DEFAULT-REPL-PORT)
  (try
    (:call-command command-man :repl true host port)
    ((err fib)
     (:show-tooltip ui-man :repl
        (string/format "Failed to launch REPL at %s:%d\n%s\n%s"
                       host
                       port
                       err
                       (get-stack-trace fib))))))


(defn main-loop [args]
  (def [cli-args context] args)

  (forever
   (def event (ev/select ;(in context :event-sources)))

   (match event
     [:take chan msg]
     (match msg
       [:ui/initialized thread-id msg-hwnd]
       (do
         (:initialized (in context :ui-manager) thread-id msg-hwnd)
         (late-init cli-args context))

       :ui/display-changed
       (handle-display-changed context)

       :ui/launch-repl
       (handle-launch-repl context)

       :ui/exit
       (break)

       [:uia/window-opened hwnd]
       (:window-opened (in context :window-manager) hwnd)

       :uia/focus-changed
       (:focus-changed (in context :window-manager))

       [:uia/desktop-name-changed vd-name]
       (:desktop-name-changed (in context :window-manager) vd-name)

       [:key/command cmd-info]
       (let [{:cmd cmd} cmd-info]
         (:dispatch-command (in context :command-manager) cmd))

       [:key/switch-keymap cur-keymap]
       (:call-hook (in context :hook-manager) :keymap-switched cur-keymap)

       [:key/reset-keymap cur-keymap]
       (:call-hook (in context :hook-manager) :keymap-reset cur-keymap)

       [:key/push-keymap cur-keymap]
       (:call-hook (in context :hook-manager) :keymap-pushed cur-keymap)

       [:key/pop-keymap cur-keymap]
       (:call-hook (in context :hook-manager) :keymap-popped cur-keymap)

       [:key/raw raw-key]
       (:call-hook (in context :hook-manager) :key-pressed raw-key)

       [:debug/thunk thunk]
       (thunk context)

       _
       (log/warning "Unknown message: %n" msg))
     _
     (log/warning "Unhandled ev/select event: %n" event))))


(defn format-help-msg [msg]
  (->> msg
       # Add a new line after "Optional:", so that it's clearly separated
       # from the actual help text
       (peg/replace-all
        ~{:main " Optional:"}
        "Optional:\n")
       # Fix the help text for "--help" flag, which we can't control at all
       (peg/replace-all
        ~{:main (sequence (capture "-h, --help") (some (set " \t\f\v")) (capture (some (if-not "\n" 1))))}
        (fn [_str flags text]
          (string flags "\n" text "\n")))
       # Remove excessive spaces before flag names
       (peg/replace-all
        ~{:main (sequence "\n" (some (set " \t\f\v")) "-")}
        "\n-")))


(defn parse-args []
  (def out-buf @"")
  (def cli-args
    (try
      (with-dyns [:out out-buf]
        (parse-command-line))
      ((err fib)
       (show-error-and-exit err 1 (get-stack-trace fib)))))

  (when (nil? cli-args)
    # out-buf contains the help message built by spork/argparse.
    # Re-format it so that it looks nicer on MessageBox.
    # It's a workaround until we get a proper message box with
    # monospaced fonts.
    (def formatted (format-help-msg out-buf))
    (show-error-and-exit formatted 1))

  cli-args)


(defn run-repl-client [cli-args]
  (def repl-addr (in cli-args "repl"))
  (when (nil? repl-addr)
    (show-error-and-exit "Jwno is started in client mode, but REPL address is not specified." 1))
  (try
    (do
      (alloc-console-and-reopen-streams)
      (netrepl/client ;repl-addr))
    ((err fib)
     (show-error-and-exit err 1 (get-stack-trace fib)))))


(defn init-log [cli-args]
  (def loggers @[])

  (when (not (in cli-args "no-console"))
    (array/push loggers log/print-logger))

  (def log-path (in cli-args "log-file"))
  (when log-path
    (array/push loggers (fn [] (log/file-logger log-path))))

  (try
    (log/init (in cli-args "log-level") ;loggers)
    ((err fib)
     (show-error-and-exit err 1 (get-stack-trace fib)))))


(defn init-com []
  (try
    (CoInitializeEx nil COINIT_MULTITHREADED)
    ((err fib)
     (show-error-and-exit err 1 (get-stack-trace fib)))))


(defn main [& args]
  (def cli-args (parse-args))

  (when (in cli-args "version")
    (show-version-info)
    (os/exit 0))

  (when (in cli-args "client")
    (run-repl-client cli-args)
    (os/exit 0))

  (init-log cli-args)

  (log/debug "cli-args = %n" cli-args)

  (init-com)

  (def context @{}) # Forward declaration

  (def hook-man (hook-manager))

  (def command-man (command-manager hook-man))

  (def ui-man (ui-manager (GetModuleHandle nil) (in args 0) nil))

  (def uia-man
    (try
      (uia-manager)
      ((err fib)
       (show-error-and-exit err 1 (get-stack-trace fib)))))

  (def key-man (key-manager ui-man hook-man))

  (def window-man
    (try
      (window-manager uia-man ui-man hook-man)
      ((err fib)
       (show-error-and-exit (string err) 1 (get-stack-trace fib)))))

  (def module-man (module-manager))

  (def repl-man (repl-manager context))
  (if-let [repl-addr (in cli-args "repl")]
    (:start-server repl-man ;repl-addr))

  (put context :hook-manager hook-man)
  (put context :command-manager command-man)
  (put context :ui-manager ui-man)
  (put context :uia-manager uia-man)
  (put context :event-sources [(in uia-man :chan) (in ui-man :chan)])
  (put context :key-manager key-man)
  (put context :window-manager window-man)
  (put context :module-manager module-man)
  (put context :repl-manager repl-man)

  (add-default-commands command-man context)

  (def main-loop-sup (ev/chan))
  (def main-loop-fib (ev/go main-loop [cli-args context] main-loop-sup))

  (forever
   (match (ev/take main-loop-sup)
     [stat (@ main-loop-fib) & _rest]
     # TODO: Restart it when errors were raised?
     (do
       (log/debug "Main loop stopped")
       (unless (= :ok stat)
         (log/error "%n signal from main loop: %n\n%s"
                    stat
                    (fiber/last-value main-loop-fib)
                    (get-stack-trace main-loop-fib))
         # In normal shutdown sequence, the UI thread will end itself
         # and then tells us to quit.
         # But when the main loop errored out, we need to wait for the
         # UI thread instead.
         (:destroy ui-man)
         (var ui-ev (ev/take (in ui-man :chan)))
         (while (not= :ui/exit ui-ev)
           (log/warning "Not handled event from UI thread: %n" ui-ev)
           (set ui-ev (ev/take (in ui-man :chan)))))
       (break))

     [stat fib & _rest]
     # Some signals from child fibers bubbled up, we can't do anything
     # meaningful here except writing down the log.
     (unless (= :ok stat)
       (log/error "%n signal from fiber spawned by main loop: %n\n%s"
                  stat
                  (fiber/last-value fib)
                  (get-stack-trace fib)))

     unknown-ev
     (log/warning "Unknown event from main loop supervisor: %n" unknown-ev)))

  (:stop-all-servers repl-man)
  (:unregister-loader module-man)
  (:destroy window-man)
  (:destroy uia-man)
  (CoUninitialize)
  (log/deinit))
