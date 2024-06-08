(use jw32/_consoleapi)
(use jw32/_util)


(defn log-thread [chan logger-factories]
  (def loggers
    (map |($)
         logger-factories))
  (forever
   (def msg (ev/take chan))
   (if-not msg (break))
   (each lgr loggers
     (:output lgr msg)))
  (each lgr loggers
    (:close lgr)))


(var log-chan nil)
(var log-level :quiet)
(def log-levels
  {:quiet 0
   :error 1
   :warning 2
   :info 3
   :debug 4})


(defn print-logger []
  {:output (fn [_self msg] (prin msg))
   :close (fn [_self] :nop)})


(defn file-logger [log-path]
  (def log-file (file/open log-path :a+n))
  {:output (fn [_self msg]
             (file/write log-file msg)
             (file/flush log-file))
   :close (fn [_self] (file/close log-file))})


(defn init [level & factories]
  (when (nil? (in log-levels level))
    (error (string/format "Unknown log level: %s" level)))

  (set log-level level)
  (def logger-factories
    (if (empty? factories)
      [print-logger]
      factories))

  (when (<= (in log-levels level) (in log-levels :quiet))
    # No log at all
    (break))

  (when (find |(= $ print-logger) logger-factories)
    (alloc-console-and-reopen-streams))

  # XXX: If this channel is full, log functions called in win32 callbacks would
  # try to trap into the Janet event loop, and break the callback control flow.
  # Have to make it large enough so that it's not easily filled up.
  (set log-chan (ev/thread-chan 65536))
  (ev/spawn-thread
   (log-thread log-chan logger-factories)))


(defn deinit []
  (when log-chan
    (ev/give log-chan nil)))


(defn- format-log-msg [level fmt args]
  (def time-str (os/strftime "%Y-%m-%d %H:%M:%S" nil true))
  (string/format (string "%s [%s] " fmt "\n")
                 time-str
                 level
                 ;args))


(defn check-log-level [level]
  (<= (in log-levels level) (in log-levels log-level)))


(defn send-log [level fmt & args]
  (when (check-log-level level)
    (ev/give log-chan
             (format-log-msg level fmt args))))


(defmacro debug [fmt & args]
  ~(,send-log :debug ,fmt ,;args))


(defmacro info [fmt & args]
  ~(,send-log :info ,fmt ,;args))


(defmacro warning [fmt & args]
  ~(,send-log :warning ,fmt ,;args))


(defmacro error [fmt & args]
  ~(,send-log :error ,fmt ,;args))
