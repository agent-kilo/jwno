(use jw32/_consoleapi)


(defn log-thread [chan output-fn]
  (forever
   (def msg (ev/take chan))
   (if-not msg (break))
   (output-fn msg)))


(var log-chan nil)
(var log-level :quiet)
(def log-levels
  {:quiet 0
   :error 1
   :warning 2
   :info 3
   :debug 4})


(defn init [level &opt output-fn]
  (default output-fn print)
  (set log-level level)
  (when (<= (in log-levels level) (in log-levels :quiet))
    # No log at all, detach from the console
    (FreeConsole)
    (break))
  # XXX: If this channel is full, log functions called in win32 callbacks would
  # try to trap into the Janet event loop, and break the callback control flow.
  # Have to make it large enough so that it's not easily filled up.
  (set log-chan (ev/thread-chan 65536))
  (ev/spawn-thread
   (log-thread log-chan output-fn)))


(defn deinit []
  (when log-chan
    (ev/give log-chan nil)))


(defn- format-log-msg [level fmt args]
  (def time-str (os/strftime "%Y-%m-%d %H:%M:%S" nil true))
  (string/format (string "%s [%s] " fmt)
                 (os/strftime "%Y-%m-%d %H:%M:%S" nil true)
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
