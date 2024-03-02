(defn log-thread [chan output-fn]
  (forever
   (def msg (ev/take chan))
   (if-not msg (break))
   (output-fn msg)))


(var log-chan nil)


(defn init [level &opt output-fn]
  (default output-fn print)
  # XXX: If this channel is full, log functions called in win32 callbacks would
  # try to trap into the Janet event loop, and break the callback control flow.
  # Have to make it large enough so that it's not easily filled up.
  (set log-chan (ev/thread-chan 65536))
  (ev/spawn-thread
   (log-thread log-chan output-fn)))


(defn deinit []
  (ev/give log-chan nil))


(defn- format-log-msg [level fmt args]
  (def time-str (os/strftime "%Y-%m-%d %H:%M:%S" nil true))
  (string/format (string "%s [%s] " fmt)
                 (os/strftime "%Y-%m-%d %H:%M:%S" nil true)
                 level
                 ;args))


(defn debug [fmt & args]
  (ev/give log-chan
           (format-log-msg :debug fmt args)))


(defn info [fmt & args]
  (ev/give log-chan
           (format-log-msg :info fmt args)))


(defn warning [fmt & args]
  (ev/give log-chan
           (format-log-msg :warning fmt args)))


(defn error [fmt & args]
  (ev/give log-chan
           (format-log-msg :error fmt args)))
