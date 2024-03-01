(defn log-thread [chan output-fn]
  (forever
   (def msg (ev/take chan))
   (if-not msg (break))
   (def time-str (os/strftime "%Y-%m-%d %H:%M:%S" nil true))
   (match msg
     [:debug fmt args]
     (output-fn (string/format (string time-str " [DEBUG] " fmt) ;args))

     [:info fmt args]
     (output-fn (string/format (string time-str " [INFO] " fmt) ;args))

     [:warning fmt args]
     (output-fn (string/format (string time-str " [WARNING] " fmt) ;args))

     [:error fmt args]
     (output-fn (string/format (string time-str " [ERROR] " fmt) ;args))

     _
     (output-fn (string/format (string time-str " [WARNING] Unknown log message: %p") msg)))))


# XXX: If this channel is full, log functions called in win32 callbacks would
# try to trap into the Janet event loop, and break the callback control flow.
# Have to make it large enough so that it's not easily filled up.
(var log-chan (ev/thread-chan 65536))


(defn init [level &opt output-fn]
  (default output-fn printf)
  (ev/spawn-thread
   (log-thread log-chan output-fn)))


(defn deinit []
  (ev/give log-chan nil))


(defn debug [fmt & args]
  (ev/give log-chan [:debug fmt args]))


(defn info [fmt & args]
  (ev/give log-chan [:info fmt args]))


(defn warning [fmt & args]
  (ev/give log-chan [:warning fmt args]))


(defn error [fmt & args]
  (ev/give log-chan [:error fmt args]))
