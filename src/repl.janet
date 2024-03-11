(import spork/netrepl)

(import ./log)

(def- repl-env (make-env))
(merge-module repl-env (require "jw32/winuser"))
(merge-module repl-env (require "./log") "log/")


(defn make-repl-env [name stream server]
  (def server-def
    @{:value server
      :doc "The Jwno REPL server object.\n"})
  (def client-name-def
    @{:value name
      :doc "The name for the current REPL client.\n"})
  (def client-stream-def
    @{:value stream
      :doc "The socket stream for the current REPL client.\n"})

  (def new-env (make-env repl-env))
  (put new-env 'jwno-server server-def)
  (put new-env 'jwno-client-name client-name-def)
  (put new-env 'jwno-client-stream client-stream-def)

  (make-env new-env))


(defn start-server [context &opt addr port]
  (default addr "127.0.0.1")
  (default port 9527)
  (def server
    @{:context context})
  (def server-stream
    (netrepl/server addr port
                    (fn [name stream]
                      (make-repl-env name stream server))
                    nil
                    "Welcome to Jwno REPL!\n"))
  (log/debug "REPL server running at %s:%d" addr port)
  (put server :stream server-stream))


(defn stop-server [server]
  (def server-stream (in server :stream))
  (:close server-stream))
