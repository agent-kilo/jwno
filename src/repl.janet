(import spork/netrepl)

(import ./log)

(def- repl-env (make-env))


(defn make-repl-env [name stream context]
  (def context-def
    @{:value context
      :doc "The Jwno main loop context object.\n"})
  (def client-name-def
    @{:value name
      :doc "The name for the current REPL client.\n"})
  (def client-stream-def
    @{:value stream
      :doc "The socket stream for the current REPL client.\n"})

  (def new-env (make-env repl-env))
  (put new-env 'jwno-context context-def)
  (put new-env 'jwno-client-name client-name-def)
  (put new-env 'jwno-client-stream client-stream-def)

  (make-env new-env))


(defn start-server [context &opt addr port]
  (default addr "127.0.0.1")
  (default port 9527)
  (def server-stream
    (netrepl/server addr port
                    (fn [name stream]
                      (make-repl-env name stream context))
                    nil
                    "Welcome to Jwno REPL!\n"))
  (log/debug "REPL server running at %s:%d" addr port)
  server-stream)


(defn stop-server [server-stream]
  (:close server-stream))
