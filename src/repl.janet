(import spork/netrepl)

(import ./const)
(import ./log)


######### REPL Server object #########

(defn repl-server-stop [self]
  (:close (in self :stream)))


(defn repl-server-export [self & syms-meta]
  (def count (length syms-meta))
  (unless (even? count)
    (errorf "expected even number of symbol and meta values, got %n" count))
  (loop [i :range [0 count 2]]
    (put (in self :env)
         (in syms-meta i)
         (in syms-meta (+ 1 i)))))


(defn repl-server-unset [self & syms]
  (each s syms
    (put (in self :env) s nil)))


(defn repl-server-make-env [self client-name client-stream]
  (def context-def
    @{:value (in self :context)
      :doc "The Jwno main loop context object.\n"})
  (def repl-server-def
    @{:value self
      :doc "The current REPL server.\n"})
  (def client-name-def
    @{:value client-name
      :doc "The name for the current REPL client.\n"})
  (def client-stream-def
    @{:value client-stream
      :doc "The socket stream for the current REPL client.\n"})

  (def new-env (make-env (in self :env)))
  (put new-env 'jwno-context context-def)
  (put new-env 'jwno-repl-server repl-server-def)
  (put new-env 'jwno-client-name client-name-def)
  (put new-env 'jwno-client-stream client-stream-def)

  (make-env new-env))


(def repl-server-proto
  @{:stop repl-server-stop
    :make-env repl-server-make-env
    :export repl-server-export
    :unset repl-server-unset})


(defn repl-server [context &opt host port env-proto]
  (default host const/DEFAULT-REPL-HOST)
  (default port const/DEFAULT-REPL-PORT)

  (def server-obj
    @{:context context
      :env (make-env env-proto)
      :address [host port]})

  (def stream
    (netrepl/server host port
                    (fn [name stream]
                      (:make-env server-obj name stream))
                    nil
                    "Welcome to Jwno REPL!\n"))
  (put server-obj :stream stream)

  (log/debug "REPL server running at %s:%d" host port)
  (table/setproto server-obj repl-server-proto))


######### REPL Manager object #########

(defn repl-manager-start-server [self &opt host port env]
  (def default-env (in self :default-env))

  (def merged-env
    (cond
      (and default-env env)
      (merge default-env env)

      true
      (or env default-env)))

  (def server (repl-server (in self :context) host port merged-env))
  (array/push (in self :servers) server)
  server)


(defn repl-manager-stop-server [self server]
  (put self :servers (filter |(not= $ server) (in self :servers)))
  (:stop server))


(defn repl-manager-stop-all-servers [self]
  (def servers (in self :servers))
  (while (> (length servers) 0)
    (def s (array/pop servers))
    (:stop s)))


(defn repl-manager-get-default-server [self]
  (first (in self :servers)))


(def repl-manager-proto
  @{:start-server repl-manager-start-server
    :stop-server repl-manager-stop-server
    :stop-all-servers repl-manager-stop-all-servers
    :get-default-server repl-manager-get-default-server})


(defn repl-manager [context &opt default-env]
  (table/setproto
   @{:context context
     :default-env default-env
     :servers @[]}
   repl-manager-proto))
