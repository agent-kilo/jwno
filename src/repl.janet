(import spork/netrepl)

(import ./const)
(import ./log)


(def- repl-env (make-env))

(defn export* [sym meta]
  (put repl-env sym meta))

(defmacro export [sym]
  ~(,export* (quote ,sym) (in (curenv) (quote ,sym))))

(defn unset* [sym]
  (put repl-env sym nil))

(defmacro unset [sym]
  ~(,unset* (quote ,sym)))


(defn- make-repl-env [name stream context &opt env-proto]
  (default env-proto repl-env)

  (def context-def
    @{:value context
      :doc "The Jwno main loop context object.\n"})
  (def client-name-def
    @{:value name
      :doc "The name for the current REPL client.\n"})
  (def client-stream-def
    @{:value stream
      :doc "The socket stream for the current REPL client.\n"})

  (def new-env (make-env env-proto))
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




######### REPL Server object #########

(defn repl-server-stop [self]
  (:close (in self :stream)))


(defn repl-server-export [self sym meta]
  (put (in self :env) sym meta))


(defn repl-server-unset [self sym]
  (put (in self :env) sym nil))


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
  (table/setproto repl-server-proto server-obj))


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


(def repl-manager-proto
  @{:start-server repl-manager-start-server
    :stop-server repl-manager-stop-server
    :stop-all-servers repl-manager-stop-all-servers})


(defn repl-manager [context &opt default-env]
  (table/setproto
   @{:context context
     :default-env default-env
     :servers @[]}
   repl-manager-proto))
