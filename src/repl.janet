(import spork/netrepl)
(import spork/msg)

(use jw32/_winuser)
(use jw32/_util)

(use ./util)

(import ./resource)
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
  (def new-env (make-env (in self :env)))
  (def context (in self :context))

  (put new-env
       (global-ns 'context)
       @{:value context
         :doc "Jwno's main loop context object.\n"})
  (put new-env
       (global-ns 'user-config)
       @{:value (in context :user-config)
         :doc "Jwno's user config file environment.\n"})
  (put new-env
       (global-ns 'repl-server)
       @{:value self
         :doc "The current REPL server.\n"})
  (put new-env
       (global-ns 'client-name)
       @{:value client-name
         :doc "The name for the current REPL client.\n"})
  (put new-env
       (global-ns 'client-stream)
       @{:value client-stream
         :doc "The socket stream for the current REPL client.\n"})
  (put new-env
       (global-ns 'version)
       @{:value [resource/VERSION_MAJOR
                 resource/VERSION_MINOR
                 resource/VERSION_PATCH
                 resource/VERSION_VCS]
         :doc "Current Jwno version.\n"})

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
                    (fn [name]
                      (if (string/has-prefix? const/DEFAULT-REPL-EVAL-CLIENT-NAME name)
                        nil
                        # else
                        "Welcome to Jwno REPL!\n"))))
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


######### Other helper functions #########

#
# Runtime error and stacktrace returned by the REPL. Example:
#
# error: exploded
#   in thunk [repl] (tail call) on line 1, column 1
#
(def eval-error-peg
  (peg/compile ~{:err-prefix "error:"
                 :err-msg (any (if-not "\n" 1))
                 :st-function (any (if-not :s 1))
                 :st-filename (sequence "[" (thru "]"))
                 :st-tailcall "(tail call)"
                 :st-location (sequence "line" :s+ :d+ "," :s+ "column" :s+ :d+)
                 :st-line (sequence :s+ "in" :s+ :st-function :s+ :st-filename (opt (sequence :s+ :st-tailcall)) :s+ "on" :s+ :st-location "\n")
                 :main (sequence :err-prefix :s+ :err-msg "\n" (any :st-line))}))

#
# Parse/compile error returned by the REPL. Examples:
#
# repl:1:1: compile error: unknown symbol abc
# repl:1:4: parse error: unexpected closing delimiter )
#
(def repl-error-peg
  (peg/compile ~{:repl-prefix "repl:"
                 :location (sequence :d+ ":" :d+)
                 :err-type (sequence (some :S) :s+ "error:")
                 :err-msg (some (if-not "\n" 1))
                 :main (sequence :repl-prefix :location ":" :s+ :err-type :s+ :err-msg "\n")}))

#
# Prompt string sent by the REPL. Examples (Note the trailing whitespaces):
#
# jwno-repl-eval:1: 
# jwno-repl-eval:2:( 
#
(def repl-prompt-peg
  (peg/compile ~{:sep ":"
                 :client-name (capture (sequence ,const/DEFAULT-REPL-EVAL-CLIENT-NAME (any (if-not :sep 1))))
                 :line-no (capture :d+)
                 :open-delimiters  (capture (any :S))
                 :main (sequence :client-name :sep :line-no :sep :open-delimiters :s+ -1)}))


(defn- check-prompt-string [prmpt]
  (log/debug "repl prompt: %n" prmpt)
  (when (nil? prmpt)
    (error "no prompt received"))
  (def matched (peg/match repl-prompt-peg prmpt))
  (unless matched
    (errorf "unrecognized prompt: %n" prmpt))
  matched)


(defn run-repl-eval [eval-list cli-args]
  (def [repl-host repl-port]
    (if-let [addr (in cli-args "repl")]
      addr
      # else
      @[const/DEFAULT-REPL-HOST const/DEFAULT-REPL-PORT]))

  (try
    (with [stream (net/connect repl-host repl-port)]
      (def unpack
        |(if (= 0xFE (first $))
           # Drop the 0xFE byte, see comments at the top of spork/netrepl
           (string/slice $ 1)
           # else
           (string $)))

      (def [send recv] (msg/make-proto stream nil unpack))

      (def settings
        {:auto-flush false
         :name const/DEFAULT-REPL-EVAL-CLIENT-NAME})
      (log/debug "repl settings: %n" settings)
      (send (string/format "\xFF%j" settings))

      (var prmpt (check-prompt-string (recv)))

      (each line eval-list
        (log/debug "repl input: %n" line)
        (send (string line "\n"))
        (def res (recv))
        (log/debug "repl result: %n" res)

        (when (and (not (nil? res))
                   (or (peg/find repl-error-peg res)
                       (peg/find eval-error-peg res)))
          (if (log/check-log-level :error)
            (log/error "REPL returned:\n%s" res)
            (MessageBox nil
                        (string/format "REPL returned:\n%s" res)
                        "Error"
                        (bor MB_ICONEXCLAMATION)))
          (os/exit 1))

        (set prmpt (check-prompt-string (recv))))

      (def open-delimiters (in prmpt 2))
      (unless (empty? open-delimiters)
        (error "incomplete expression")))

    ((err fib)
     (if (log/check-log-level :error)
       (log/error "REPL evaluation failed: %n\n%s"
                  err (get-stack-trace fib))
       (show-error-and-exit err 1 (get-stack-trace fib))))))


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
