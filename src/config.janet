(use spork/argparse)

(use ./util)

(import ./const)
(import ./log)


(def- config-env (make-env))


(def repl-addr-peg
  (peg/compile
   ~{:ip-seg (choice
              (sequence "25" (range "05"))
              (sequence "2" (range "04") :d)
              (sequence "1" :d :d)
              (sequence :d :d)
              (sequence :d))
     :ip (sequence :ip-seg "." :ip-seg "." :ip-seg "." :ip-seg)
     :ip-capture (capture :ip)
     :port (sequence
            (any "0")
            (choice
             (sequence "6553" (range "05"))
             (sequence "655" (range "02") :d)
             (sequence "65" (range "04") :d :d)
             (sequence "6" (range "04") :d :d :d)
             (sequence (range "05") :d :d :d :d)
             (between 2 4 :d)
             (range "19")))
     :port-capture (replace (capture :port) ,parse)
     :main (sequence :ip-capture ":" :port-capture -1)}))


(defn parse-command-line []
  (def default-config-file-path (string (get-exe-dir) const/DEFAULT-CONFIG-FILE-NAME))

  (argparse
   "Jwno: A Tiling Window Manager for Windows"

   :default
   {:help (string "\nThe config file to run. Default: " default-config-file-path "\n")
    :kind :accumulate}

   "client"
   {:short "C"
    :help "\nStarts in REPL client mode. Connects to the REPL address specified by the repl option.\n"
    :kind :flag}

   "config"
   {:short "c"
    :help (string "\nThe config file to run. Default: " default-config-file-path "\n")
    :kind :accumulate}

   "log-file"
   {:help "\nSpecifies a log file to write to. No log file will be generated if this option is omitted.\n"
    :kind :option}

   "log-level"
   {:help "\nThe log level. Can be quiet, error, warning, info or debug. Default: quiet\n"
    :default "quiet"
    :kind :option
    :map keyword}

   "no-console"
   {:help "\nSupress console output, when log-level is not quiet.\n"
    :default false
    :kind :flag}

   "mod-path"
   {:short "m"
    :help "\nA custom path to load modules from.\n"
    :kind :accumulate}

   "repl"
   {:short "r"
    :help "\nSpecifies the address the REPL should be listening on. The REPL will not be started if this option is omitted.\n"
    :kind :option
    :map (fn [addr-str]
           (if-let [repl-addr (peg/match repl-addr-peg addr-str)]
             repl-addr
             (errorf "Malformed REPL address: %n" addr-str)))}
   ))


(defn look-for-config-file [paths-to-try]
  (var found nil)
  (each path paths-to-try
    (try
      (with [_config-file (os/open path :r)]
        (set found path))
      ((_err _fib) :nop))
    (when found
      (break)))
  found)


(defn load-config-file [config-path context]
  (def new-env (make-env config-env))
  (put new-env
       (global-ns 'context)
       @{:value context
         :doc "The Jwno main loop context object.\n"})
  (dofile config-path
          :source config-path
          :env (make-env new-env)))
