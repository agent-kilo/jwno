(use spork/argparse)

(use ./util)

(import ./const)
(import ./log)


(def- config-env (make-env))
(merge-module config-env (require "./log") "log/")


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

   "config"
   {:short "c"
    :help (string "The config file to run. Default: " default-config-file-path)
    :default default-config-file-path
    :kind :option}

   "repl"
   {:short "r"
    :help "Specifies the address the REPL should be listening on. The REPL will not be started if this option is omitted."
    :kind :option
    :map |(peg/match repl-addr-peg $)}

   "log-level"
   {:help "The log level. Can be quiet, error, warning, info or debug. Default: quiet"
    :default "quiet"
    :kind :option
    :map keyword}

   "log-file"
   {:help "Specifies a log file to write to. No log file will be generated if this option is omitted."
    :kind :option}

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


(defn load-config-file [paths-to-try context]
  (def config-path (look-for-config-file paths-to-try))
  (when (nil? config-path)
    (break))

  (def new-env (make-env config-env))
  (put new-env 'jwno-context @{:value context
                               :doc "The Jwno main loop context object.\n"})
  (dofile config-path
          :source config-path
          :env (make-env new-env)))
