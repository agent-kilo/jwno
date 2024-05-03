(use spork/argparse)

(use ./util)

(import ./const)
(import ./log)


(def- config-env (make-env))
# TODO: More concise API?
(merge-module config-env (require "jw32/_winuser"))
(merge-module config-env (require "./key"))
(merge-module config-env (require "./log") "log/")


(defn parse-command-line []
  (def default-config-file-path (string (get-exe-dir) const/DEFAULT-CONFIG-FILE-NAME))

  (argparse
   "Jwno: A Tiling Window Manager for Windows"

   "config"
   {:short "c"
    :help (string "The config file to run. Default: " default-config-file-path)
    :default default-config-file-path
    :kind :option}

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
      ((_err _fib) :nop)))
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
