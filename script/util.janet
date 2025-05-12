(defn dir-exists? [name]
  (def stat (os/stat name))
  (and (not (nil? stat)) (= (stat :mode) :directory)))

(defn ensure-dir [name]
  (when (not (dir-exists? name))
    (when (not (os/mkdir name))
      (error (string/format "failed to create directory %s" name)))))

(defn spawn-and-wait [& args]
  (when (dyn :verbose)
    (printf "Running command: %n" args))
  (def os-env (os/environ))
  (put os-env :out :pipe)
  (def proc (os/spawn args :ep os-env))
  (os/proc-wait proc)
  (def out (in proc :out))
  (def ret (in proc :return-code))
  (when (not (= ret 0))
    (print (:read out :all))
    (error (string/format "subprocess exited abnormally: %d" ret)))
  (:read out :all))
