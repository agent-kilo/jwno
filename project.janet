(declare-project
 :name "Jwno"
 :description "A window manager built with Janet."
 :dependencies [{:url "https://github.com/janet-lang/spork.git"
                 :tag "d644da0fd05612a2d5a3c97277bf7b9bb96dcf6b"}])


(defn dir-exists? [name]
  (def stat (os/stat name))
  (and (not (nil? stat)) (= (stat :mode) :directory)))

(defn ensure-dir [name]
  (when (not (dir-exists? name))
    (when (not (os/mkdir name))
      (error (string/format "failed to create directory %s" name)))))

(defn spawn-and-wait [& args]
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

(defn generate-resource-header [env out-file-name]
  (with [out-file (file/open out-file-name :wn)]
    (eachp [k v] env
      (when (and (table? v) (v :resource))
        (file/write out-file (string/format "#define %s %v\n" k (v :value)))))))

(defn generated [name]
  (string (find-build-dir) name))


(let [target (generated "resource.h")]
  (rule target ["src/resource.janet"]
    (ensure-dir (find-build-dir))
    (generate-resource-header (dofile "src/resource.janet") target)
    (printf "Generated %s" target)))


(let [target (generated "resource.res")]
  (rule target [(generated "resource.h")]
    (ensure-dir (find-build-dir))
    (def rc-out
      (spawn-and-wait "rc.exe" "/I" (find-build-dir) "/fo" target "res/jwno.rc"))
    (print rc-out)
    (printf "Generated %s" target)))


(let [target (generated "resource.obj")]
  (rule target [(generated "resource.res")]
    (ensure-dir (find-build-dir))
    (def cvtres-out
      (spawn-and-wait "cvtres.exe" "/machine:x64" (string "/out:" target) (generated "resource.res")))
    (print cvtres-out)
    (printf "Generated %s" target)))


(declare-executable
 :name "jwno"
 :entry "src/main.janet"
 :deps [;(->> (os/dir "src")
              (filter |(string/has-suffix? ".janet" $))
              (map |(string "src/" $)))
        (generated "resource.obj")]
 :ldflags [(generated "resource.obj")])
