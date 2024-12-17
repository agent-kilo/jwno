(declare-project
 :name "Jwno"
 :description "A window manager built with Janet."
 :dependencies [{:url "https://github.com/janet-lang/spork.git"
                 :tag "18938c57212c8d4dc0a37b6ea10b8b859aad7518"}])


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

(def fossil-info-peg
  (peg/compile
   ~{:main (some :info-line)
     :info-line (replace
                 (sequence :info-name ":" :s* :info-value "\n")
                 ,(fn [& info-pair] info-pair))
     :info-name (capture (to ":"))
     :info-value (capture (to "\n"))}))

(defn get-fossil-version []
  (def fossil-info
    (try
      (spawn-and-wait "fossil" "info")
      ((_err _fib)
       # Fossil is not installed
       nil)))
  (def info-table @{})
  (when fossil-info
    (each [k v] (peg/match fossil-info-peg fossil-info)
      (put info-table k v)))
  (when-let [hash-and-time (in info-table "checkout")]
    (def matched
      (peg/match ~(sequence (capture (some (range "af" "AF" "09"))) :s+ (thru -1))
                  hash-and-time))
    (when matched
      (in matched 0))))

(defn get-git-version []
  (try
    (string/trim
     (spawn-and-wait "git" "rev-parse" "HEAD"))
    ((_err _fib)
     nil)))

(def fossil-origin-name-peg
  (peg/compile
   ~{:main (sequence (to :fossil-origin-name) :fossil-origin-name :fossil-checkout :s* -1)
     :fossil-origin-name (sequence "FossilOrigin-Name:" :s*)
     :fossil-checkout (capture (some (range "af" "AF" "09")))}))

(defn get-git-fossil-origin-version []
  (when-let [git-version (get-git-version)]
    (def git-msg
      (try
        (spawn-and-wait "git" "show" "-s" "--format=format:%B" git-version)
        ((_err _fib)
         nil)))
    (when (nil? git-msg)
      (break nil))

    (def matched (peg/match fossil-origin-name-peg git-msg))
    (when (nil? matched)
      (break nil))

    (in matched 0)))

(defn get-vcs-version []
  (def fossil-version (get-fossil-version))
  (when fossil-version
    (break [:fossil fossil-version]))

  (def fossil-origin-version (get-git-fossil-origin-version))
  (when fossil-origin-version
    (break [:fossil-origin fossil-origin-version]))

  (def git-version (get-git-version))
  (when git-version
    (break [:git git-version]))

  nil)

(defn generate-resource-header [env out-file-name]
  (with [out-file (file/open out-file-name :wn)]
    (eachp [k v] env
      (when (and (table? v)
                 (v :resource)
                 (v :value))
        (file/write out-file (string/format "#define %s %v\n" k (v :value)))))))

(defn generated [name]
  (string (find-build-dir) name))


(defmacro gen-rule [target deps & body]
  ~(let [_target ,target
         _deps   ,deps]
     (rule _target [;_deps]
       (ensure-dir (find-build-dir))
       ,;body
       (printf "Generated %s" _target))))


(gen-rule (generated "resource.h") ["src/resource.janet"]
  (generate-resource-header (dofile "src/resource.janet") _target))


(gen-rule (generated "resource.res") ["res/jwno.rc"
                                      "res/jwno.ico"
                                      (generated "resource.h")]
  (spawn-and-wait "rc.exe" "/I" (find-build-dir) "/fo" _target (_deps 0)))


(gen-rule (generated "resource.obj") [(generated "resource.res")]
  (spawn-and-wait "cvtres.exe" "/machine:x64" (string "/out:" _target) (_deps 0)))


(gen-rule (generated "winmain_stub.o") []
  (spawn-and-wait "cl.exe" "/c" "/nologo" "/MD" "/O2" (string "/Fo" _target) "c/winmain_stub.c"))

(add-dep (generated "winmain_stub.o")
         "c/winmain_stub.c")


(task "embed-manifest" [(generated "jwno.exe")]
  (let [manifest "manifest/jwno.manifest"
        exe-file (generated "jwno.exe")]
    (spawn-and-wait "mt.exe" "-manifest" manifest (string "-outputresource:" exe-file ";#1"))
    (printf "Embedded %s into %s" manifest exe-file)))


(task "vcs-version" []
  (def vcs-version-file (generated "vcs-version.txt"))
  (def vcs-version (get-vcs-version))
  (printf "Detected source version: %n" vcs-version)
  (def abbr-hash-len 10)
  (def cur-version
    (match vcs-version
      [:fossil fossil-version]
      (string/format "fossil-%s" (string/slice fossil-version 0 abbr-hash-len))

      [:fossil-origin fossil-origin-version]
      (string/format "fossil-%s" (string/slice fossil-origin-version 0 abbr-hash-len))

      [:git git-version]
      (string/format "git-%s" (string/slice git-version 0 abbr-hash-len))

      nil
      # There's no version control
      nil))
  (def old-version
    (try
      (string/trim (slurp vcs-version-file))
      ((_err _fib)
       nil)))

  (printf "Old vcs-version: %n" old-version)
  (printf "Current vcs-version: %n" cur-version)

  (when (and cur-version
             (not= cur-version old-version))
    (ensure-dir (find-build-dir))
    (spit vcs-version-file cur-version)
    (try
      # So that the next build will try to use the new version info
      (os/rm (generated "resource.h"))
      ((_err _fib) :ignore))))


(declare-executable
 :name "jwno"
 :entry "src/main.janet"
 :deps [;(->> (os/dir "src")
              (filter |(string/has-suffix? ".janet" $))
              (map |(string "src/" $)))
        (generated "resource.obj")
        (generated "winmain_stub.o")]

 # Cannot embed the manifest directly when linking, or there'll be this warning
 #   warning LNK4078: multiple '.rsrc' sections found with different attributes
 # And the built binary would fail to launch. We put the manifest in with mt.exe
 # after the binary is built. See the embed-manifest task.

 #:lflags ["/nologo"
 #         "/manifest:embed"
 #         "/manifestinput:manifest/jwno.manifest"]

 :ldflags [(generated "winmain_stub.o") (generated "resource.obj") "/subsystem:windows"])
