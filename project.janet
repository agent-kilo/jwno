(declare-project
 :name "Jwno"
 :description "A window manager built with Janet."
 :dependencies [{:url "https://github.com/janet-lang/spork.git"
                 :tag "253a67e89dca695632283ef60f77851311c404c9"}])


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
