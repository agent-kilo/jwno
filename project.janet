(declare-project
 :name "Jwno"
 :description "A window manager built with Janet."
 :dependencies [{:url "https://github.com/janet-lang/spork.git"
                 :tag "v1.1.1"}])


(import ./script/util)
(import ./script/vcs)


(def DEBUG-CFLAGS
  (if (= "debug" (dyn :build-type))
    ["/Zi" (string "/Fd" (find-build-dir))]
    []))

# Only used in manually specified compiler commands. In commands generated
# by JPM, these flags are derived from (dyn :build-type)
(def OPTIMIZE-CFLAGS
  (if (= "debug" (dyn :build-type))
    ["/Od"]
    ["/O2"]))

(def DEBUG-LDFLAGS
  (if (= "debug" (dyn :build-type))
    ["/DEBUG"]
    []))


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
       (util/ensure-dir (find-build-dir))
       ,;body
       (printf "Generated %s" _target))))


(gen-rule (generated "resource.h") ["src/resource.janet"]
  (generate-resource-header (dofile "src/resource.janet") _target))


(gen-rule (generated "resource.res") ["res/jwno.rc"
                                      "res/jwno.ico"
                                      (generated "resource.h")]
  (util/spawn-and-wait "rc.exe" "/I" (find-build-dir) "/fo" _target (_deps 0)))


(gen-rule (generated "resource.obj") [(generated "resource.res")]
  (util/spawn-and-wait "cvtres.exe" "/machine:x64" (string "/out:" _target) (_deps 0)))


(gen-rule (generated "winmain_stub.o") ["c/winmain_stub.c"]
  (util/spawn-and-wait "cl.exe" "/c" ;(dyn :cflags) ;OPTIMIZE-CFLAGS ;DEBUG-CFLAGS (string "/Fo" _target) "c/winmain_stub.c"))


(task "embed-manifest" [(generated "jwno.exe")]
  (let [manifest "manifest/jwno.manifest"
        exe-file (generated "jwno.exe")]
    (util/spawn-and-wait "mt.exe" "-manifest" manifest (string "-outputresource:" exe-file ";#1"))
    (printf "Embedded %s into %s" manifest exe-file)))


(task "vcs-version" []
  (def vcs-version-file (generated "vcs-version.txt"))
  (def vcs-version (vcs/get-vcs-version))
  (printf "Detected source version: %n" vcs-version)

  (def cur-version
    (vcs/format-vcs-version-string vcs-version 10))
  (def old-version
    (try
      (string/trim (slurp vcs-version-file))
      ((_err _fib)
       nil)))

  (printf "Old vcs-version: %n" old-version)
  (printf "Current vcs-version: %n" cur-version)

  (when (and cur-version
             (not= cur-version old-version))
    (util/ensure-dir (find-build-dir))
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

 :cflags [;(dyn :cflags) ;DEBUG-CFLAGS]

 # Cannot embed the manifest directly when linking, or there'll be this warning
 #   warning LNK4078: multiple '.rsrc' sections found with different attributes
 # And the built binary would fail to launch. We put the manifest in with mt.exe
 # after the binary is built. See the embed-manifest task.

 #:lflags ["/nologo"
 #         "/manifest:embed"
 #         "/manifestinput:manifest/jwno.manifest"]

 :ldflags [(generated "winmain_stub.o") (generated "resource.obj") "/subsystem:windows" ;DEBUG-LDFLAGS])
