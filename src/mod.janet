(import ./log)


(defn build-module-cache [mod-cache mod-list]
  (each mod mod-list
    (match mod
      [name cache-name]
      (put mod-cache cache-name (require name))

      name
      (put mod-cache name (require name))))
  mod-cache)


(def- BUILT-IN-MODULE-CACHE
  (let [cache @{}]
    # Jw32 modules
    (build-module-cache
     cache
     ["jw32/_combaseapi"
      "jw32/_commctrl"
      "jw32/_consoleapi"
      "jw32/_dwmapi"
      "jw32/_errhandlingapi"
      "jw32/_handleapi"
      "jw32/_libloaderapi"
      "jw32/_oaidl"
      "jw32/_processthreadsapi"
      "jw32/_securitybaseapi"
      "jw32/_shellapi"
      "jw32/_shobjidl_core"
      "jw32/_uiautomation"
      "jw32/_util"
      "jw32/_winbase"
      "jw32/_winnt"
      "jw32/_winuser"])

    # Jwno modules
    (build-module-cache
     cache
     [["./auto-layout" "jwno/auto-layout"]
      ["./util" "jwno/util"]
      ["./log" "jwno/log"]])

    cache))


(defn- check-module-in-cache [name]
  (if (has-key? BUILT-IN-MODULE-CACHE name)
    name))


(defn- check-module-not-relative [name]
  # See check-is-dep function in boot.janet
  (unless (or (string/has-prefix? "/" name)
              (string/has-prefix? "@" name)
              (string/has-prefix? "." name))
    name))


(defn- load-built-in-module [name args]
  (log/debug "Loading built-in module: %n, args = %n" name args)
  (in BUILT-IN-MODULE-CACHE name))


(defn module-manager-register-loader [self & paths]
  # To ensure we have no more than one entry in module/paths
  (:unregister-loader self)

  (put module/loaders :jwno-built-in-module load-built-in-module)

  (array/insert module/paths 0 [check-module-in-cache :jwno-built-in-module])

  (def to-insert
    (if-let [idx (find-index |(= :preload (in $ 1)) module/paths)]
      # Put our paths AFTER the default module cache
      (+ 1 idx)
      # There's no default cache, go after our own cache
      1))
  (each p paths
    (array/insert module/paths
                  to-insert
                  [(string p "/:all::native:")
                   :native
                   check-module-not-relative])
    (array/insert module/paths
                  to-insert
                  [(string p "/:all:/init.janet")
                   :source
                   check-module-not-relative])
    (array/insert module/paths
                  to-insert
                  [(string p "/:all:.janet")
                   :source
                   check-module-not-relative])
    (array/insert module/paths
                  to-insert
                  [(string p "/:all:.jimage")
                   :image
                   check-module-not-relative])))


(defn module-manager-unregister-loader [self]
  (var i 0)
  (while (< i (length module/paths))
    (def entry (in module/paths i))
    # See module-manager-register-loader for the entry structures
    (if (or (= entry
               [check-module-in-cache :jwno-built-in-module])
            (and (= 3 (length entry))
                 (= check-module-not-relative (in entry 2))))
      (array/remove module/paths i)
      (++ i)))
  (put module/loaders :jwno-built-in-module nil))


(def- module-manager-proto
  @{:register-loader module-manager-register-loader
    :unregister-loader module-manager-unregister-loader})


(defn module-manager []
  (table/setproto
   @{:cache BUILT-IN-MODULE-CACHE}
   module-manager-proto))
