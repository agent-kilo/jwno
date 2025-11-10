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
      "jw32/_heapapi"
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
      "jw32/_wingdi"
      "jw32/_winreg"
      "jw32/_winuser"])

    # Jwno modules
    (build-module-cache
     cache
     [["./auto-layout" "jwno/auto-layout"]
      ["./indicator" "jwno/indicator"]
      ["./ui-hint" "jwno/ui-hint"]
      ["./scratch-pad" "jwno/scratch-pad"]
      ["./layout-history" "jwno/layout-history"]
      ["./util" "jwno/util"]
      ["./log" "jwno/log"]])

    # See main/late-init for jwno/user-config

    cache))


(defn- check-module-not-relative [name]
  # See check-is-dep function in boot.janet
  (unless (or (string/has-prefix? "/" name)
              (string/has-prefix? "@" name)
              (string/has-prefix? "." name))
    name))


(defn module-manager-register-loader [self & paths]
  # To ensure we have no more than one entry in module/paths
  (:unregister-loader self)

  (put module/loaders
       :jwno-cached-module
       (fn [name args]
         (:load-cached-module self name args)))

  (array/insert module/paths
                0
                [(fn [name] (:check-module-in-cache self name))
                 :jwno-cached-module])

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
    (match entry
      [_ :jwno-cached-module]
      (array/remove module/paths i)

      [_ _ (@ check-module-not-relative)]
      (array/remove module/paths i)

      _
      (++ i)))
  (put module/loaders :jwno-cached-module nil))


(defn module-manager-add-cached-module [self name mod-env]
  (put (in self :cache) name mod-env))


(defn module-manager-remove-cached-module [self name]
  (put (in self :cache) name nil))


(defn module-manager-check-module-in-cache [self name]
  (when (has-key? (in self :cache) name)
    name))


(defn module-manager-load-cached-module [self name _args]
  (log/debug "Loading cached module: %n, args = %n" name _args)
  (get-in self [:cache name]))


(def- module-manager-proto
  @{:register-loader module-manager-register-loader
    :unregister-loader module-manager-unregister-loader
    :add-cached-module module-manager-add-cached-module
    :remove-cached-module module-manager-remove-cached-module
    :check-module-in-cache module-manager-check-module-in-cache
    :load-cached-module module-manager-load-cached-module})


(defn module-manager []
  (def cache (table/setproto @{} BUILT-IN-MODULE-CACHE))
  (table/setproto
   @{:cache cache}
   module-manager-proto))
