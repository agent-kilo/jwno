(import ./log)


(defn build-module-cache [mod-list]
  (def mod-cache @{})
  (each name mod-list
    (put mod-cache name (require name)))
  mod-cache)


(def- BUILT-IN-MODULE-CACHE
  (build-module-cache
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
    "jw32/_winuser"]))


(defn- check-module-in-cache [name]
  (if (has-key? BUILT-IN-MODULE-CACHE name)
    name))


(defn- load-built-in-module [name args]
  (log/debug "Loading built-in module: %n, args = %n" name args)
  (in BUILT-IN-MODULE-CACHE name))


(defn module-manager-register-loader [self]
  (put module/loaders :jwno-built-in-module load-built-in-module)
  (array/insert module/paths 0 [check-module-in-cache :jwno-built-in-module]))


(defn module-manager-unregister-loader [self]
  (var i 0)
  (while (< i (length module/paths))
    (if (= (in module/paths i)
           [check-module-in-cache :jwno-built-in-module])
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
