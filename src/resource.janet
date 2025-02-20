(import spork/path)


(def IDI_LOGO
  "Main logo."
  {:resource :icon}
  101)


(def VERSION_MAJOR
  "Major version."
  {:resource :versioninfo}
  0)
(def VERSION_MINOR
  "Minor version."
  {:resource :versioninfo}
  9)
(def VERSION_PATCH
  "Patch level."
  {:resource :versioninfo}
  11)
(def VERSION_INTERNAL
  "Internal version number."
  {:resource :versioninfo}
  0)

(def VERSION_VCS
  "VCS version."
  {:resource :versioninfo}
  (try
    (do
      (def print-path |(do (printf "Reading %s for VCS version info..." $) $))
      (def cur-file (dyn *current-file*))
      # This happens at compile-time. Should make sure the relative
      # path to build dir is correct when compiling. The actual version
      # string is generated in project.janet.
      (-> cur-file
          (path/abspath)
          (path/dirname)
          (path/join ".." "build" "vcs-version.txt")
          (print-path)
          (slurp)
          (string/trim)))
    ((_err _fib)
     # XXX: In the case of failure, always assume the version file does not exist.
     nil)))


(def ID_MENU_EXIT
  "Exit command in the notify icon menu."
  (int/u64 90001))

(def ID_MENU_RESET_KBD_HOOKS
  "Reset Keyboard Hooks command in the notify icon menu."
  (int/u64 90002))

(def ID_MENU_UPDATE_MONITOR_LAYOUT
  "Update Monitor Layout command in the notify icon menu."
  (int/u64 90003))

(def ID_MENU_LAUNCH_REPL
  "Launch REPL command in the notify icon menu."
  (int/u64 90004))

(def ID_MENU_VERSION
  "Jwno version in the notify icon menu."
  (int/u64 90005))
