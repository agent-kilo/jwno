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
  8)
(def VERSION_INTERNAL
  "Internal version number."
  {:resource :versioninfo}
  0)


(def ID_MENU_EXIT
  "Exit command in the notify icon menu."
  (int/u64 90001))

(def ID_MENU_RESET_KBD_HOOKS
  "Reset Keyboard Hooks command in the notify icon menu."
  (int/u64 90002))

(def ID_MENU_UPDATE_MONITOR_LAYOUT
  "Update Monitor Layout command in the notify icon menu."
  (int/u64 90003))
