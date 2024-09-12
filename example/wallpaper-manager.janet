#
# This is an example module for Jwno, in pure Janet.
#
# To use it, place this file alongside your config file,
# and add these code in your config:
#
#     (import wallpaper-manager)
#     (def wp-man (wallpaper-manager/wallpaper-manager))
#
# Then you can use wp-man's methods to control your wallpaper.
# See wallpaper-manager-proto below for the available methods.
#


(use jw32/_combaseapi)
(use jw32/_shobjidl_core)


(defn- wallpaper-manager-get-wallpaper [self monitor]
  (:GetWallpaper (in self :com) monitor))

(defn- wallpaper-manager-set-wallpaper [self monitor wallpaper]
  (:SetWallpaper (in self :com) monitor wallpaper))

(defn- wallpaper-manager-get-monitor-count [self]
  (:GetMonitorDevicePathCount (in self :com)))

(defn- wallpaper-manager-get-monitor-at [self index]
  (:GetMonitorDevicePathAt (in self :com) index))

(defn- wallpaper-manager-destroy [self]
  (:Release (in self :com))
  (put self :com nil))


(def wallpaper-manager-proto
  @{:get-wallpaper wallpaper-manager-get-wallpaper
    :set-wallpaper wallpaper-manager-set-wallpaper
    :get-monitor-count wallpaper-manager-get-monitor-count
    :get-monitor-at wallpaper-manager-get-monitor-at
    :destroy wallpaper-manager-destroy})


(defn wallpaper-manager [&]
  (def dw-com
    (CoCreateInstance CLSID_DesktopWallpaper
                      nil
                      CLSCTX_LOCAL_SERVER
                      IDesktopWallpaper))
  (table/setproto
   @{:com dw-com}
   wallpaper-manager-proto))
