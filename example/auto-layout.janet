#
# This module contains some basic demo of automatically managing
# the layout of frames. To use this module, place it alongside your
# config file and import it:
#
#     (import auto-layout)
#
# See comments below for the usage of a specific object.
#

(import jwno/util)
(use jw32/_uiautomation)


# ================== BSP Layout ==================
#
# Automatically split and arrange frames in the good old BSP fasion.
# To use it, add these in your config:
#
#     (def bsp-layout (auto-layout/bsp jwno/context))
#     (:enable bsp-layout)
#
# To stop it:
#
#     (:disable bsp-layout)
#

(defn bsp-on-window-created [self win uia-win _exe-path desktop-info]
  # Don't create new frames for these windows
  (cond
    (or (= "#32770" (:get_CachedClassName uia-win))
        (not= 0 (:GetCurrentPropertyValue uia-win UIA_IsDialogPropertyId)))
    # Dialog windows
    (break))

  (def {:window-manager window-man} self)
  (def cur-frame
    (:get-current-frame-on-desktop (in window-man :root) desktop-info))
  (unless (empty? (in cur-frame :children))
    (def rect (in cur-frame :rect))
    (def [width height] (util/rect-size rect))
    (if (> height width)
      (:split cur-frame :vertical)
      (:split cur-frame :horizontal))
    (put (in win :tags) :frame (get-in cur-frame [:children 1]))
    # ev/spawn to put the :retile call in the event queue
    (ev/spawn
     (:retile window-man (get-in cur-frame [:children 0])))))


(defn bsp-enable [self]
  (:disable self) # To prevent multiple hook entries
  (def hook-fn
    (:add-hook (in self :hook-manager) :window-created
       (fn [& args]
         (:on-window-created self ;args))))
  (put self :hook-fn hook-fn))


(defn bsp-disable [self]
  (def hook-fn (in self :hook-fn))
  (when hook-fn
    (put self :hook-fn nil)
    (:remove-hook (in self :hook-manager) :window-created hook-fn)))


(defn bsp-refresh [self]
  (def {:window-manager window-man} self)
  (def cur-frame (:get-current-frame (in window-man :root)))
  (def top-frame (:get-top-frame cur-frame))

  (:flatten top-frame)
  (def last-focus (:get-current-window top-frame))
  (def all-wins (in top-frame :children))

  (when (empty? all-wins)
    (break))

  (var fr top-frame)
  # The first window is already in fr
  (each w (slice all-wins 1)
    (def [fr-width fr-height] (util/rect-size (in fr :rect)))
    (if (> fr-height fr-width)
      (:split fr :vertical)
      (:split fr :horizontal))
    (set fr (get-in fr [:children 1]))
    (:add-child fr w))

  (:retile window-man top-frame)
  (:activate window-man last-focus))


(def bsp-proto
  @{:on-window-created bsp-on-window-created
    :enable bsp-enable
    :disable bsp-disable
    :refresh bsp-refresh})


(defn bsp [context]
  (def {:window-manager window-man
        :hook-manager hook-man}
    context)
  (table/setproto
   @{:window-manager window-man
     :hook-manager hook-man}
   bsp-proto))


# ================== Auto Zoom-In ==================
#
# Automatically :zoom-in to the activated frame.
# To use it, add these lines in your config:
#
#     (def auto-zoom-in (auto-layout/zoom-in jwno/context))
#     (:enable auto-zoom-in)
#
# To stop it:
#
#     (:disable auto-zoom-in)
#

(defn zoom-in-on-window-activated [self win]
  (ev/spawn
   (:call-command (in self :command-manager) :zoom-in (in self :ratio))))


(defn zoom-in-enable [self]
  (:disable self) # To prevent multiple hook entries
  (def hook-fn
    (:add-hook (in self :hook-manager) :window-activated
       (fn [& args]
         (:on-window-activated self ;args))))
  (put self :hook-fn hook-fn))


(defn zoom-in-disable [self]
  (def hook-fn (in self :hook-fn))
  (when hook-fn
    (put self :hook-fn nil)
    (:remove-hook (in self :hook-manager) :window-activated hook-fn)))


(def zoom-in-proto
  @{:on-window-activated zoom-in-on-window-activated
    :enable zoom-in-enable
    :disable zoom-in-disable})


(defn zoom-in [context &opt ratio]
  (default ratio 0.7)

  (def {:command-manager command-man
        :hook-manager hook-man}
    context)
  (table/setproto
   @{:command-manager command-man
     :hook-manager hook-man
     :ratio ratio}
   zoom-in-proto))
