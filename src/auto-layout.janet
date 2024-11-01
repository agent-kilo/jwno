#
# To use the objects below, `(import jwno/auto-layout)` in your
# config file.
#

(use jw32/_uiautomation)

(use ./util)


# ================== Auto-Close Empty Frame ==================
#
# Automatically check for empty frames and close them, when a window
# is removed. To use it, add these in your config:
#
#     (def auto-close-empty-frame (auto-layout/close-empty-frame jwno/context))
#     (:enable auto-close-empty-frame)
#
# To stop it:
#
#     (:disable auto-close-empty-frame)
#

(defn close-empty-frame-on-window-removed [self dead-win]
  (def parent (in dead-win :parent))
  (unless (:attached? parent)
    (break))

  (def {:window-manager window-man} self)
  (when (and (empty? (in parent :children))
             # Don't touch the top-level frame
             (nil? (in parent :monitor)))
    (with-activation-hooks window-man
      (:close parent))
    (def to-retile (in parent :parent))
    # ev/spawn to put the :retile call in the event queue
    (ev/spawn
     (:retile window-man to-retile))))


(defn close-empty-frame-enable [self]
  (:disable self) # To prevent multiple hook entries
  (def hook-fn
    (:add-hook (in self :hook-manager) :window-removed
       (fn [& args]
         (:on-window-removed self ;args))))
  (put self :hook-fn hook-fn))


(defn close-empty-frame-disable [self]
  (def hook-fn (in self :hook-fn))
  (when hook-fn
    (put self :hook-fn nil)
    (:remove-hook (in self :hook-manager) :window-removed hook-fn)))


(def close-empty-frame-proto
  @{:on-window-removed close-empty-frame-on-window-removed
    :enable close-empty-frame-enable
    :disable close-empty-frame-disable})


(defn close-empty-frame [context]
  (def {:window-manager window-man
        :hook-manager hook-man}
    context)
  (table/setproto
   @{:window-manager window-man
     :hook-manager hook-man}
   close-empty-frame-proto))


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
    (def [width height] (rect-size rect))
    (if (> height width)
      (:split cur-frame :vertical)
      (:split cur-frame :horizontal))
    (put (in win :tags) :frame (get-in cur-frame [:children 1]))
    (def to-retile (get-in cur-frame [:children 0]))
    # ev/spawn to put the :retile call in the event queue
    (ev/spawn
     (:retile window-man to-retile))))


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
  (unless cur-frame
    (break))
  (def top-frame (:get-top-frame cur-frame))
  (def last-focus (:get-current-window top-frame))

  (with-activation-hooks window-man
    (:flatten top-frame)
    (def all-wins (in top-frame :children))

    (when (empty? all-wins)
      (break))

    (var fr top-frame)
    # The first window is already in fr
    (:reset-visual-state (first all-wins) true false)
    (each w (slice all-wins 1)
      (def [fr-width fr-height] (rect-size (in fr :rect)))
      (if (> fr-height fr-width)
        (:split fr :vertical)
        (:split fr :horizontal))
      (set fr (get-in fr [:children 1]))
      (:reset-visual-state w true false)
      (:add-child fr w))

    (:retile window-man top-frame)
    (if last-focus
      (:activate last-focus)
      (:set-focus window-man top-frame))))


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
