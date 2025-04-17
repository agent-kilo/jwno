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
    (:layouts-changed window-man [(:get-layout to-retile)])
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


# ================== Common Code for Auto Layouts ==================

(var auto-layout-default-filter-hook-fn nil)

(defn auto-layout-default-filter [win uia-win exe-path desktop-info]
  (cond
    (= "#32770" (:get_CachedClassName uia-win))
    false

    (not= 0 (:GetCurrentPropertyValue uia-win UIA_IsDialogPropertyId))
    false

    true))


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


(defn bsp-on-window-created [self win uia-win exe-path desktop-info]
  (def filter-result
    (:call-filter-hook
       (in self :hook-manager)
       :and
       :filter-auto-layout-window
       win uia-win exe-path desktop-info))

  (unless filter-result
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

  (def hook-man (in self :hook-manager))

  (unless auto-layout-default-filter-hook-fn
    (set auto-layout-default-filter-hook-fn
         (:add-hook hook-man :filter-auto-layout-window
            auto-layout-default-filter)))

  (put self :hook-fn
     (:add-hook hook-man :window-created
        (fn [& args]
          (:on-window-created self ;args)))))


(defn bsp-disable [self]
  (def {:hook-manager hook-man
        :hook-fn hook-fn}
    self)

  (when hook-fn
    (put self :hook-fn nil)
    (:remove-hook hook-man :window-created hook-fn)))


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

    (:layouts-changed window-man [(:get-layout top-frame)])

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


# ================== Rows Layout ==================
#
# Automatically split and arrange frames into horizontal or vertical rows.
# To use it, add these in your config:
#
#     (def rows-layout (auto-layout/rows jwno/context))
#     (put rows-layout :direction :horizontal)  # or :vertical, defaults to :horizontal
#     (:enable rows-layout)
#
# To stop it:
#
#     (:disable rows-layout)
#


(defn rows-on-window-created [self win uia-win exe-path desktop-info]
  (def filter-result
    (:call-filter-hook
       (in self :hook-manager)
       :and
       :filter-auto-layout-window
       win uia-win exe-path desktop-info))

  (unless filter-result
    (break))

  (def {:window-manager window-man} self)
  (def cur-frame
    (:get-current-frame-on-desktop (in window-man :root) desktop-info))
  (unless (empty? (in cur-frame :children))
    (def cur-top-frame (:get-top-frame cur-frame))
    (def top-dir (:get-direction cur-top-frame))
    (def rows-dir (in self :direction))
    (def target-fr
      (cond
        (nil? top-dir)
        (do
          (:split cur-top-frame rows-dir)
          (get-in cur-top-frame [:children 1]))

        (= top-dir rows-dir)
        (do
          (:insert-sub-frame cur-top-frame -1)
          (last (in cur-top-frame :children)))

        (not= top-dir rows-dir)
        (do
          (def orig-children (slice (in cur-top-frame :children)))
          (array/clear (in cur-top-frame :children))
          (put cur-top-frame :current-child nil)
          (def orig-proto (table/getproto cur-top-frame))
          (:split cur-top-frame rows-dir)
          (def fr (get-in cur-top-frame [:children 0]))
          (each vc orig-children
            (put vc :parent nil)
            (:add-child fr vc))
          (table/setproto fr orig-proto)
          # re-calculate sub-frame rects
          (:transform fr (in fr :rect))
          # return target frame
          (last (in cur-top-frame :children)))))

    (put (in win :tags) :frame target-fr)

    # ev/spawn to put the :retile call in the event queue
    (ev/spawn
     (:retile window-man cur-top-frame))))


(defn rows-enable [self]
  (:disable self)

  (def hook-man (in self :hook-manager))

  (unless auto-layout-default-filter-hook-fn
    (set auto-layout-default-filter-hook-fn
         (:add-hook hook-man :filter-auto-layout-window
            auto-layout-default-filter)))

  (put self :hook-fn
     (:add-hook hook-man :window-created
        (fn [& args]
          (:on-window-created self ;args)))))


(defn rows-disable [self]
  (def {:hook-manager hook-man
        :hook-fn hook-fn}
    self)

  (when hook-fn
    (put self :hook-fn nil)
    (:remove-hook hook-man :window-created hook-fn)))


(defn rows-refresh [self]
  (def {:window-manager window-man
        :direction rows-dir}
    self)

  (def cur-frame (:get-current-frame (in window-man :root)))
  (unless cur-frame
    (break))
  (def top-frame (:get-top-frame cur-frame))
  (def last-focus (:get-current-window top-frame))
  (def all-wins (:get-all-windows top-frame))

  (with-activation-hooks window-man
    (array/clear (in top-frame :children))
    (put top-frame :current-child nil)

    (when (empty? all-wins)
      (break))

    (:split top-frame rows-dir (length all-wins))
    (map (fn [fr win]
           (:reset-visual-state win true false)
           (:add-child fr win))
         (in top-frame :children)
         all-wins)

    (:layouts-changed window-man [(:get-layout top-frame)])

    (:retile window-man top-frame)
    (if last-focus
      (:activate last-focus)
      (:set-focus window-man top-frame))))


(def rows-proto
  @{:on-window-created rows-on-window-created
    :enable rows-enable
    :disable rows-disable
    :refresh rows-refresh})


(defn rows [context]
  (def {:window-manager window-man
        :hook-manager hook-man}
    context)
  (table/setproto
   @{:window-manager window-man
     :hook-manager hook-man

     # default settings
     :direction :horizontal}
   rows-proto))


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

(defn zoom-in-on-frame-activated [self win]
  (ev/spawn
   (:call-command (in self :command-manager) :zoom-in (in self :ratio))))


(defn zoom-in-enable [self]
  (:disable self) # To prevent multiple hook entries
  (def hook-fn
    (:add-hook (in self :hook-manager) :frame-activated
       (fn [& args]
         (:on-frame-activated self ;args))))
  (put self :hook-fn hook-fn))


(defn zoom-in-disable [self]
  (def hook-fn (in self :hook-fn))
  (when hook-fn
    (put self :hook-fn nil)
    (:remove-hook (in self :hook-manager) :frame-activated hook-fn)))


(def zoom-in-proto
  @{:on-frame-activated zoom-in-on-frame-activated
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
