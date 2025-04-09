(import ./win)
(import ./log)

(use jw32/_dwmapi)
(use jw32/_util)
(use ./util)


(defn history-stack []
  [@[] @[]])


(defn history-stack-push [stack x]
  (def [bottom top] stack)
  (array/clear top)
  (array/push bottom x)
  stack)


(defn history-stack-undo [stack]
  (def [bottom top] stack)
  (def x
    (when (< 1 (length bottom))
      (array/pop bottom)))
  (when x
    (array/push top x))
  (last bottom))


(defn history-stack-redo [stack]
  (def [bottom top] stack)
  (def x (array/pop top))
  (when x
    (array/push bottom x))
  (last bottom))


(defn layout-history-on-layout-changed [self lo]
  (def {:stacks stacks} self)
  (def lo-id (in lo :id))
  (def dump (:dump lo))
  (def lo-stack (in stacks lo-id (history-stack)))
  (history-stack-push lo-stack dump)
  (put stacks lo-id lo-stack))


(defn layout-history-enable [self]
  (:disable self)

  (def {:window-manager window-man
        :hook-manager hook-man
        :hook-fns hook-fns}
    self)

  # Clear and re-init history stacks every time
  (put self :stacks @{})
  (each lo (get-in window-man [:root :children])
    (:on-layout-changed self lo))

  (put hook-fns :layout-changed
     (:add-hook hook-man :layout-changed
        (fn [& args]
          (:on-layout-changed self ;args))))
  (put hook-fns :layout-created
     (:add-hook hook-man :layout-created
        (fn [& args]
          (:on-layout-changed self ;args)))))


(defn layout-history-disable [self]
  (def {:hook-manager hook-man
        :hook-fns hook-fns}
    self)
  (eachp [h f] (table/clone hook-fns)
    (put hook-fns h nil)
    (:remove-hook hook-man h f)))


(defn place-excessive-windows [lo hwnd-list all-win-list]
  (def win-map @{})
  (each w all-win-list
    (put win-map (in w :hwnd) w))

  (log/debug "all windows: %n" (keys win-map))
  (log/debug "excessive windows: %n" hwnd-list)

  (def all-leaves (:get-all-leaf-frames lo))
  (def first-frame (:get-first-frame lo))

  (each hwnd hwnd-list
    (when-let [win (in win-map hwnd)]
      (def hwnd-rect (DwmGetWindowAttribute hwnd DWMWA_EXTENDED_FRAME_BOUNDS))
      (def [found-fr _ dist]
        (win/find-closest-frame hwnd-rect all-leaves))
      (def target-fr
        (if found-fr
          (do
            (log/debug "found frame %n for window %n, dist = %n"
                       (in found-fr :rect) hwnd-rect dist)
            found-fr)
          # else
          (do
            (log/debug "can not find a close frame, fallback to first frame")
            first-frame)))
      (:add-child target-fr win))))


(defn load-layout [lo dump wm uia-man]
  (def focused-hwnd
    (with-uia [uia-win (:get-focused-window uia-man)]
      (when uia-win
        (def hwnd? (:get_CachedNativeWindowHandle uia-win))
        (if (null? hwnd?)
          nil
          hwnd?))))
  (def win-list (:get-all-windows lo))

  # Clear all top frames first, or there may be duplicate window objects
  # linked to different parents, since we're generating all windows anew
  # below.
  (each fr (in lo :children)
    (:clear-children fr))

  (def exc-hwnd-map (:load lo dump (map |(in $ :hwnd) win-list)))
  (def exc-hwnd-list (values exc-hwnd-map))
  (place-excessive-windows lo exc-hwnd-list win-list)
  (:retile wm lo)
  (when-let [w (:find-hwnd lo focused-hwnd)]
    (:activate w)))


(defn layout-history-undo [self lo]
  (def {:window-manager window-man
        :uia-manager uia-man}
    self)
  (def lo-id (in lo :id))
  (def lo-stack (get-in self [:stacks lo-id]))
  (def dump (history-stack-undo lo-stack))
  (load-layout lo dump window-man uia-man))


(defn layout-history-redo [self lo]
  (def {:window-manager window-man
        :uia-manager uia-man}
    self)
  (def lo-id (in lo :id))
  (def lo-stack (get-in self [:stacks lo-id]))
  (def dump (history-stack-redo lo-stack))
  (load-layout lo dump window-man uia-man))


(defn layout-history-set-manual [self manual?]
  (def {:hook-fns hook-fns
        :hook-manager hook-man}
    self)
  (def hook-fn (in hook-fns :layout-changed))
  (cond
    (and hook-fn manual?)
    (do
      (put hook-fns :layout-changed nil)
      (:remove-hook hook-man :layout-changed hook-fn))

    (and (nil? hook-fn) (not manual?))
    (do
      (def new-fn
        (:add-hook hook-man :layout-changed
           (fn [& args]
             (:on-layout-changed self ;args))))
      (put hook-fns :layout-changed new-fn))))


(defn layout-history-push [self lo]
  (:on-layout-changed self lo))


(def layout-history-proto
  @{:on-layout-changed layout-history-on-layout-changed
    :enable layout-history-enable
    :disable layout-history-disable
    :undo layout-history-undo
    :redo layout-history-redo
    :push layout-history-push
    :set-manual layout-history-set-manual})


(defn layout-history [context]
  (def {:window-manager window-man
        :uia-manager uia-man
        :hook-manager hook-man}
    context)
  (table/setproto
   @{:window-manager window-man
     :uia-manager uia-man
     :hook-manager hook-man
     :stacks @{}
     :hook-fns @{}}
   layout-history-proto))
