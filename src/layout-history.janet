(import ./win)
(import ./log)

(import spork/path)

(use jw32/_winuser)
(use jw32/_dwmapi)
(use jw32/_util)
(use ./util)


(defn history-stack []
  [@[] @[]])


(defn history-stack-push [stack x &opt limit]
  (def [bottom top] stack)
  (array/clear top)
  (array/push bottom x)
  (def count (length bottom))
  (when (and limit
             (> count limit))
    (array/remove bottom 0 (- count limit)))
  stack)


(defn history-stack-peek [stack]
  (def [bottom top] stack)
  (last bottom))


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


#
# Manual mode and automatic mode have different timings when pushing
# a layout: Manual mode pushes BEFORE the layout change, and automatic
# mode pushes AFTER the change. This results in an off-by-one situation,
# which we have different push/peek/undo/redo logic to deal with.
#
(defn history-stack-manual-push [stack x &opt limit]
  (def [bottom top] stack)
  (def last-layout (last top))
  (array/clear top)
  (when last-layout
    (array/push bottom last-layout))
  (array/push top x)
  (def count (+ 1 (length bottom)))
  (when (and limit
             (> count limit))
    (array/remove bottom 0 (- count limit)))
  stack)


(defn history-stack-manual-peek [stack]
  (def [bottom top] stack)
  (last top))


(defn history-stack-manual-undo [stack]
  (def [bottom top] stack)
  (def x (array/pop bottom))
  (when x
    (array/push top x))
  (last top))


(defn history-stack-manual-redo [stack]
  (def [bottom top] stack)
  (def x
    (when (< 1 (length top))
      (array/pop top)))
  (when x
    (array/push bottom x))
  (last top))


(defn new-layout-state []
  @{:top-frame-count nil
    :history (history-stack)})


(defn layout-history-on-layout-changed [self lo]
  (def {:layout-states layout-states
        :manual manual-state}
    self)

  (def lo-id (in lo :id))
  (def lo-state (in layout-states lo-id (new-layout-state)))
  (put layout-states lo-id lo-state)

  (when manual-state
    (put lo-state :unsaved-changes true)
    # Early return
    (break))

  (def old-top-frame-count (in lo-state :top-frame-count))
  (def new-top-frame-count (length (in lo :children)))
  # The :layout-changed hook also gets triggered when a new monitor
  # is plugged in, but currently all new monitors are empty, and it's
  # meaningless to save an empty top frame, so we opt out in this case.
  (if (or (nil? old-top-frame-count)
            (<= new-top-frame-count old-top-frame-count))
    (:push self lo)
    # else, treat all empty top frames as "saved"
    (do
      (put lo-state :top-frame-count new-top-frame-count)
      (put lo-state :unsaved-changes nil))))


(defn layout-history-get-backing-file-path [self]
  (def bfile (in self :backing-file))
  (unless bfile
    # Early return
    (break nil))

  (def {:context context} self)
  (def {:data-dir data-dir} context)
  (if (path/abspath? bfile)
    bfile
    (path/join data-dir bfile)))


(defn layout-history-save-to-backing-file [self]
  (def dump-file (:get-backing-file-path self))
  (unless dump-file
    # Early return
    (break))

  (def to-write @{})
  (eachp [lo-id lo-state] (in self :layout-states)
    (def history (in lo-state :history))
    (def stack (array/join @[] ;history))
    (put to-write lo-id (tuple/slice stack)))
  (spit dump-file (string/format "%n" (table/to-struct to-write))))


(defn find-layout-by-id [wm lo-id]
  (var layout-found nil)
  (each lo (get-in wm [:root :children])
    (when (= lo-id (in lo :id))
      (set layout-found lo)
      (break)))
  layout-found)


(defn layout-history-load-from-backing-file [self]
  (def dump-file (:get-backing-file-path self))
  (unless dump-file
    # Early return
    (break false))

  (def text
    (try
      (slurp dump-file)
      ((err fib)
       (log/warning "failed to load from file: %s\n%s"
                    dump-file
                    (get-stack-trace fib))
       nil)))

  (unless text
    # Early return
    (break false))

  (def to-load
    (try
      (parse text)
      ((err fib)
       (log/warning "failed to parse loaded text: %n\n%s"
                    err
                    (get-stack-trace fib))
       nil)))

  (unless to-load
    # Early return
    (break false))

  (def {:context context
        :manual manual-state}
    self)
  (def {:window-manager window-man} context)

  (def layout-states @{})
  (eachp [lo-id lo-stack] to-load
    (def lo-state (new-layout-state))
    (put layout-states lo-id lo-state)

    # Restore :history
    (def history (in lo-state :history))
    (def bottom (first history))
    (each d lo-stack
      (array/push bottom d))

    (when manual-state
      # Adjust for the off-by-one case
      (history-stack-manual-undo history))

    # Restore :top-frame-count
    (def lo (find-layout-by-id window-man lo-id))
    (when lo
      (put lo-state :top-frame-count (length (in lo :children))))

    # XXX: Always assume the layout does not match the last entry in the history
    (put lo-state :unsaved-changes true))

  (put self :layout-states layout-states)
  true)


(defn same-user-session? [root-uia-elem]
  (def desktop-hwnd (:get_CachedNativeWindowHandle root-uia-elem))
  (def prop-name "_jwno-layout-history-same-user-session_")
  (if (null? (GetProp desktop-hwnd prop-name))
    (do
      (SetProp desktop-hwnd prop-name 1)
      false)
    true))


(defn load-last-history-entries [layout-states manual-state window-man uia-man]
  (def focused-hwnd
    (with-uia [uia-win (:get-focused-window uia-man)]
      (when uia-win
        (def hwnd? (:get_CachedNativeWindowHandle uia-win))
        (if (null? hwnd?)
          nil
          hwnd?))))

  (with-activation-hooks window-man
    (def root (in window-man :root))
    (:clear-children root)

    (each lo-state layout-states
      (def dump
        (if manual-state
          (history-stack-manual-redo (in lo-state :history))
          (history-stack-redo (in lo-state :history))))
      (when dump
        (def [_ dump-data] dump)
        (def [_ lo-id lo-name _] dump-data)
        (def vd-info {:id lo-id :name lo-name})
        (def lo (:new-layout root vd-info))
        (:add-child root lo)
        (:load lo dump-data)))

    (:retile window-man)
    (when-let [w (:find-hwnd root focused-hwnd)]
      (:activate w))))


(defn layout-history-enable [self &opt add-commands?]
  (default add-commands? true)

  (:disable self false)

  (def {:context context
        :hook-fns hook-fns
        :manual manual-state}
    self)
  (def {:window-manager window-man
        :uia-manager uia-man
        :hook-manager hook-man}
    context)

  (def same-session (same-user-session? (in uia-man :root)))
  # Clear and re-init history stacks every time
  (put self :layout-states @{})
  (def bfile-loaded (:load-from-backing-file self))
  (cond
    (not bfile-loaded)
    # No backing file is loaded, update the history to reflect
    # current layouts
    (each lo (get-in window-man [:root :children])
      (:on-layout-changed self lo))

    same-session
    # We got restarted in the same user session, try to restore last
    # history entry from backing file
    (load-last-history-entries (in self :layout-states)
                               manual-state
                               window-man
                               uia-man)

    # Otherwise, we're in a new user session, start afresh
    )

  (put hook-fns :layout-changed
     (:add-hook hook-man :layout-changed
        (fn [& args]
          (:on-layout-changed self ;args))))
  (put hook-fns :layout-created
     (:add-hook hook-man :layout-created
        (fn [& args]
          (:on-layout-changed self ;args))))
  (put hook-fns :shutting-down
     (:add-hook hook-man :shutting-down
        (fn [& _args]
          (:save-to-backing-file self))))

  (when add-commands?
    (:add-commands self)))


(defn layout-history-disable [self &opt save-backing-file]
  (default save-backing-file true)

  (def {:context context
        :hook-fns hook-fns}
    self)
  (def {:hook-manager hook-man} context)
  (eachp [h f] (table/clone hook-fns)
    (put hook-fns h nil)
    (:remove-hook hook-man h f))
  (:remove-commands self)

  (when save-backing-file
    (:save-to-backing-file self)))


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
  (def [_ts dump-data] dump)
  (def focused-hwnd
    (with-uia [uia-win (:get-focused-window uia-man)]
      (when uia-win
        (def hwnd? (:get_CachedNativeWindowHandle uia-win))
        (if (null? hwnd?)
          nil
          hwnd?))))
  (def win-list (:get-all-windows lo))

  (with-activation-hooks wm
    # Clear all top frames first, or there may be duplicate window objects
    # linked to different parents, since we're generating all windows anew
    # below.
    (each fr (in lo :children)
      (:clear-children fr))

    (def exc-hwnd-map (:load lo dump-data (map |(in $ :hwnd) win-list)))
    (def exc-hwnd-list (values exc-hwnd-map))
    (place-excessive-windows lo exc-hwnd-list win-list)
    (:retile wm lo)
    (when-let [w (:find-hwnd lo focused-hwnd)]
      (:activate w))))


(defn layout-history-undo [self lo]
  (def {:context context
        :manual manual-state}
    self)
  (def {:window-manager window-man
        :uia-manager uia-man}
    context)
  (def lo-id (in lo :id))
  (def lo-state (get-in self [:layout-states lo-id]))
  (unless lo-state
    # Early return
    (break))

  (def unsaved (in lo-state :unsaved-changes))
  (def history (in lo-state :history))

  (def dump
    (cond
      (and unsaved manual-state)
      # manual mode with unsaved changes
      (history-stack-manual-peek history)

      unsaved
      # automatic mode with unsaved changes
      (history-stack-peek history)

      manual-state
      (history-stack-manual-undo history)

      true
      (history-stack-undo history)))
  (when dump
    (put lo-state :unsaved-changes nil)
    (load-layout lo dump window-man uia-man)))


(defn layout-history-redo [self lo]
  (def {:context context
        :manual manual-state}
    self)
  (def {:window-manager window-man
        :uia-manager uia-man}
    context)
  (def lo-id (in lo :id))
  (def lo-state (get-in self [:layout-states lo-id]))
  (unless lo-state
    # Early return
    (break))

  (def unsaved (in lo-state :unsaved-changes))
  (def history (in lo-state :history))

  (def dump
    (cond
      (and unsaved manual-state)
      # manual mode with unsaved changes
      (history-stack-manual-peek history)

      unsaved
      # automatic mode with unsaved changes
      (history-stack-peek history)

      manual-state
      (history-stack-manual-redo history)

      true
      (history-stack-redo history)))
  (when dump
    (put lo-state :unsaved-changes nil)
    (load-layout lo dump window-man uia-man)))


(defn layout-history-set-manual [self manual?]
  (def {:context context
        :hook-fns hook-fns
        :layout-states layout-states
        :manual manual-state}
    self)
  (def {:hook-manager hook-man} context)
  (def hook-fn (in hook-fns :layout-changed))

  (cond
    (and (not manual-state) manual?)
    # automatic -> manual
    (do
      (put self :manual true)
      # Adjust for the off-by-one case
      (each {:history h} layout-states
        (history-stack-manual-undo h)))

    (and manual-state (not manual?))
    # manual -> automatic
    (do
      (put self :manual false)
      # Adjust for the off-by-one case
      (each {:history h} layout-states
        (history-stack-redo h)))))


(defn layout-history-manual? [self]
  (in self :manual))


(defn layout-history-push [self lo]
  (def {:layout-states layout-states
        :manual manual-state
        :limit limit}
    self)

  (def lo-id (in lo :id))
  (def lo-state (in layout-states lo-id (new-layout-state)))
  (put layout-states lo-id lo-state)

  (def now (os/clock :realtime))
  (def dump [now (:dump lo)])
  (if manual-state
    (history-stack-manual-push (in lo-state :history) dump limit)
    # else
    (history-stack-push (in lo-state :history) dump limit))
  (put lo-state :top-frame-count (length (in lo :children)))
  (put lo-state :unsaved-changes nil)
  (:save-to-backing-file self))


(defn layout-history-add-commands [self]
  (def {:context context} self)
  (def {:window-manager window-man
        :command-manager command-man}
    context)

  (:add-command command-man :undo-layout-history
     (fn []
       (:undo self (:get-current-layout (in window-man :root)))))
  (:add-command command-man :redo-layout-history
     (fn []
       (:redo self (:get-current-layout (in window-man :root)))))
  (:add-command command-man :push-layout-history
     (fn []
       (:push self (:get-current-layout (in window-man :root))))))


(defn layout-history-remove-commands [self]
  (def {:context context} self)
  (def {:command-manager command-man} context)

  (:remove-command command-man :undo-layout-history)
  (:remove-command command-man :redo-layout-history)
  (:remove-command command-man :push-layout-history))


(def layout-history-proto
  @{:on-layout-changed layout-history-on-layout-changed
    :enable layout-history-enable
    :disable layout-history-disable
    :undo layout-history-undo
    :redo layout-history-redo
    :push layout-history-push
    :set-manual layout-history-set-manual
    :manual? layout-history-manual?
    :add-commands layout-history-add-commands
    :remove-commands layout-history-remove-commands
    :get-backing-file-path layout-history-get-backing-file-path
    :save-to-backing-file layout-history-save-to-backing-file
    :load-from-backing-file layout-history-load-from-backing-file})


(defn layout-history [context]
  (table/setproto
   @{:context context
     :layout-states @{}
     :hook-fns @{}
     :manual false

     # default settings
     :limit 1024
     :backing-file nil}
   layout-history-proto))
