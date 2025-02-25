#
# scratch-pad.janet
#
# A simple scratch pad (temporary "cache" that holds user-selected windows)
# for Jwno. (WIP)
#

(use jw32/_winuser)
(use jw32/_util)

(import jwno/util)
(import jwno/log)


(defn scratch-pad-get-win-list [self]
  (def {:win-list win-list} self)
  (def new-list (filter |(not= FALSE (IsWindow $)) win-list))
  (put self :win-list new-list)
  new-list)


(defn scratch-pad-show-window-on-current-vd [self hwnd]
  (def {:window-manager wm
        :show-window-timeout timeout
        :show-window-time-incr time-incr
        :show-window-init-wait-time init-wait-time}
    self)

  (var wait-time init-wait-time)

  (:remove-hwnd wm hwnd)

  (def start-time (os/clock :monotonic))
  (var cur-time (os/clock :monotonic))
  (var win-vd-stat (:IsWindowOnCurrentVirtualDesktop (in wm :vdm-com) hwnd))
  (log/debug "---- scratch pad: start-time = %n, cur-time = %n, win-vd-stat = %n"
             start-time cur-time win-vd-stat)
  (while (and (< (- cur-time start-time) timeout)
              (= FALSE win-vd-stat))
    (ShowWindow hwnd SW_HIDE)
    (log/debug "---- scratch pad: waiting for %n seconds...." wait-time)
    (ev/sleep wait-time)
    (ShowWindow hwnd SW_SHOW)
    (+= wait-time time-incr)
    (set cur-time (os/clock :monotonic))
    (set win-vd-stat (:IsWindowOnCurrentVirtualDesktop (in wm :vdm-com) hwnd))
    (log/debug "---- scratch pad: start-time = %n, cur-time = %n, win-vd-stat = %n"
               start-time cur-time win-vd-stat))

  (ShowWindow hwnd SW_SHOW)
  (not= FALSE win-vd-stat))


(defn scratch-pad-add-window [self hwnd]
  (def {:window-manager wm
        :hidden hidden
        :rect rect}
    self)

  (def vd-info (:get-hwnd-virtual-desktop wm hwnd))
  (unless (and vd-info
               (in vd-info :id))
    (errorf "not manageable %n" hwnd))

  (def win-list (:get-win-list self))
  (array/push win-list hwnd)

  (:remove-hwnd wm hwnd)
  (:ignore-hwnd wm hwnd)

  (if hidden
    (:hide self)
    (:show self)))


(defn scratch-pad-remove-window [self hwnd]
  (def {:window-manager wm
        :hidden hidden}
    self)

  # XXX: Make a window-manager method for this
  (put (in wm :ignored-hwnds) hwnd nil)

  (def win-list (:get-win-list self))
  (when-let [idx (find-index |(= $ hwnd) win-list)]
    (array/remove win-list idx))

  (ShowWindow hwnd SW_SHOW)

  (if hidden
    (:hide self)
    (:show self)))


(defn scratch-pad-remove-all-windows [self]
  (def {:window-manager wm} self)

  (def win-list (:get-win-list self))
  (each hwnd win-list
    (put (in wm :ignored-hwnds) hwnd nil)
    (ShowWindow hwnd SW_SHOW))
  (array/clear win-list)

  (put self :hidden true))


(defn scratch-pad-transform [self rect]
  :TODO
  )


(defn scratch-pad-show [self &opt dir]
  (def {:rect rect
        :window-manager wm
        :uia-manager uia-man}
    self)

  (def win-list (:get-win-list self))
  (def new-list
    (case dir
      nil
      win-list

      :next
      (if-let [first-hwnd (first win-list)]
        (do
          (array/remove win-list 0 1)
          (array/push win-list first-hwnd))
        # else, empty list
        win-list)

      :prev
      (if-let [last-hwnd (last win-list)]
        (do
          (array/remove win-list -1 1)
          (array/insert win-list 0 last-hwnd))
        # else, empty list
        win-list)))

  (def cur-hwnd (first new-list))
  (when cur-hwnd
    (each hwnd (slice new-list 1)
      (ShowWindow hwnd SW_HIDE))
    (def sw-res (:show-window-on-current-vd self cur-hwnd))
    (if sw-res
      (do
        (log/debug "---- scratch pad: moving window: %n" cur-hwnd)
        (SetWindowPos cur-hwnd
                      0
                      (in rect :left)
                      (in rect :top)
                      ;(util/rect-size rect)
                      (bor SWP_NOACTIVATE SWP_NOZORDER))
        (:set-focus-to-window uia-man cur-hwnd))
      # else
      (log/debug "---- scratch pad: :show-window-on-current-vd failed"))
    (put self :hidden false)))


(defn scratch-pad-hide [self]
  (each hwnd (:get-win-list self)
    (ShowWindow hwnd SW_HIDE))
  (put self :hidden true))


(def scratch-pad-proto
  @{:add-window scratch-pad-add-window
    :remove-window scratch-pad-remove-window
    :remove-all-windows scratch-pad-remove-all-windows
    :transform scratch-pad-transform
    :show scratch-pad-show
    :hide scratch-pad-hide
    :show-window-on-current-vd scratch-pad-show-window-on-current-vd
    :get-win-list scratch-pad-get-win-list})


(defn scratch-pad [context]
  (def {:window-manager wm
        :uia-manager uia-man}
    context)

  (table/setproto
   @{:window-manager wm
     :uia-manager uia-man
     :win-list @[]
     :hidden true

     # Default settings
     :show-window-timeout 3 # in seconds
     :show-window-time-incr 0.05
     :show-window-init-wait-time 0.05
     :rect {:left 100 :top 100 :right 600 :bottom 600}}
   scratch-pad-proto))
