#
# scratch-pad.janet
#
# A simple scratch pad (temporary "cache" that holds user-selected windows)
# for Jwno. (WIP)
#

(use jw32/_winuser)
(use jw32/_errhandlingapi)
(use jw32/_util)

(import jwno/util)
(import jwno/log)

(use jwno/util)


(def FLAGS-PROP-NAME "_jwno-scratch-pad-flags_")

(defn set-managed-flag [hwnd]
  (def ret (SetProp hwnd FLAGS-PROP-NAME 1))
  (when (= ret FALSE)
    (log/warning "---- scratch pad: SetProp failed for %n: %n"
                 hwnd (GetLastError))))

(defn unset-managed-flag [hwnd]
  # XXX: Should use RemoveProp instead?
  (def ret (SetProp hwnd FLAGS-PROP-NAME 0))
  (when (= ret FALSE)
    (log/warning "---- scratch pad: SetProp failed for %n: %n"
                 hwnd (GetLastError))))

(defn check-managed-flag [hwnd]
  (not= NULL (GetProp hwnd FLAGS-PROP-NAME)))


(defn do-not-ignore [wm hwnd]
  # XXX: Make a window-manager method for this
  (put (in wm :ignored-hwnds) hwnd nil))


(defn trigger-window-opened-event [uia-man hwnd]
  # Tell the main loop it now has a new window to handle
  (ev/give (in uia-man :chan) [:uia/window-opened hwnd]))


(defn find-flagged-windows []
  (def flagged-list @[])
  (EnumChildWindows
   nil
   (fn [hwnd]
     (when (check-managed-flag hwnd)
       (array/push flagged-list hwnd))
     1 # !!! IMPORTANT
     ))
  (log/debug "---- scratch pad: found flagged windows: %n" flagged-list)
  flagged-list)


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
        :rect rect}
    self)

  (with-uia [uia-win (:get-hwnd-uia-element wm hwnd)]
    (def vd-info (:get-hwnd-virtual-desktop wm hwnd uia-win))
    (cond
      (nil? vd-info)
      (errorf "no virtual desktop info for window %n" hwnd)

      (nil? (in vd-info :id))
      (errorf "no virtual desktop id for window %n" hwnd)

      (= "ApplicationFrameWindow" (:get_CachedClassName uia-win))
      (errorf "ApplicationFrameWindow %n not supported" hwnd)))

  (def visible (:visible? self))

  (def win-list (:get-win-list self))
  (array/push win-list hwnd)
  (set-managed-flag hwnd)

  (:remove-hwnd wm hwnd)
  (:ignore-hwnd wm hwnd)

  (if visible
    (:show self)
    (:hide self)))


(defn scratch-pad-remove-window [self hwnd]
  (def {:window-manager wm
        :uia-manager uia-man}
    self)

  (do-not-ignore wm hwnd)

  (def visible (:visible? self))

  (def win-list (:get-win-list self))
  (when-let [idx (find-index |(= $ hwnd) win-list)]
    (array/remove win-list idx))

  (ShowWindow hwnd SW_SHOW)
  (unset-managed-flag hwnd)
  (trigger-window-opened-event uia-man hwnd)

  (if visible
    (:show self)
    (:hide self)))


(defn scratch-pad-remove-all-windows [self]
  (def {:window-manager wm
        :uia-manager uia-man}
    self)

  (def win-list (:get-win-list self))
  (each hwnd win-list
    (do-not-ignore wm hwnd)
    (def win-visible (not= FALSE (IsWindowVisible hwnd)))
    (if win-visible
      (do
        (log/debug "---- scratch pad: trigger-window-opened-event for %n" hwnd)
        (trigger-window-opened-event uia-man hwnd))
      # else
      (ShowWindow hwnd SW_SHOW))
    (unset-managed-flag hwnd))
  (array/clear win-list))


(defn scratch-pad-visible? [self]
  (def {:window-manager wm} self)

  (def win-list (:get-win-list self))
  (if-let [cur-hwnd (first win-list)]
    (with-uia [uia-win (:get-hwnd-uia-element wm cur-hwnd)]
      (def [_point gcp-ret] (:GetClickablePoint uia-win))
      (and (not= FALSE gcp-ret)
           (= FALSE (:get_CurrentIsOffscreen uia-win))
           (not= FALSE (IsWindowVisible cur-hwnd))
           (not= FALSE (:IsWindowOnCurrentVirtualDesktop (in wm :vdm-com) cur-hwnd))))
    # else, there's no window in the list
    false))


(defn scratch-pad-transform [self rect]
  :TODO
  )


(defn rotate-win-list [win-list dir]
  (case dir
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
      win-list)

    (errorf "unknown direction: %n" dir)))


(defn scratch-pad-show [self &opt dir]
  (def {:rect rect
        :window-manager wm
        :uia-manager uia-man
        :always-on-top always-on-top}
    self)

  (def visible (:visible? self))

  (def win-list (:get-win-list self))
  (def new-list
    (cond
      dir
      (rotate-win-list win-list dir)

      visible
      # Switch to the next window when we're already visible
      (rotate-win-list win-list :next)

      true
      win-list))

  (def cur-hwnd (first new-list))
  (when cur-hwnd
    (each hwnd (slice new-list 1)
      (ShowWindow hwnd SW_HIDE))
    (def sw-res (:show-window-on-current-vd self cur-hwnd))
    (if sw-res
      (do
        (log/debug "---- scratch pad: moving window: %n" cur-hwnd)
        (def z-hwnd
          (if always-on-top
            HWND_TOPMOST
            HWND_NOTOPMOST))
        (SetWindowPos cur-hwnd
                      z-hwnd
                      (in rect :left)
                      (in rect :top)
                      ;(util/rect-size rect)
                      SWP_NOACTIVATE)
        (:set-focus-to-window uia-man cur-hwnd))
      # else
      (log/debug "---- scratch pad: :show-window-on-current-vd failed"))))


(defn scratch-pad-hide [self]
  (each hwnd (:get-win-list self)
    (ShowWindow hwnd SW_HIDE)))


(defn scratch-pad-enable [self]
  (:disable self)

  (def {:command-manager command-man
        :window-manager wm}
    self)

  # Check for windows left in hidden state last time we exited
  (def flagged-list (find-flagged-windows))
  (def win-list (:get-win-list self))
  (each hwnd flagged-list
    (:remove-hwnd wm hwnd)
    (:ignore-hwnd wm hwnd)
    (array/push win-list hwnd))
  # XXX: default to hide all windows
  (:hide self)

  (def get-focused-hwnd
    (fn []
      (with-uia [uia-win (:get-focused-window (in self :uia-manager))]
        (when uia-win
          (:get_CachedNativeWindowHandle uia-win)))))

  (:add-command command-man :add-to-scratch-pad
     (fn []
       (when-let [hwnd (get-focused-hwnd)]
         (:add-window self hwnd))))

  (:add-command command-man :remove-from-scratch-pad
     (fn []
       (when-let [hwnd (get-focused-hwnd)]
         (:remove-window self hwnd))))

  (:add-command command-man :show-scratch-pad
     (fn [&opt dir]
       (:show self dir)))

  (:add-command command-man :hide-scratch-pad
     (fn []
       (:hide self)))

  (:add-command command-man :toggle-scratch-pad
     (fn []
       (if (:visible? self)
         (:hide self)
         (:show self))))

  (:add-command command-man :remove-all-from-scratch-pad
     (fn []
       (:remove-all-windows self))))


(defn scratch-pad-disable [self]
  (def {:command-manager command-man} self)

  (:remove-command command-man :add-to-scratch-pad)
  (:remove-command command-man :remove-from-scratch-pad)
  (:remove-command command-man :show-scratch-pad)
  (:remove-command command-man :hide-scratch-pad)
  (:remove-command command-man :toggle-scratch-pad)
  (:remove-command command-man :remove-all-windows-from-scratch-pad)

  (:remove-all-windows self))


(def scratch-pad-proto
  @{:add-window scratch-pad-add-window
    :remove-window scratch-pad-remove-window
    :remove-all-windows scratch-pad-remove-all-windows
    :transform scratch-pad-transform
    :show scratch-pad-show
    :hide scratch-pad-hide
    :show-window-on-current-vd scratch-pad-show-window-on-current-vd
    :get-win-list scratch-pad-get-win-list
    :visible? scratch-pad-visible?
    :enable scratch-pad-enable
    :disable scratch-pad-disable})


(defn scratch-pad [context]
  (def {:window-manager wm
        :uia-manager uia-man
        :command-manager command-man}
    context)

  (table/setproto
   @{:window-manager wm
     :uia-manager uia-man
     :command-manager command-man
     :win-list @[]

     # Default settings
     :always-on-top true
     :rect {:left 100 :top 100 :right 600 :bottom 600}
     :show-window-timeout 3 # in seconds
     :show-window-time-incr 0.05
     :show-window-init-wait-time 0.05
     }
   scratch-pad-proto))
