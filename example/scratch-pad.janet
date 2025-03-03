#
# scratch-pad.janet
#
# A simple scratch pad (temporary "cache" that holds user-selected windows)
# for Jwno. (WIP)
#

(use jw32/_winuser)
(use jw32/_dwmapi)
(use jw32/_errhandlingapi)
(use jw32/_util)

(import jwno/util)
(import jwno/log)

(use jwno/util)


(def FLAGS-PROP-NAME "_jwno-scratch-pad-flags_")

(defn get-flags-prop-name [sp-name]
  (string FLAGS-PROP-NAME sp-name))

(defn set-managed-flag [hwnd sp-name]
  (def ret (SetProp hwnd (get-flags-prop-name sp-name) 1))
  (when (= ret FALSE)
    (log/warning "---- scratch pad: SetProp failed for %n: %n"
                 hwnd (GetLastError))))

(defn unset-managed-flag [hwnd sp-name]
  # XXX: Should use RemoveProp instead?
  (def ret (SetProp hwnd (get-flags-prop-name sp-name) 0))
  (when (= ret FALSE)
    (log/warning "---- scratch pad: SetProp failed for %n: %n"
                 hwnd (GetLastError))))

(defn check-managed-flag [hwnd sp-name]
  (not= NULL (GetProp hwnd (get-flags-prop-name sp-name))))


(defn do-not-ignore [wm hwnd]
  # XXX: Make a window-manager method for this
  (put (in wm :ignored-hwnds) hwnd nil))


(defn reset-topmost-window [hwnd &opt extra-flags]
  (default extra-flags 0)

  (def fg-hwnd (GetForegroundWindow))
  # If hwnd is not the active window, put it to the bottom
  # so that it won't obscure the actual active window
  (def z-hwnd
    (if (= hwnd fg-hwnd)
      HWND_NOTOPMOST
      HWND_BOTTOM))
  (def flags
    (bor SWP_NOACTIVATE SWP_NOMOVE SWP_NOSIZE extra-flags))
  (SetWindowPos hwnd z-hwnd 0 0 0 0 flags))


(defn trigger-window-opened-event [uia-man hwnd]
  # Tell the main loop it now has a new window to handle
  (ev/give (in uia-man :chan) [:uia/window-opened hwnd]))


(defn find-flagged-windows [sp-name]
  (def flagged-list @[])
  (EnumChildWindows
   nil
   (fn [hwnd]
     (when (check-managed-flag hwnd sp-name)
       (array/push flagged-list hwnd))
     1 # !!! IMPORTANT
     ))
  (log/debug "---- scratch pad: found flagged windows: %n" flagged-list)
  flagged-list)


(defn snap-to-window [scratch-pad hwnd]
  (def efb-rect (DwmGetWindowAttribute hwnd DWMWA_EXTENDED_FRAME_BOUNDS))
  (put scratch-pad :rect efb-rect))


(defn update-auto-transform [scratch-pad]
  (unless (in scratch-pad :auto-transform)
    (break))

  (when-let [cur-hwnd (:get-current-hwnd scratch-pad)]
    (when (= FALSE (IsWindowVisible cur-hwnd))
      (break))
    (snap-to-window scratch-pad cur-hwnd)))


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


(defn scratch-pad-do-default-filter [self hwnd uia-win exe-path vd-info]
  (def {:ui-manager ui-man} self)
  (def win-list (:get-win-list self))

  (cond
    (find |(= $ hwnd) win-list)
    (do
      (:show-tooltip ui-man :scratch-pad
         (string/format "Scratch Pad: Window already in scratch pad: %n"
                        (:get_CachedName uia-win)))
      false)

    (nil? vd-info)
    (do
      (:show-tooltip ui-man :scratch-pad
         (string/format "Scratch Pad: Failed to get virtual desktop info for window %n"
                        (:get_CachedName uia-win)))
      false)

    (nil? (in vd-info :id))
    (do
      (:show-tooltip ui-man :scratch-pad
         (string/format "Scratch Pad: Failed to get virtual desktop ID for window %n"
                        (:get_CachedName uia-win)))
      false)

    (= "ApplicationFrameWindow" (:get_CachedClassName uia-win))
    (do
      (:show-tooltip ui-man :scratch-pad
         "Scratch Pad: ApplicationFrameWindow class is not supported")
      false)

    true))


(defn scratch-pad-add-window [self hwnd]
  (def {:name name
        :window-manager wm
        :hook-manager hook-man}
    self)

  (update-auto-transform self)
  (def rect (in self :rect))

  (def should-manage
    (with-uia [uia-win (:get-hwnd-uia-element wm hwnd)]
      (def vd-info (:get-hwnd-virtual-desktop wm hwnd uia-win))
      (def exe-path (:get-hwnd-path wm hwnd))
      (:call-filter-hook hook-man :and :filter-scratch-pad-window
         hwnd uia-win exe-path vd-info)))

  (unless should-manage
    (break))

  (def visible (:visible? self))

  (def win-list (:get-win-list self))
  (array/insert win-list 0 hwnd)
  (set-managed-flag hwnd name)

  (:remove-hwnd wm hwnd)
  (:ignore-hwnd wm hwnd)

  # Transform the window here, before update-auto-transform is called
  # in (:hide self), so that the window snaps to scratch pad, instead
  # of scratch pad snapping to the newly added window
  (log/debug "---- scratch pad: moving window: %n" hwnd)
  (:transform-hwnd wm hwnd rect)

  (if visible
    (:show self)
    (:hide self)))


(defn scratch-pad-remove-window [self hwnd]
  (def {:name name
        :window-manager wm
        :uia-manager uia-man}
    self)

  (update-auto-transform self)

  (do-not-ignore wm hwnd)

  (def visible (:visible? self))

  (def win-list (:get-win-list self))
  (def removed
    (when-let [idx (find-index |(= $ hwnd) win-list)]
      (array/remove win-list idx)
      true))

  (unless removed
    (break))

  (reset-topmost-window hwnd SWP_SHOWWINDOW)
  (unset-managed-flag hwnd name)
  (trigger-window-opened-event uia-man hwnd)

  (when visible
    (:show self))
  (:show-window-on-current-vd self hwnd)
  (:set-focus-to-window uia-man hwnd))


(defn scratch-pad-remove-all-windows [self]
  (def {:name name
        :window-manager wm
        :uia-manager uia-man}
    self)

  (update-auto-transform self)

  (def fg-hwnd (GetForegroundWindow))
  (def win-list (:get-win-list self))
  (def cur-hwnd (first win-list))

  (each hwnd win-list
    (do-not-ignore wm hwnd)
    (def win-visible (not= FALSE (IsWindowVisible hwnd)))
    (if win-visible
      (do
        (reset-topmost-window hwnd)
        (log/debug "---- scratch pad: trigger-window-opened-event for %n" hwnd)
        (trigger-window-opened-event uia-man hwnd))
      # else
      (reset-topmost-window hwnd SWP_SHOWWINDOW))
    (unset-managed-flag hwnd name))
  (array/clear win-list)

  (when cur-hwnd
    (:show-window-on-current-vd self cur-hwnd)
    (:set-focus-to-window uia-man cur-hwnd)))


(defn scratch-pad-visible? [self]
  (def {:window-manager wm} self)

  (if-let [cur-hwnd (:get-current-hwnd self)]
    (with-uia [uia-win (:get-hwnd-uia-element wm cur-hwnd)]
      (def [_point gcp-ret] (:GetClickablePoint uia-win))
      (and (not= FALSE gcp-ret)
           (= FALSE (:get_CurrentIsOffscreen uia-win))
           (not= FALSE (IsWindowVisible cur-hwnd))
           (not= FALSE (:IsWindowOnCurrentVirtualDesktop (in wm :vdm-com) cur-hwnd))))
    # else, there's no window in the list
    false))


(defn scratch-pad-transform [self rect]
  (def {:rect orig-rect
        :auto-transform orig-auto-transform}
    self)

  (defer
    (put self :auto-transform orig-auto-transform)

    (put self :auto-transform false)
    (put self :rect rect)
    (try
      (:show self)
      ((err _fib)
       # XXX: Assuming the rect is invalid, reset it
       (put self :rect orig-rect)
       (error err)))))


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
  (def {:window-manager wm
        :uia-manager uia-man
        :always-on-top always-on-top
        :auto-transform auto-transform}
    self)

  (update-auto-transform self)
  (def rect (in self :rect))

  (def win-list (:get-win-list self))
  (def new-list
    (cond
      dir
      (rotate-win-list win-list dir)

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
        (:transform-hwnd wm cur-hwnd rect)
        (def z-hwnd
          (if always-on-top
            HWND_TOPMOST
            HWND_NOTOPMOST))
        (SetWindowPos cur-hwnd
                      z-hwnd
                      0 0 0 0
                      (bor SWP_NOACTIVATE SWP_NOSIZE SWP_NOMOVE))
        (:set-focus-to-window uia-man cur-hwnd))
      # else
      (log/debug "---- scratch pad: :show-window-on-current-vd failed"))))


(defn scratch-pad-hide [self]
  (update-auto-transform self)
  (each hwnd (:get-win-list self)
    (ShowWindow hwnd SW_HIDE))

  (def {:window-manager wm
        :uia-manager uia-man}
    self)

  (when-let [cur-win (:get-current-window (in wm :root))]
    # XXX: Make a window method for this?
    (def vd-stat
      (:IsWindowOnCurrentVirtualDesktop
         (in wm :vdm-com)
         (in cur-win :hwnd)))
    (if (not= FALSE vd-stat)
      (:set-focus cur-win wm)
      # Set focus the desktop instead. XXX: Make a uia-manager method for this?
      (:SetFocus (in uia-man :def-view)))))


(defn scratch-pad-get-current-hwnd [self]
  (first (:get-win-list self)))


(defn scratch-pad-add-commands [self]
  (def {:uia-manager uia-man
        :command-manager command-man}
    self)

  (def get-focused-hwnd
    (fn []
      (with-uia [uia-win (:get-focused-window uia-man)]
        (when uia-win
          (:get_CachedNativeWindowHandle uia-win)))))

  (:add-command command-man (:command-name self :add-to)
                (fn []
                  (when-let [hwnd (get-focused-hwnd)]
                    (:add-window self hwnd))))

  (:add-command command-man (:command-name self :remove-from)
                (fn []
                  (when-let [cur-hwnd (:get-current-hwnd self)]
                    (:remove-window self cur-hwnd))))

  (:add-command command-man (:command-name self :show)
                (fn [&opt dir]
                  (cond
                    dir
                    (:show self dir)

                    (:visible? self)
                    # Switch to the next window if scratch pad is already shown
                    (:show self :next)

                    true
                    (:show self))))

  (:add-command command-man (:command-name self :hide)
                (fn []
                  (:hide self)))

  (:add-command command-man (:command-name self :toggle)
                (fn []
                  (if (:visible? self)
                    (:hide self)
                    (:show self))))

  (:add-command command-man (:command-name self :remove-all-from)
                (fn []
                  (:remove-all-windows self))))


(defn scratch-pad-remove-commands [self]
  (def {:command-manager command-man} self)

  (:remove-command command-man (:command-name self :add-to))
  (:remove-command command-man (:command-name self :remove-from))
  (:remove-command command-man (:command-name self :show))
  (:remove-command command-man (:command-name self :hide))
  (:remove-command command-man (:command-name self :toggle))
  (:remove-command command-man (:command-name self :remove-all-from)))


(defn scratch-pad-enable [self &opt add-commands]
  (default add-commands true)

  (:disable self)

  (def {:name name
        :hook-manager hook-man
        :window-manager wm}
    self)

  # Check for windows left in hidden state last time we exited
  (def flagged-list (find-flagged-windows name))
  (def win-list (:get-win-list self))
  (each hwnd flagged-list
    (:remove-hwnd wm hwnd)
    (:ignore-hwnd wm hwnd)
    (array/push win-list hwnd))
  # XXX: default to hide all windows
  (:hide self)

  (put self :default-filter-hook-fn
     (:add-hook hook-man :filter-scratch-pad-window
        (fn [& args]
          (:do-default-filter self ;args))))

  (when add-commands
    (:add-commands self)))


(defn scratch-pad-disable [self]
  (def {:hook-manager hook-man
        :default-filter-hook-fn default-filter-hook-fn}
    self)

  (when default-filter-hook-fn
    (:remove-hook hook-man
                  :filter-scratch-pad-window
                  default-filter-hook-fn)
    (put self :default-filter-hook-fn nil))

  (:remove-commands self)

  (:remove-all-windows self))


(defn scratch-pad-command-name [self cmd]
  (keyword cmd "-" (in self :name)))


(def scratch-pad-proto
  @{:add-window scratch-pad-add-window
    :remove-window scratch-pad-remove-window
    :remove-all-windows scratch-pad-remove-all-windows
    :transform scratch-pad-transform
    :show scratch-pad-show
    :hide scratch-pad-hide
    :show-window-on-current-vd scratch-pad-show-window-on-current-vd
    :do-default-filter scratch-pad-do-default-filter
    :get-win-list scratch-pad-get-win-list
    :visible? scratch-pad-visible?
    :get-current-hwnd scratch-pad-get-current-hwnd
    :enable scratch-pad-enable
    :disable scratch-pad-disable
    :command-name scratch-pad-command-name
    :add-commands scratch-pad-add-commands
    :remove-commands scratch-pad-remove-commands})


(defn scratch-pad [context]
  (def {:window-manager wm
        :ui-manager ui-man
        :hook-manager hook-man
        :uia-manager uia-man
        :command-manager command-man}
    context)

  (table/setproto
   @{:window-manager wm
     :ui-manager ui-man
     :hook-manager hook-man
     :uia-manager uia-man
     :command-manager command-man
     :win-list @[]
     # initialized in scratch-pad-enable
     :default-filter-hook-fn nil

     # Default settings
     :name :scratch-pad
     :always-on-top true
     :auto-transform true
     :rect {:left 100 :top 100 :right 600 :bottom 600}
     :show-window-timeout 3 # in seconds
     :show-window-time-incr 0.1
     :show-window-init-wait-time 0.1
     }
   scratch-pad-proto))
