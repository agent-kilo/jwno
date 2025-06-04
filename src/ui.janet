(use jw32/_winuser)
(use jw32/_shellapi)
(use jw32/_commctrl)
(use jw32/_errhandlingapi)
(use jw32/_processthreadsapi)
(use jw32/_util)

(use ./key)
(use ./input)
(use ./resource)
(use ./util)

(import ./const)
(import ./log)

(def GC-TIMER-INTERVAL 5000)    # in milliseconds

(def NOTIFY-ICON-ID 1)

(def NOTIFY-ICON-CALLBACK-MSG    (+ WM_APP 0x01))
(def SET-KEYMAP-MSG              (+ WM_APP 0x02))
(def SET-HOOKS-MSG               (+ WM_APP 0x03))
(def REMOVE-HOOKS-MSG            (+ WM_APP 0x04))
(def SHOW-ERROR-AND-EXIT-MSG     (+ WM_APP 0x05))
(def SHOW-TOOLTIP-MSG            (+ WM_APP 0x06))
(def HIDE-TOOLTIP-MSG            (+ WM_APP 0x07))
(def SET-TOOLTIP-TIMEOUTS-MSG    (+ WM_APP 0x08))
(def SET-TOOLTIP-ANCHORS-MSG     (+ WM_APP 0x09))
(def SET-TOOLTIP-MAX-WIDTHS-MSG  (+ WM_APP 0x0A))
(def UPDATE-WORK-AREA-MSG        (+ WM_APP 0x0B))
(def ADD-CUSTOM-MSG-MSG          (+ WM_APP 0x0C))
(def REMOVE-CUSTOM-MSG-MSG       (+ WM_APP 0x0D))
(def SET-KEY-MODE-MSG            (+ WM_APP 0x0E))
(def SHOW-TOOLTIP-GROUP-MSG      (+ WM_APP 0x0F))
(def HIDE-TOOLTIP-GROUP-MSG      (+ WM_APP 0x10))

# Where custom messages begin
(def CUSTOM-MSG (+ WM_APP 0x400))

(def TIMER-ID-DISPLAY-CHANGE (int/u64 1))
# The tooltip timers are generated from tooltip numeric IDs.
# The tooltip numeric IDs are from 0x0001 to 0xffff, and the
# timers are from 0x10001 to 0x1ffff.
(def TOOLTIP-TIMER-BASE (int/u64 0x10000))
(def TOOLTIP-TIMER-BASE-MASK (int/u64 0xffff0000))


(defn show-version-info []
  (MessageBox nil
              (string/format
               ```
               Jwno version: %d.%d.%d%s
               Janet version: %s-%s
               ```
               VERSION_MAJOR VERSION_MINOR VERSION_PATCH
               (if VERSION_VCS
                 (string "-" VERSION_VCS)
                 "")
               janet/version
               janet/build)
              "Jwno Version"
              (bor MB_ICONINFORMATION MB_OK)))


(defn- msg-loop [chan gc-timer-id hook-handler]
  (def msg (MSG))

  (forever
   (case (GetMessage msg nil 0 0)
     0
     (break)

     -1
     (show-error-and-exit "GetMessage() failed" 1)

     (do
       (var skip false)
       (case (msg :message)
         WM_TIMER
         (case (msg :wParam)
           gc-timer-id
           # This timer is used to break out from GetMessage, so that
           # the garbage collector has a chance to run.
           (do
             (log/debug "GC timer triggered")
             (set skip true))))
       (unless skip
         (TranslateMessage msg)
         (DispatchMessage msg))))))


(defn- create-notify-icon-menu []
  (def hMenu (CreatePopupMenu))
  (AppendMenu hMenu
              MF_STRING ID_MENU_VERSION
              (string/format "Jwno %d.%d.%d"
                             VERSION_MAJOR
                             VERSION_MINOR
                             VERSION_PATCH))
  (AppendMenu hMenu MF_SEPARATOR 0 0)
  (AppendMenu hMenu MF_STRING ID_MENU_LAUNCH_REPL "Launch &REPL")
  (AppendMenu hMenu MF_STRING ID_MENU_UPDATE_MONITOR_LAYOUT "Update &Monitor Layout")
  (AppendMenu hMenu MF_STRING ID_MENU_RESET_KBD_HOOKS "Reset &Keyboard Hooks")
  (AppendMenu hMenu MF_SEPARATOR 0 0)
  (AppendMenu hMenu MF_STRING ID_MENU_EXIT "E&xit")
  hMenu)


(defn- show-notify-icon-menu [hwnd x y]
  (def hMenu (create-notify-icon-menu))
  (log/debug ".... hMenu = %p" hMenu)
  (SetForegroundWindow hwnd)
  (var uFlags TPM_RIGHTBUTTON)
  (if (not= (GetSystemMetrics SM_MENUDROPALIGNMENT) 0)
    (set uFlags (bor uFlags TPM_RIGHTALIGN))
    (set uFlags (bor uFlags TPM_LEFTALIGN)))
  (def tpm-ret
    (TrackPopupMenuEx hMenu uFlags x y hwnd nil))
  (log/debug ".... tpm-ret = %p" tpm-ret)
  (DestroyMenu hMenu))


(defn- create-notify-icon [hinst hwnd argv0]
  (def hIcon
    (try
      (LoadIconMetric hinst IDI_LOGO LIM_SMALL)
      ((_err _fib)
       # Are we running from the source tree?
       (if (or (string/has-suffix? "/main.janet" argv0)
               (string/has-suffix? "\\main.janet" argv0))
         (let [ico-path (string
                         (string/slice argv0 0 (+ 1 (- (length argv0) (length "/main.janet"))))
                         "../res/jwno.ico")]
           (LoadImage nil ico-path IMAGE_ICON 0 0 (bor LR_DEFAULTSIZE LR_LOADFROMFILE)))
         (LoadIcon nil IDI_QUESTION)))))
  (def nid
    (NOTIFYICONDATA
     :hWnd hwnd
     :uID NOTIFY-ICON-ID
     :uFlags (bor NIF_MESSAGE NIF_ICON NIF_TIP NIF_SHOWTIP)
     :uCallbackMessage NOTIFY-ICON-CALLBACK-MSG
     :hIcon hIcon
     :szTip "Jwno"
     :uVersion NOTIFYICON_VERSION_4))
  (unless (= TRUE (Shell_NotifyIcon NIM_ADD nid))
    (error "Failed to create notify icon"))
  (unless (= TRUE (Shell_NotifyIcon NIM_SETVERSION nid))
    (error "Failed to set notify icon version")))


(defn- remove-notify-icon [hwnd]
  (def nid
    (NOTIFYICONDATA
     :hWnd hwnd
     :uID NOTIFY-ICON-ID))
  (unless (= TRUE (Shell_NotifyIcon NIM_DELETE nid))
    (error "Failed to delete notify icon")))


(defn- log-key-event [code wparam hook-struct]
  (log/debug "################## log-key-event ##################")
  (log/debug "code = %n" code)
  (log/debug "wparam = %n" wparam)
  (log/debug "vkCode = %n" (hook-struct :vkCode))
  (log/debug "scanCode = %n" (hook-struct :scanCode))
  (log/debug "flags.extended = %n" (hook-struct :flags.extended))
  (log/debug "flags.lower_il_injected = %n" (hook-struct :flags.lower_il_injected))
  (log/debug "flags.injected = %n" (hook-struct :flags.injected))
  (log/debug "flags.altdown = %n" (hook-struct :flags.altdown))
  (log/debug "flags.up = %n" (hook-struct :flags.up))
  (log/debug "time = %n" (hook-struct :time))
  (log/debug "dwExtraInfo = %n" (hook-struct :dwExtraInfo)))


(defn- suppress-modifier-key-action [mod-states key-up? extra-info]
  #
  # Some applications interpret modifier key-down events without a
  # trigger key specially, e.g.:
  #
  # 1. The Start menu pops up when only the `Win` key is pressed;
  # 2. The input focus for a window moves to the menu bar when only
  #    the `Alt` key is pressed.
  #
  # This code is to send a dummy trigger key, when the actual trigger
  # key gets intercepted by us, so that applications don't see the
  # modifier keys as pressed alone.
  #
  (unless (empty? mod-states)
    # Send a dummy key event to stop the Start Menu from popping up
    (send-input (keyboard-input VK_DUMMY
                                (if key-up? :up :down)
                                (bor KEI-FLAG-PASSTHROUGH extra-info)))))


# msg-wndproc and this keyboard hook both have access to the
# same keyboard-hook-handler, but since hook events and window
# messages are serialized on the same UI thread, there won't be
# any race conditions.
(defn- keyboard-hook-proc [code wparam hook-struct handler]
  (log-key-event code wparam hook-struct)

  (when (< code 0)
    (break (CallNextHookEx nil code wparam (hook-struct :address))))

  (def key-up (hook-struct :flags.up))
  (def extra-info (hook-struct :dwExtraInfo))

  (when (test-kei-flag KEI-FLAG-SUPPRESS extra-info)
    # The event is sent to alter lower level key states (XXX: not implemented yet)
    (log/debug "Suppressing key")
    (break 1))

  (when (test-kei-flag KEI-FLAG-PASSTHROUGH extra-info)
    (log/debug "Passing through key")
    (break (CallNextHookEx nil code wparam (hook-struct :address))))

  (when-let [new-key (:translate-key handler hook-struct)]
    (send-input (keyboard-input new-key
                                (if key-up :up :down)
                                (bor KEI-FLAG-REMAPPED extra-info)))
    (break 1))

  (def ev-chan (in handler :chan))
  (def mod-states (:get-modifier-states handler hook-struct))

  (def key-mode (:check-key-mode handler hook-struct))
  (when (= :raw key-mode)
    (def [pass-through? msg]
      (:handle-raw-key handler hook-struct mod-states))
    (when msg
      (ev/give ev-chan msg))
    (if pass-through?
      (break (CallNextHookEx nil code wparam (hook-struct :address)))
      (do
        (suppress-modifier-key-action mod-states key-up extra-info)
        (break 1))))

  (if-let [binding (:find-binding handler hook-struct mod-states)]
    (do
      (suppress-modifier-key-action mod-states key-up extra-info)
      (when-let [msg (:handle-binding handler hook-struct binding)]
        (ev/give ev-chan msg))
      1) # !!! IMPORTANT

    (do
      (when-let [msg (:handle-unbound handler hook-struct)]
        (ev/give ev-chan msg))
      (CallNextHookEx nil code wparam (hook-struct :address)))))


(defn- tooltip-uid-generator [start]
  (var last-id start)
  (fiber/new
   (fn []
     (while (< last-id 0xffff)
       (++ last-id)
       (yield last-id))
     (error "tooltip uid overflow"))))


(defn- tooltip-uid-to-timer-id [uid]
  (+ TOOLTIP-TIMER-BASE uid))


(defn- create-tooltip-window [parent-hwnd tt-id &opt center? text max-width]
  (default center? false)
  (default text "")
  (default max-width const/DEFAULT-TOOLTIP-MAX-WIDTH)

  (def tt-hwnd
    (CreateWindowEx WS_EX_TOPMOST
                    TOOLTIPS_CLASS
                    nil
                    (bor WS_POPUP TTS_NOPREFIX TTS_ALWAYSTIP)
                    CW_USEDEFAULT CW_USEDEFAULT CW_USEDEFAULT CW_USEDEFAULT
                    parent-hwnd
                    nil nil nil))

  (when (null? tt-hwnd)
    (log/warning "Failed to create tooltip window: %n" (GetLastError))
    (break nil))

  (def t-info (TTTOOLINFO :uFlags (bor TTF_ABSOLUTE
                                       TTF_TRACK
                                       (if center?
                                         TTF_CENTERTIP
                                         0))
                          :hwnd parent-hwnd
                          :hinst nil
                          :uId tt-id
                          :lpszText (buffer text "\0")))

  (def ret
    (SendMessage tt-hwnd TTM_ADDTOOL 0 (in t-info :address)))
  (when (= ret 0)
    (log/warning "TTM_ADDTOOL failed")
    (DestroyWindow tt-hwnd)
    (break nil))

  # For multi-line tooltip text.
  (SendMessage tt-hwnd TTM_SETMAXTIPWIDTH 0 max-width)
  [tt-hwnd t-info])


(defn- msg-wnd-handle-set-keymap [_hwnd _msg wparam _lparam hook-handler _state]
  (:set-keymap hook-handler wparam)
  0 # !!! IMPORTANT
  )


(defn- msg-wnd-handle-set-key-mode [_hwnd _msg wparam _lparam hook-handler _state]
  (let [new-mode (unmarshal-and-free wparam)]
    (:set-key-mode hook-handler new-mode))
  0 # !!! IMPORTANT
  )


(defn- msg-wnd-handle-set-hooks [_hwnd _msg _wparam _lparam hook-handler _state]
  (let [old-hook (in hook-handler :hook-id)]
    (when (and (not (nil? old-hook))
               (not (null? old-hook)))
      (log/debug "Removing old hook: %n" old-hook)
      (UnhookWindowsHookEx old-hook))
    (def new-hook
      (SetWindowsHookEx WH_KEYBOARD_LL
                        (fn [code wparam hook-struct]
                          (keyboard-hook-proc code
                                              wparam
                                              hook-struct
                                              hook-handler))
                        nil
                        0))
    (when (null? new-hook)
      (show-error-and-exit (string/format "Failed to enable windows hook: 0x%x" (GetLastError)) 1))
    (log/debug "Registered new hook: %n" new-hook)
    (put hook-handler :hook-id new-hook))
  0)


(defn- msg-wnd-handle-remove-hooks [_hwnd _msg _wparam _lparam hook-handler _state]
  (when-let [old-hook (in hook-handler :hook-id)]
    (log/debug "Removing old hook: %n" old-hook)
    (UnhookWindowsHookEx old-hook)
    (put hook-handler :hook-id nil))
  0)


(defn- get-desktop-rect [ui-state]
  (if-let [rect (in ui-state :desktop-rect)]
    rect
    (let [desktop-rect @{:top 0 :left 0 :bottom 0 :right 0}]
      (def mon-info (MONITORINFOEX))
      (def enum-ret
        (EnumDisplayMonitors
         nil nil
         (fn [hmon _hmdc rect]
           (each [cmp edge] [[< :top] [< :left] [> :bottom] [> :right]]
             (when (cmp (in rect edge) (in desktop-rect edge))
               (put desktop-rect edge (in rect edge))))
           TRUE)))
      (if (= FALSE enum-ret)
        (do
          (log/error "EnumDisplayMonitors failed")
          nil)
        (let [ret-rect (table/to-struct desktop-rect)]
          (log/debug "New desktop rect: %n" ret-rect)
          (put ui-state :desktop-rect ret-rect)
          ret-rect)))))


(defn- get-current-work-area [ui-state]
  (def fwin (GetForegroundWindow))
  (def [ret rect] (GetWindowRect fwin))
  (def desktop-rect (get-desktop-rect ui-state))

  (def h-mon
    (cond
      (null? fwin)
      # No focused window, try to fallback to the cached work area
      (if-let [last-wa (in ui-state :last-active-work-area)]
        (MonitorFromRect last-wa MONITOR_DEFAULTTOPRIMARY)
        (MonitorFromPoint [0 0] MONITOR_DEFAULTTOPRIMARY))

      (= FALSE ret)
      # No rect for the window, default to primary monitor
      (MonitorFromPoint [0 0] MONITOR_DEFAULTTOPRIMARY)

      (nil? desktop-rect)
      # Failed to get the bounding rect for the whole desktop
      (MonitorFromPoint [0 0] MONITOR_DEFAULTTOPRIMARY)

      (= rect desktop-rect)
      # The foreground window occupies the whole desktop, assume it's the desktop window.
      # And since MonitorFromWindow(desktop_hwnd, ...) always returns the primary monitor,
      # use the cached work area instead.
      (if-let [last-wa (in ui-state :last-active-work-area)]
        (MonitorFromRect last-wa MONITOR_DEFAULTTOPRIMARY)
        (MonitorFromPoint [0 0] MONITOR_DEFAULTTOPRIMARY))

      true
      (MonitorFromWindow fwin MONITOR_DEFAULTTOPRIMARY)))

  (def mon-info (MONITORINFOEX))
  (def ret (GetMonitorInfo h-mon mon-info))
  (if (= FALSE ret)
    (do
      (log/error "GetMonitorInfo failed for monitor %n" h-mon)
      nil)
    (let [cur-wa (in mon-info :rcWork)]
      (put ui-state :last-active-work-area cur-wa)
      cur-wa)))


(defn- adjust-tooltip-position [tt-hwnd anchor x y]
  (def [ret rect] (GetWindowRect tt-hwnd))
  (when (= FALSE ret)
    (log/error "GetWindowRect failed for tooltip window %n" tt-hwnd)
    (break))

  (def [tt-width tt-height] (rect-size rect))

  (def [adjusted-x adjusted-y]
    (cond
      (or (= :top-left anchor)
          (= :left-top anchor))
      [x y]

      (or (= :top-right anchor)
          (= :right-top anchor))
      [(- x tt-width) y]

      (or (= :bottom-left anchor)
          (= :left-bottom anchor))
      [x (- y tt-height)]

      (or (= :bottom-right anchor)
          (= :right-bottom anchor))
      [(- x tt-width) (- y tt-height)]

      (= :top anchor)
      [(- x (brshift tt-width 1)) y]

      (= :bottom anchor)
      [(- x (brshift tt-width 1)) (- y tt-height)]

      (= :left anchor)
      [x (- y (brshift tt-height 1))]

      (= :right anchor)
      [(- x tt-width) (- y (brshift tt-height 1))]

      (= :center anchor)
      [(- x (brshift tt-width 1)) (- y (brshift tt-height 1))]

      true
      (do
        (log/warning "Unknown anchor value: %n, default to :top-left" anchor)
        [x y])))

  (when (or (not= x adjusted-x)
            (not= y adjusted-y))
    (SendMessage tt-hwnd TTM_TRACKPOSITION
                 0 (bor (band adjusted-x 0xffff) (blshift (band adjusted-y 0xffff) 16)))))


(defn- get-tooltip-default-position [work-area anchor]
  (def [wa-width wa-height] (rect-size work-area))

  (cond
    (or (= :top-left anchor)
        (= :left-top anchor))
    [(in work-area :left) (in work-area :top)]

    (or (= :top-right anchor)
        (= :right-top anchor))
    [(in work-area :right) (in work-area :top)]

    (or (= :bottom-left anchor)
        (= :left-bottom anchor))
    [(in work-area :left) (in work-area :bottom)]

    (or (= :bottom-right anchor)
        (= :right-bottom anchor))
    [(in work-area :right) (in work-area :bottom)]

    (= :top anchor)
    [(+ (in work-area :left) (brshift wa-width 1)) (in work-area :top)]

    (= :bottom anchor)
    [(+ (in work-area :left) (brshift wa-width 1)) (in work-area :bottom)]

    (= :left anchor)
    [(in work-area :left) (+ (in work-area :top) (brshift wa-height 1))]

    (= :right anchor)
    [(in work-area :right) (+ (in work-area :top) (brshift wa-height 1))]

    (= :center anchor)
    [(+ (in work-area :left) (brshift wa-width 1))
     (+ (in work-area :top) (brshift wa-height 1))]

    true
    (do
      (log/warning "Unknown anchor value: %n, default to :top-left" anchor)
      [(in work-area :left) (in work-area :top)])))


(defn show-tooltip [state owner-hwnd tt-id text &opt opt-x opt-y opt-timeout opt-anchor opt-work-area]
  (def tooltip (get-in state [:tooltips tt-id]))

  (def timeout
    (if opt-timeout
      opt-timeout
      (if-let [to (get-in tooltip [:timeout])]
        to
        const/DEFAULT-TOOLTIP-TIMEOUT)))

  (def anchor
    (if opt-anchor
      opt-anchor
      (if-let [ac (get-in tooltip [:anchor])]
        ac
        const/DEFAULT-TOOLTIP-ANCHOR)))

  (def max-width
    (if-let [mw (get-in tooltip [:max-width])]
      mw
      const/DEFAULT-TOOLTIP-MAX-WIDTH))

  (def [x y]
    (if (or (nil? opt-x) (nil? opt-y))
      # Default to the current monitor
      (if-let [wa (or opt-work-area
                      (get-current-work-area state))]
        (get-tooltip-default-position wa anchor)
        # Something went wrong, try our best to return a coordinate....
        [0 0])
      [opt-x opt-y]))

  (def tt-hwnd? (get-in tooltip [:hwnd]))
  (def [tt-hwnd tt-info]
    (if tt-hwnd?
      [tt-hwnd? (in tooltip :info)]
      (let [uid (resume (in state :tooltip-uid-generator))
            created (create-tooltip-window owner-hwnd uid false text max-width)]
        (if (nil? created)
          [nil nil]
          created))))

  (when tt-hwnd
    (if tooltip
      (do
        (put tooltip :hwnd tt-hwnd)
        (put tooltip :info tt-info))
      (put (in state :tooltips)
           tt-id
           @{:timeout (get-in tooltip [:timeout])
             :hwnd tt-hwnd
             :info tt-info}))

    (def updated-info
      (TTTOOLINFO :hwnd (in tt-info :hwnd)
                  :uId (in tt-info :uId)
                  :lpszText (buffer text "\0")))

    (SendMessage tt-hwnd TTM_UPDATETIPTEXT 0 (in updated-info :address))
    (SendMessage tt-hwnd TTM_TRACKPOSITION 0 (bor (band x 0xffff) (blshift (band y 0xffff) 16)))
    (SendMessage tt-hwnd TTM_TRACKACTIVATE 1 (in updated-info :address))

    # This must be called after TTM_TRACKACTIVATE, or the tooltip window
    # geometry it gets will be wrong.
    (adjust-tooltip-position tt-hwnd anchor x y)

    (def timer-id (tooltip-uid-to-timer-id (in tt-info :uId)))
    (if (> timeout 0)
      (when (= (int/u64 0)
               (SetTimer owner-hwnd timer-id timeout nil))
        (log/debug "SetTimer failed for tooltip %n(%n): %n"
                   tt-id (in tt-info :uId) (GetLastError)))
      (KillTimer owner-hwnd timer-id))))


(defn hide-tooltip [state tt-id]
  (when-let [tooltip (get-in state [:tooltips tt-id])]
    (def {:hwnd tt-hwnd
          :info tt-info}
      tooltip)
    (when tt-hwnd
      (SendMessage tt-hwnd TTM_TRACKACTIVATE 0 (in tt-info :address)))))


(defn- msg-wnd-handle-show-tooltip [hwnd _msg wparam _lparam _hook-handler state]
  (let [[tt-id text opt-x opt-y opt-timeout opt-anchor opt-work-area] (unmarshal-and-free wparam)]
    (show-tooltip state hwnd tt-id text opt-x opt-y opt-timeout opt-anchor opt-work-area))
  0)


(defn- msg-wnd-handle-hide-tooltip [_hwnd _msg wparam _lparam _hook-handler state]
  (let [tt-id (unmarshal-and-free wparam)]
    (hide-tooltip state tt-id))
  0)


(defn- msg-wnd-handle-show-tooltip-group [hwnd _msg wparam _lparam _hook-handler state]
  (let [[tg-list opt-timeout opt-anchor opt-work-area] (unmarshal-and-free wparam)]
    (each [tt-id text opt-x opt-y] tg-list
      (show-tooltip state hwnd tt-id text opt-x opt-y opt-timeout opt-anchor opt-work-area)))
  0)


(defn- msg-wnd-handle-hide-tooltip-group [_hwnd _msg wparam _lparam _hook-handler state]
  (let [tg-list (unmarshal-and-free wparam)]
    (each tt-id tg-list
      (hide-tooltip state tt-id)))
  0)


(defn- set-tooltip-property [tooltips tt-id prop value]
  (def tooltip
    (if-let [tt (in tooltips tt-id)]
      tt
      @{}))
  (put tooltip prop value)
  (put tooltips tt-id tooltip))


(defn- msg-wnd-handle-set-tooltip-timeouts [_hwnd _msg wparam _lparam _hook-handler state]
  (let [timeouts (unmarshal-and-free wparam)
        tooltips (in state :tooltips)]
    (eachp [tt-id tt-timeout] timeouts
      (set-tooltip-property tooltips tt-id :timeout tt-timeout))
    (log/debug "New tooltips: %n" tooltips))
  0)


(defn- msg-wnd-handle-set-tooltip-anchors [_hwnd _msg wparam _lparam _hook-handler state]
  (let [anchors (unmarshal-and-free wparam)
        tooltips (in state :tooltips)]
    (eachp [tt-id tt-anchor] anchors
      (set-tooltip-property tooltips tt-id :anchor tt-anchor))
    (log/debug "New tooltips: %n" tooltips))
  0)


(defn- msg-wnd-handle-set-tooltip-max-widths [_hwnd _msg wparam _lparam _hook-handler state]
  (let [max-widths (unmarshal-and-free wparam)
        tooltips (in state :tooltips)]
    (eachp [tt-id tt-max-width] max-widths
      (set-tooltip-property tooltips tt-id :max-width tt-max-width))
    (log/debug "New tooltips: %n" tooltips))
  0)


(defn- msg-wnd-handle-update-work-area [_hwnd _msg wparam _lparam _hook-handler state]
  (def work-area (unmarshal-and-free wparam))
  (log/debug "Updated work area: %n" work-area)
  (put state :last-active-work-area work-area)
  0)


(defn- alloc-new-message [msg-wndproc-state]
  (def last-msg (in msg-wndproc-state :last-custom-message))
  (def custom-msgs (in msg-wndproc-state :custom-messages))
  (var new-msg nil)
  (for i (+ 1 CUSTOM-MSG) (+ 1 last-msg)
    (unless (has-key? custom-msgs i)
      (set new-msg i)
      (break)))
  (unless new-msg
    (set new-msg (+ 1 last-msg))
    (if (> new-msg (int/u64 0xBFFF))
      (do
        (log/error "Custom message ID overflow")
        (set new-msg nil))
      # else
      (put msg-wndproc-state :last-custom-message new-msg)))
  new-msg)


(defn- msg-wnd-handle-add-custom-msg [_hwnd _msg wparam _lparam _hook-handler state]
  (def handler-fn (unmarshal-and-free wparam))
  (unless (or (function? handler-fn) (cfunction? handler-fn))
    (log/error "Invalid custom message handler function: %n" handler-fn)
    (break -1))

  (def new-msg (alloc-new-message state))
  (unless new-msg
    (break -1))

  (put (in state :custom-messages) new-msg handler-fn)
  (int/to-number new-msg))


(defn- msg-wnd-handle-remove-custom-msg [_hwnd _msg wparam _lparam _hook-handler state]
  # WPARAM and messages all use int/u64
  (def msg wparam)
  (def custom-msgs (in state :custom-messages))

  (def handler-fn (in custom-msgs msg))
  (unless handler-fn
    (log/warning "Unknown custom message: %n" msg)
    (break -1))
  (put custom-msgs msg nil)
  0)


(defn- msg-wnd-handle-notify-icon-callback [hwnd _msg wparam lparam _hook-handler _state]
  (def notif-event (LOWORD lparam))
  (def anchor-x (GET_X_LPARAM wparam))
  (def anchor-y (GET_Y_LPARAM wparam))

  (case (int/u64 notif-event)
    WM_CONTEXTMENU
    (show-notify-icon-menu hwnd anchor-x anchor-y))
  0)


(defn- msg-wnd-handle-wm-command [hwnd _msg wparam _lparam hook-handler state]
  (case wparam
    ID_MENU_EXIT
    (do
      (try
        (remove-notify-icon hwnd)
        ((err fib)
         (show-error-and-exit err 1 (get-stack-trace fib))))
      (when-let [tooltip (in state :cur-frame-tooltip)]
        (def [tt-hwnd _tt-info] tooltip)
        (DestroyWindow tt-hwnd))
      (when-let [tooltip (in state :tooltip)]
        (def [tt-hwnd _tt-info] tooltip)
        (DestroyWindow tt-hwnd))
      (DestroyWindow hwnd))

    ID_MENU_RESET_KBD_HOOKS
    (PostMessage hwnd SET-HOOKS-MSG 0 0)

    ID_MENU_UPDATE_MONITOR_LAYOUT
    (ev/give (in hook-handler :chan) :ui/display-changed)

    ID_MENU_LAUNCH_REPL
    (ev/give (in hook-handler :chan) :ui/launch-repl)

    ID_MENU_VERSION
    (show-version-info))
  0)


(defn- msg-wnd-handle-wm-timer [hwnd _msg wparam _lparam hook-handler state]
  (case wparam
    TIMER-ID-DISPLAY-CHANGE
    (do
      (KillTimer hwnd TIMER-ID-DISPLAY-CHANGE)
      (ev/give (in hook-handler :chan) :ui/display-changed))

    # Default branch
    (if (= TOOLTIP-TIMER-BASE (band wparam TOOLTIP-TIMER-BASE-MASK))
      # It's a timer for tooltips
      (do
        (KillTimer hwnd wparam)
        (eachp [tt-id tt] (in state :tooltips)
          (when-let [tt-info (in tt :info)]
            (def timer-id (tooltip-uid-to-timer-id (in tt-info :uId)))
            (when (= wparam timer-id)
              (PostMessage hwnd HIDE-TOOLTIP-MSG (alloc-and-marshal tt-id) 0)))))
      (log/warning "Unknown timer: %n" wparam)))
  0)


(defn- msg-wnd-handle-wm-displaychange [hwnd msg wparam _lparam hook-handler state]
  # XXX: This function also handles WM_SETTINGCHANGE
  (when (= msg WM_SETTINGCHANGE)
    (unless (= wparam SPI_SETWORKAREA)
      (break) # Early return
      ))

  # Clear cached desktop rect, see get-desktop-rect and get-current-work-area
  (put state :desktop-rect nil)

  # GetMonitorInfo() may get called before the task bar is properly set up,
  # and return an inaccurate work area in that case. This timer is here so that
  # we can wait for things to settle down before actually updating the monitor
  # layout.
  (when (= (int/u64 0)
           (SetTimer hwnd TIMER-ID-DISPLAY-CHANGE
                     const/DISPLAY-CHANGE-DELAY-TIME
                     nil))
    (log/debug "SetTimer failed for TIMER-ID-DISPLAY-CHANGE: %n" (GetLastError))
    # :ui/display-changed should be sent when the timer fires, but
    # since SetTimer failed, we fall back to send it here directly.
    (ev/give (in hook-handler :chan) :ui/display-changed))
  0)


(defn- msg-wnd-handle-show-error-and-exit [hwnd _msg wparam _lparam _hook-handler _state]
  (let [msg (unmarshal-and-free wparam)]
    (MessageBox hwnd msg "Error" (bor MB_ICONEXCLAMATION MB_OK))
    (PostMessage hwnd WM_COMMAND ID_MENU_EXIT 0))
  0)


(defn- msg-wnd-handle-wm-close [hwnd _msg _wparam _lparam _hook-handler _state]
  (DestroyWindow hwnd)
  0)


(defn- msg-wnd-handle-wm-destroy [_hwnd _msg _wparam _lparam _hook-handler _state]
  (PostQuitMessage 0)
  0)


(def msg-wnd-handlers
  {SET-KEYMAP-MSG msg-wnd-handle-set-keymap
   SET-KEY-MODE-MSG msg-wnd-handle-set-key-mode

   SET-HOOKS-MSG msg-wnd-handle-set-hooks
   REMOVE-HOOKS-MSG msg-wnd-handle-remove-hooks

   SHOW-TOOLTIP-MSG msg-wnd-handle-show-tooltip
   HIDE-TOOLTIP-MSG msg-wnd-handle-hide-tooltip
   SHOW-TOOLTIP-GROUP-MSG msg-wnd-handle-show-tooltip-group
   HIDE-TOOLTIP-GROUP-MSG msg-wnd-handle-hide-tooltip-group
   SET-TOOLTIP-TIMEOUTS-MSG msg-wnd-handle-set-tooltip-timeouts
   SET-TOOLTIP-ANCHORS-MSG msg-wnd-handle-set-tooltip-anchors
   SET-TOOLTIP-MAX-WIDTHS-MSG msg-wnd-handle-set-tooltip-max-widths

   UPDATE-WORK-AREA-MSG msg-wnd-handle-update-work-area

   ADD-CUSTOM-MSG-MSG msg-wnd-handle-add-custom-msg
   REMOVE-CUSTOM-MSG-MSG msg-wnd-handle-remove-custom-msg

   NOTIFY-ICON-CALLBACK-MSG msg-wnd-handle-notify-icon-callback

   WM_COMMAND msg-wnd-handle-wm-command
   WM_TIMER msg-wnd-handle-wm-timer

   WM_DISPLAYCHANGE msg-wnd-handle-wm-displaychange
   WM_SETTINGCHANGE msg-wnd-handle-wm-displaychange # XXX: Reuse WM_DISPLAYCHANGE handler

   SHOW-ERROR-AND-EXIT-MSG msg-wnd-handle-show-error-and-exit

   WM_CLOSE msg-wnd-handle-wm-close
   WM_DESTROY msg-wnd-handle-wm-destroy})


(defn- msg-wndproc [hwnd msg wparam lparam hook-handler state]
  (log/debug "################## msg-wndproc ##################")
  (log/debug "hwnd = %p" hwnd)
  (log/debug "msg = %p" msg)
  (log/debug "wparam = %p" wparam)
  (log/debug "lparam = %p" lparam)

  (def handler-ret
    (if-let [handler (in msg-wnd-handlers msg)]
      (handler hwnd msg wparam lparam hook-handler state)
      (if-let [custom-msgs (in state :custom-messages)
               custom-handler (in custom-msgs msg)]
        (try
          (custom-handler hwnd msg wparam lparam hook-handler state)
          ((err fib)
           (log/warning "Custom message handler failed: %n\n%s"
                        err
                        (get-stack-trace fib))
           0 # !!! IMPORTANT
           ))
        (DefWindowProc hwnd msg wparam lparam))))

  (if (or (int? handler-ret)
          (= :core/s64 (type handler-ret)))
    handler-ret
    (do
      (log/warning "UI Message handler returned: %n" handler-ret)
      # XXX: Coerce to zero
      0)))


(defn- create-msg-window [hInstance hook-handler]
  (def class-name "JwnoMsgWinClass")
  (def msg-wndproc-state
    @{:tooltips @{:current-frame @{:timeout const/DEFAULT-CURRENT-FRAME-TOOLTIP-TIMEOUT
                                   :anchor const/DEFAULT-CURRENT-FRAME-TOOLTIP-ANCHOR}
                  :keymap @{:timeout const/DEFAULT-KEYMAP-TOOLTIP-TIMEOUT}}
      :tooltip-uid-generator (tooltip-uid-generator 0)
      :custom-messages @{}
      :last-custom-message CUSTOM-MSG})
  (def wc
    (WNDCLASSEX
     :lpfnWndProc (fn [hwnd msg wparam lparam]
                    (msg-wndproc hwnd msg wparam lparam hook-handler msg-wndproc-state))
     :hInstance hInstance
     :lpszClassName class-name))
  (when (null? (RegisterClassEx wc))
    (error (string/format "Window class registration failed: 0x%x" (GetLastError))))
  (def hwnd
    (CreateWindowEx 0                      # dwExStyle
                    class-name             # lpClassName
                    "Jwno Message Window"  # lpWindowName
                    0                      # dwStyle
                    0                      # x
                    0                      # y
                    100                    # nWidtn
                    100                    # nHeight
                    nil                    # hWndParent
                    nil                    # hMenu
                    hInstance              # hInstance
                    nil                    # lpParam
                    ))
  (when (null? hwnd)
    (error (string/format "Window creation failed: 0x%x" (GetLastError))))
  hwnd)


(defn- init-timer []
  (def timer-id (SetTimer nil 0 GC-TIMER-INTERVAL nil))
  (when (= timer-id (int/u64 0))
    (log/warning "Failed to create GC timer: %n" (GetLastError)))
  (log/debug "timer-id = %n" timer-id)
  timer-id)


(defn- deinit-timer [timer-id]
  (when (> timer-id (int/u64 0))
    (KillTimer nil timer-id)))


(defn ui-thread [hInstance argv0 keymap chan]
  (def hook-handler (keyboard-hook-handler keymap))
  (put hook-handler :chan chan)

  (def hook-id
    (SetWindowsHookEx WH_KEYBOARD_LL
                      (fn [code wparam hook-struct]
                        (keyboard-hook-proc code
                                            wparam
                                            hook-struct
                                            hook-handler))
                      nil
                      0))
  (when (null? hook-id)
    (show-error-and-exit (string/format "Failed to enable windows hook: 0x%x" (GetLastError)) 1))
  (put hook-handler :hook-id hook-id)

  (def msg-hwnd
    (try
      (create-msg-window hInstance hook-handler)
      ((err fib)
       (show-error-and-exit err 1 (get-stack-trace fib)))))
  (try
    (create-notify-icon hInstance msg-hwnd argv0)
    ((err fib)
     (show-error-and-exit err 1 (get-stack-trace fib))))

  (def gc-timer-id (init-timer))

  (ev/give chan [:ui/initialized (GetCurrentThreadId) msg-hwnd])

  (msg-loop chan gc-timer-id hook-handler)

  (deinit-timer gc-timer-id)
  (UnhookWindowsHookEx hook-id)
  (ev/give chan :ui/exit))


(defn ui-manager-initialized [self thread-id msg-hwnd]
  (put self :thread-id thread-id)
  (put self :msg-hwnd msg-hwnd))


(defn ui-manager-post-message [self msg wparam lparam]
  (if-let [msg-hwnd (in self :msg-hwnd)]
    (PostMessage msg-hwnd msg wparam lparam)
    (error "ui thread is not initialized")))


(defn ui-manager-send-message [self msg wparam lparam]
  (if-let [msg-hwnd (in self :msg-hwnd)]
    (SendMessage msg-hwnd msg wparam lparam)
    (error "ui thread is not initialized")))


(defn ui-manager-set-keymap [self buf-ptr]
  (ui-manager-post-message self SET-KEYMAP-MSG buf-ptr 0))


(defn ui-manager-set-key-mode [self new-mode]
  (def buf-ptr (alloc-and-marshal new-mode))
  (ui-manager-post-message self SET-KEY-MODE-MSG buf-ptr 0))


(defn ui-manager-set-hooks [self]
  (ui-manager-post-message self SET-HOOKS-MSG 0 0))


(defn ui-manager-remove-hooks [self]
  (ui-manager-post-message self REMOVE-HOOKS-MSG 0 0))


(defn ui-manager-show-tooltip [self tt-id text &opt x y timeout anchor work-area]
  (ui-manager-post-message self
                           SHOW-TOOLTIP-MSG
                           (alloc-and-marshal [tt-id text x y timeout anchor work-area])
                           0))


(defn ui-manager-hide-tooltip [self tt-id]
  (ui-manager-post-message self HIDE-TOOLTIP-MSG (alloc-and-marshal tt-id) 0))


(defn ui-manager-show-tooltip-group [self tg-list &opt timeout anchor work-area]
  (ui-manager-post-message self
                           SHOW-TOOLTIP-GROUP-MSG
                           (alloc-and-marshal [tg-list timeout anchor work-area])
                           0))


(defn ui-manager-hide-tooltip-group [self tg-list]
  (ui-manager-post-message self
                           HIDE-TOOLTIP-GROUP-MSG
                           (alloc-and-marshal tg-list)
                           0))


(defn ui-manager-set-tooltip-timeout [self tt-id timeout]
  (ui-manager-post-message self
                           SET-TOOLTIP-TIMEOUTS-MSG
                           (alloc-and-marshal {tt-id timeout})
                           0))


(defn ui-manager-set-tooltip-anchor [self tt-id anchor]
  (ui-manager-post-message self
                           SET-TOOLTIP-ANCHORS-MSG
                           (alloc-and-marshal {tt-id anchor})
                           0))


(defn ui-manager-set-tooltip-max-width [self tt-id max-width]
  (ui-manager-post-message self
                           SET-TOOLTIP-MAX-WIDTHS-MSG
                           (alloc-and-marshal {tt-id max-width})
                           0))


(defn ui-manager-update-work-area [self rect]
  (ui-manager-post-message self
                           UPDATE-WORK-AREA-MSG
                           (alloc-and-marshal rect)
                           0))


(defn ui-manager-add-custom-message [self handler-fn]
  (ui-manager-send-message self
                           ADD-CUSTOM-MSG-MSG
                           (alloc-and-marshal handler-fn)
                           0))


(defn ui-manager-remove-custom-message [self msg]
  (ui-manager-send-message self
                           REMOVE-CUSTOM-MSG-MSG
                           msg
                           0))


(defn ui-manager-show-error-and-exit [self msg]
  (def buf-ptr (alloc-and-marshal msg))
  (ui-manager-post-message self SHOW-ERROR-AND-EXIT-MSG buf-ptr 0))


(defn ui-manager-destroy [self]
  (ui-manager-post-message self WM_COMMAND ID_MENU_EXIT 0))


(def ui-manager-proto
  @{:initialized ui-manager-initialized
    :post-message ui-manager-post-message
    :send-message ui-manager-send-message
    :set-keymap ui-manager-set-keymap
    :set-key-mode ui-manager-set-key-mode
    :set-hooks ui-manager-set-hooks
    :remove-hooks ui-manager-remove-hooks
    :show-tooltip ui-manager-show-tooltip
    :hide-tooltip ui-manager-hide-tooltip
    :show-tooltip-group ui-manager-show-tooltip-group
    :hide-tooltip-group ui-manager-hide-tooltip-group
    :set-tooltip-timeout ui-manager-set-tooltip-timeout
    :set-tooltip-anchor ui-manager-set-tooltip-anchor
    :set-tooltip-max-width ui-manager-set-tooltip-max-width
    :update-work-area ui-manager-update-work-area
    :add-custom-message ui-manager-add-custom-message
    :remove-custom-message ui-manager-remove-custom-message
    :show-error-and-exit ui-manager-show-error-and-exit
    :destroy ui-manager-destroy})


(defn ui-manager [h-inst argv0 keymap]
  (def chan (ev/thread-chan const/DEFAULT-CHAN-LIMIT))
  (ev/spawn-thread
   (ui-thread h-inst argv0 keymap chan))
  (table/setproto
   @{:chan chan
     # Members below are set with the :ui/initialized message
     :thread-id nil
     :msg-hwnd nil}
   ui-manager-proto))
