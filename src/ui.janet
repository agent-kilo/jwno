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

(def NOTIFY-ICON-CALLBACK-MSG (+ WM_APP 1))
(def SET-KEYMAP-MSG (+ WM_APP 2))
(def SET-HOOKS-MSG (+ WM_APP 3))
(def REMOVE-HOOKS-MSG (+ WM_APP 4))
(def SHOW-ERROR-AND-EXIT-MSG (+ WM_APP 5))
(def SHOW-CURRENT-FRAME-TOOLTIP-MSG (+ WM_APP 6))
(def HIDE-CURRENT-FRAME-TOOLTIP-MSG (+ WM_APP 7))
(def SHOW-TOOLTIP-MSG (+ WM_APP 8))
(def HIDE-TOOLTIP-MSG (+ WM_APP 9))
(def SET-TOOLTIP-TIMEOUTS-MSG (+ WM_APP 10))

(def TT-ID-CURRENT-FRAME 1)
(def TT-ID-GENERIC 2)

(def TIMER-ID-CURRENT-FRAME-TOOLTIP (int/u64 1))
(def TIMER-ID-GENERIC-TOOLTIP (int/u64 2))
(def TIMER-ID-DISPLAY-CHANGE (int/u64 3))


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
  (AppendMenu hMenu MF_STRING ID_MENU_UPDATE_MONITOR_LAYOUT "Update &Monitor Layout")
  (AppendMenu hMenu MF_STRING ID_MENU_RESET_KBD_HOOKS "&Reset Keyboard Hooks")
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

  (def mod-states (:get-modifier-states handler hook-struct))

  (if-let [binding (:find-binding handler hook-struct mod-states)]
    (do
      (when (or (in mod-states :lwin)
                (in mod-states :rwin))
        # Send a dummy key event to stop the Start Menu from popping up
        (send-input(keyboard-input VK_DUMMY
                                   (if key-up :up :down)
                                   (bor KEI-FLAG-PASSTHROUGH extra-info))))
      (when-let [msg (:handle-binding handler hook-struct binding)]
        (ev/give (in handler :chan) msg))
      1) # !!! IMPORTANT

    (do
      (when-let [msg (:handle-unbound handler hook-struct)]
        (ev/give (in handler :chan) msg))
      (CallNextHookEx nil code wparam (hook-struct :address)))))


(defn- create-tooltip-window [parent-hwnd tt-id &opt center? text]
  (default center? false)
  (default text "")

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

  # For multi-line tooltip text. XXX: arbitrary value
  (SendMessage tt-hwnd TTM_SETMAXTIPWIDTH 0 1000)
  [tt-hwnd t-info])


(defn- msg-wnd-handle-set-keymap [_hwnd wparam _lparam hook-handler _state]
  (let [keymap (unmarshal-and-free wparam)]
    (:set-keymap hook-handler keymap)))


(defn- msg-wnd-handle-set-hooks [_hwnd _wparam _lparam hook-handler _state]
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
    (put hook-handler :hook-id new-hook)))


(defn- msg-wnd-handle-remove-hooks [_hwnd _wparam _lparam hook-handler _state]
  (when-let [old-hook (in hook-handler :hook-id)]
    (log/debug "Removing old hook: %n" old-hook)
    (UnhookWindowsHookEx old-hook)
    (put hook-handler :hook-id nil)))


(defn- msg-wnd-handle-show-current-frame-tooltip [hwnd wparam _lparam _hook-handler state]
  (let [tooltip (in state :cur-frame-tooltip)
        [x y opt-timeout opt-center?] (unmarshal-and-free wparam)]
    (def timeout
      (if (nil? opt-timeout)
        (get-in state [:tooltip-timeouts :current-frame])
        opt-timeout))
    (def center?
      (if (nil? opt-center?)
        true
        opt-center?))

    (def tooltip-info
      (if tooltip
        tooltip
        (create-tooltip-window hwnd TT-ID-CURRENT-FRAME center? "Current Frame")))
    (put state :cur-frame-tooltip tooltip-info)

    (when tooltip-info
      (def [tt-hwnd tt-info] tooltip-info)
      (SendMessage tt-hwnd TTM_TRACKPOSITION 0 (bor (band x 0xffff) (blshift (band y 0xffff) 16)))
      (SendMessage tt-hwnd TTM_TRACKACTIVATE 1 (in tt-info :address))
      (when (> timeout 0)
        (when (= (int/u64 0)
                 (SetTimer hwnd TIMER-ID-CURRENT-FRAME-TOOLTIP timeout nil))
          (log/debug "SetTimer failed for TIMER-ID-CURRENT-FRAME-TOOLTIP: %n"
                     (GetLastError)))))))


(defn- msg-wnd-handle-hide-current-frame-tooltip [_hwnd _wparam _lparam _hook-handler state]
  (when-let [tooltip (in state :cur-frame-tooltip)]
    (def [tt-hwnd tt-info] tooltip)
    (SendMessage tt-hwnd TTM_TRACKACTIVATE 0 (in tt-info :address))))


(defn- msg-wnd-handle-show-tooltip [hwnd wparam _lparam _hook-handler state]
  (let [[text x y opt-timeout opt-center?] (unmarshal-and-free wparam)]
    (def timeout
      (if (nil? opt-timeout)
        (get-in state [:tooltip-timeouts :generic])
        opt-timeout))
    (def center?
      (if (nil? opt-center?)
        false
        opt-center?))

    (when-let [tooltip (in state :tooltip)]
      (def [tt-hwnd tt-info] tooltip)
      (SendMessage tt-hwnd TTM_TRACKACTIVATE 0 (in tt-info :address))
      (DestroyWindow tt-hwnd))

    (def tooltip-info
      (create-tooltip-window hwnd TT-ID-GENERIC center? text))
    (put state :tooltip tooltip-info)

    (when tooltip-info
      (def [tt-hwnd tt-info] tooltip-info)
      (SendMessage tt-hwnd TTM_TRACKPOSITION 0 (bor (band x 0xffff) (blshift (band y 0xffff) 16)))
      (SendMessage tt-hwnd TTM_TRACKACTIVATE 1 (in tt-info :address))
      (when (> timeout 0)
        (when (= (int/u64 0)
                 (SetTimer hwnd TIMER-ID-GENERIC-TOOLTIP timeout nil))
          (log/debug "SetTimer failed for TIMER-ID-GENERIC-TOOLTIP: %n"
                     (GetLastError)))))))


(defn- msg-wnd-handle-hide-tooltip [_hwnd _wparam _lparam _hook-handler state]
  (when-let [tooltip (in state :tooltip)]
    (def [tt-hwnd tt-info] tooltip)
    (SendMessage tt-hwnd TTM_TRACKACTIVATE 0 (in tt-info :address))
    (DestroyWindow tt-hwnd)
    (put state :tooltip nil)))


(defn- msg-wnd-handle-set-tooltip-timeouts [_hwnd wparam _lparam _hook-handler state]
  (let [timeouts (unmarshal-and-free wparam)]
    (eachp [tt-type tt-timeout] timeouts
      (put (in state :tooltip-timeouts) tt-type tt-timeout))))


(defn- msg-wnd-handle-notify-icon-callback [hwnd wparam lparam _hook-handler _state]
  (def notif-event (LOWORD lparam))
  (def anchor-x (GET_X_LPARAM wparam))
  (def anchor-y (GET_Y_LPARAM wparam))

  (case (int/u64 notif-event)
    WM_CONTEXTMENU
    (show-notify-icon-menu hwnd anchor-x anchor-y)))


(defn- msg-wnd-handle-wm-command [hwnd wparam _lparam hook-handler state]
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
    (ev/give (in hook-handler :chan) :ui/display-changed)))


(defn- msg-wnd-handle-wm-timer [hwnd wparam _lparam hook-handler _state]
  (case wparam
    TIMER-ID-CURRENT-FRAME-TOOLTIP
    (do
      (KillTimer hwnd TIMER-ID-CURRENT-FRAME-TOOLTIP)
      (PostMessage hwnd HIDE-CURRENT-FRAME-TOOLTIP-MSG 0 0))

    TIMER-ID-GENERIC-TOOLTIP
    (do
      (KillTimer hwnd TIMER-ID-GENERIC-TOOLTIP)
      (PostMessage hwnd HIDE-TOOLTIP-MSG 0 0))

    TIMER-ID-DISPLAY-CHANGE
    (do
      (KillTimer hwnd TIMER-ID-DISPLAY-CHANGE)
      (ev/give (in hook-handler :chan) :ui/display-changed))

    (log/warning "Unknown timer: %n" wparam)))


(defn- msg-wnd-handle-wm-displaychange [hwnd wparam lparam hook-handler state]
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
    (ev/give (in hook-handler :chan) :ui/display-changed)))


(defn- msg-wnd-handle-show-error-and-exit [hwnd wparam _lparam _hook-handler _state]
  (let [msg (unmarshal-and-free wparam)]
    (MessageBox hwnd msg "Error" (bor MB_ICONEXCLAMATION MB_OK))
    (PostMessage hwnd WM_COMMAND ID_MENU_EXIT 0)))


(defn- msg-wnd-handle-wm-close [hwnd _wparam _lparam _hook-handler _state]
  (DestroyWindow hwnd))


(defn- msg-wnd-handle-wm-destroy [_hwnd _wparam _lparam _hook-handler _state]
  (PostQuitMessage 0))


(def msg-wnd-handlers
  {SET-KEYMAP-MSG msg-wnd-handle-set-keymap

   SET-HOOKS-MSG msg-wnd-handle-set-hooks
   REMOVE-HOOKS-MSG msg-wnd-handle-remove-hooks

   SHOW-CURRENT-FRAME-TOOLTIP-MSG msg-wnd-handle-show-current-frame-tooltip
   HIDE-CURRENT-FRAME-TOOLTIP-MSG msg-wnd-handle-hide-current-frame-tooltip
   SHOW-TOOLTIP-MSG msg-wnd-handle-show-tooltip
   HIDE-TOOLTIP-MSG msg-wnd-handle-hide-tooltip
   SET-TOOLTIP-TIMEOUTS-MSG msg-wnd-handle-set-tooltip-timeouts

   NOTIFY-ICON-CALLBACK-MSG msg-wnd-handle-notify-icon-callback

   WM_COMMAND msg-wnd-handle-wm-command
   WM_TIMER msg-wnd-handle-wm-timer
   WM_DISPLAYCHANGE msg-wnd-handle-wm-displaychange

   SHOW-ERROR-AND-EXIT-MSG msg-wnd-handle-show-error-and-exit

   WM_CLOSE msg-wnd-handle-wm-close
   WM_DESTROY msg-wnd-handle-wm-destroy})


(defn- msg-wndproc [hwnd msg wparam lparam hook-handler state]
  (log/debug "################## msg-wndproc ##################")
  (log/debug "hwnd = %p" hwnd)
  (log/debug "msg = %p" msg)
  (log/debug "wparam = %p" wparam)
  (log/debug "lparam = %p" lparam)

  (if-let [handler (in msg-wnd-handlers msg)]
    (do
      (handler hwnd wparam lparam hook-handler state)
      0)
    (DefWindowProc hwnd msg wparam lparam)))


(defn- create-msg-window [hInstance hook-handler]
  (def class-name "JwnoMsgWinClass")
  (def msg-wndproc-state
    @{:tooltip-timeouts @{:generic const/DEFAULT-GENERIC-TOOLTIP-TIMEOUT
                          :current-frame const/DEFAULT-CURRENT-FRAME-TOOLTIP-TIMEOUT}})
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


(defn ui-manager-set-keymap [self keymap]
  (def buf-ptr (alloc-and-marshal keymap))
  (ui-manager-post-message self SET-KEYMAP-MSG buf-ptr 0))


(defn ui-manager-set-hooks [self]
  (ui-manager-post-message self SET-HOOKS-MSG 0 0))


(defn ui-manager-remove-hooks [self]
  (ui-manager-post-message self REMOVE-HOOKS-MSG 0 0))


(defn ui-manager-show-current-frame-tooltip [self x y &opt timeout center?]
  (ui-manager-post-message self
                           SHOW-CURRENT-FRAME-TOOLTIP-MSG
                           (alloc-and-marshal [x y timeout center?])
                           0))


(defn ui-manager-hide-current-frame-tooltip [self]
  (ui-manager-post-message self HIDE-CURRENT-FRAME-TOOLTIP-MSG 0 0))


(defn ui-manager-show-tooltip [self text x y &opt timeout center?]
  (ui-manager-post-message self
                           SHOW-TOOLTIP-MSG
                           (alloc-and-marshal [text x y timeout center?])
                           0))


(defn ui-manager-hide-tooltip [self]
  (ui-manager-post-message self HIDE-TOOLTIP-MSG 0 0))


(defn ui-manager-set-tooltip-timeout [self type timeout]
  (ui-manager-post-message self
                           SET-TOOLTIP-TIMEOUTS-MSG
                           (alloc-and-marshal {type timeout})
                           0))


(defn ui-manager-show-error-and-exit [self msg]
  (def buf-ptr (alloc-and-marshal msg))
  (ui-manager-post-message self SHOW-ERROR-AND-EXIT-MSG buf-ptr 0))


(defn ui-manager-destroy [self]
  (ui-manager-post-message self WM_COMMAND ID_MENU_EXIT 0))


(def ui-manager-proto
  @{:initialized ui-manager-initialized
    :post-message ui-manager-post-message
    :set-keymap ui-manager-set-keymap
    :set-hooks ui-manager-set-hooks
    :remove-hooks ui-manager-remove-hooks
    :show-current-frame-tooltip ui-manager-show-current-frame-tooltip
    :hide-current-frame-tooltip ui-manager-hide-current-frame-tooltip
    :show-tooltip ui-manager-show-tooltip
    :hide-tooltip ui-manager-hide-tooltip
    :set-tooltip-timeout ui-manager-set-tooltip-timeout
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
