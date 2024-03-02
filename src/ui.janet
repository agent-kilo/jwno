(use jw32/winuser)
(use jw32/shellapi)
(use jw32/commctrl)
(use jw32/errhandlingapi)
(use jw32/processthreadsapi)
(use jw32/util)

(use ./resource)
(use ./util)

(import ./log)

(var GC-TIMER-ID (int/u64 0))
(def GC-TIMER-INTERVAL 5000)    # in milliseconds

(def NOTIFY-ICON-ID 1)
(def NOTIFY-ICON-CALLBACK-MSG (+ WM_APP 1))


(defn- msg-loop [&]
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
           GC-TIMER-ID
           # This timer is used to break out from GetMessage, so that
           # the garbage collector has a chance to run.
           (do
             (log/debug "GC timer triggered")
             (set skip true))))
       (when (not skip)
         (TranslateMessage msg)
         (DispatchMessage msg))))))


(defn- create-notify-icon-menu []
  (def hMenu (CreatePopupMenu))
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
      ((err fib)
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
  (when (not= TRUE (Shell_NotifyIcon NIM_ADD nid))
    (error "Failed to create notify icon"))
  (when (not= TRUE (Shell_NotifyIcon NIM_SETVERSION nid))
    (error "Failed to set notify icon version")))


(defn- remove-notify-icon [hwnd]
  (def nid
    (NOTIFYICONDATA
     :hWnd hwnd
     :uID NOTIFY-ICON-ID))
  (when (not= TRUE (Shell_NotifyIcon NIM_DELETE nid))
    (error "Failed to delete notify icon")))


(defn- msg-wndproc [hwnd msg wparam lparam]
  (log/debug "################## msg-wndproc ##################")
  (log/debug "hwnd = %p" hwnd)
  (log/debug "msg = %p" msg)
  (log/debug "wparam = %p" wparam)
  (log/debug "lparam = %p" lparam)

  (case msg
    NOTIFY-ICON-CALLBACK-MSG
    (do
      (def notif-event (LOWORD lparam))
      (def notif-icon-id (HIWORD lparam))
      (def anchor-x (GET_X_LPARAM wparam))
      (def anchor-y (GET_Y_LPARAM wparam))
      (log/debug "================== notify icon callback ==================")
      (log/debug "notif-event = %p" notif-event)
      (log/debug "notif-icon-id = %p" notif-icon-id)
      (log/debug "anchor-x = %p" anchor-x)
      (log/debug "anchor-y = %p" anchor-y)

      (case (int/u64 notif-event)
        WM_CONTEXTMENU
        (show-notify-icon-menu hwnd anchor-x anchor-y)))

    WM_COMMAND
    (case wparam
      ID_MENU_EXIT
      (do
        (try
          (remove-notify-icon hwnd)
          ((err fib)
           (show-error-and-exit err 1)))
        (DestroyWindow hwnd)))

    WM_CLOSE
    (DestroyWindow hwnd)

    WM_DESTROY
    (PostQuitMessage 0)

    (break (DefWindowProc hwnd msg wparam lparam)))

  0)


(defn- create-msg-window [hInstance]
  (def class-name "JwnoMsgWinClass")
  (def wc
    (WNDCLASSEX
     :lpfnWndProc msg-wndproc
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
                    HWND_MESSAGE           # hWndParent
                    nil                    # hMenu
                    hInstance              # hInstance
                    nil                    # lpParam
                    ))
  (when (null? hwnd)
    (error (string/format "Window creation failed: 0x%x" (GetLastError))))
  hwnd)


(defn- keyboard-hook-proc [code wparam hook-struct chan]
  (log/debug "################## keyboard-hook-proc ##################")
  (log/debug "code = %p" code)
  (log/debug "wparam = %p" wparam)
  (log/debug "hook-struct = %p" hook-struct)

  (when (< code 0)
    (break (CallNextHookEx nil code wparam (hook-struct :address))))

  (def hook-struct-tbl
    @{:vkCode (hook-struct :vkCode)
      :scanCode (hook-struct :scanCode)
      :extended (hook-struct :flags.extended)
      :lower_il_injected (hook-struct :flags.lower_il_injected)
      :injected (hook-struct :flags.injected)
      :altdown (hook-struct :flags.altdown)
      :up (hook-struct :flags.up)
      :time (hook-struct :time)
      :dwExtraInfo (hook-struct :dwExtraInfo)})

  (ev/give chan [:ui/hook-keyboard-ll wparam hook-struct-tbl])
  (CallNextHookEx nil code wparam (hook-struct :address)))


(defn- init-timer []
  (set GC-TIMER-ID (SetTimer nil 0 GC-TIMER-INTERVAL nil))
  (log/debug "GC-TIMER-ID = %p" GC-TIMER-ID)
  (when (= GC-TIMER-ID (int/u64 0))
    (log/warning "Failed to create GC timer")))


(defn- deinit-timer []
  (when (> GC-TIMER-ID (int/u64 0))
    (KillTimer nil GC-TIMER-ID)))


(defn ui-thread [hInstance argv0 chan]
  (def msg-hwnd
    (try
      (create-msg-window hInstance)
      ((err fib)
       (show-error-and-exit err 1))))
  (try
    (create-notify-icon hInstance msg-hwnd argv0)
    ((err fib)
     (show-error-and-exit err 1)))

  (def hook-id
    (SetWindowsHookEx WH_KEYBOARD_LL
                      (fn [code wparam hook-struct]
                        (keyboard-hook-proc code wparam hook-struct chan))
                      nil
                      0))
  (when (null? hook-id)
    (show-error-and-exit (string/format "Failed to enable windows hook: 0x%x" (GetLastError)) 1))

  (init-timer)

  (ev/give chan [:ui/initialized (GetCurrentThreadId)])

  (msg-loop chan)

  (deinit-timer)
  (UnhookWindowsHookEx hook-id)
  (ev/give chan :ui/exit))
