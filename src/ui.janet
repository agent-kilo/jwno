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
       (when (not skip)
         (TranslateMessage msg)
         (DispatchMessage msg))))))


(defn- create-notify-icon-menu []
  (def hMenu (CreatePopupMenu))
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


(defn- msg-wndproc [hwnd msg wparam lparam hook-handler]
  (log/debug "################## msg-wndproc ##################")
  (log/debug "hwnd = %p" hwnd)
  (log/debug "msg = %p" msg)
  (log/debug "wparam = %p" wparam)
  (log/debug "lparam = %p" lparam)

  (case msg
    SET-KEYMAP-MSG
    (let [keymap (unmarshal-and-free wparam)]
      (:set-keymap hook-handler keymap))

    SET-HOOKS-MSG
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

    REMOVE-HOOKS-MSG
    (when-let [old-hook (in hook-handler :hook-id)]
      (log/debug "Removing old hook: %n" old-hook)
      (UnhookWindowsHookEx old-hook)
      (put hook-handler :hook-id nil))

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
        (DestroyWindow hwnd))

      ID_MENU_RESET_KBD_HOOKS
      (PostMessage hwnd SET-HOOKS-MSG 0 0))

    WM_CLOSE
    (DestroyWindow hwnd)

    WM_DESTROY
    (PostQuitMessage 0)

    (break (DefWindowProc hwnd msg wparam lparam)))

  0)


(defn- create-msg-window [hInstance hook-handler]
  (def class-name "JwnoMsgWinClass")
  (def wc
    (WNDCLASSEX
     :lpfnWndProc (fn [hwnd msg wparam lparam]
                    (msg-wndproc hwnd msg wparam lparam hook-handler))
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


(defn- init-timer []
  (def timer-id (SetTimer nil 0 GC-TIMER-INTERVAL nil))
  (log/debug "timer-id = %n" timer-id)
  (when (= timer-id (int/u64 0))
    (log/warning "Failed to create GC timer"))
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
       (show-error-and-exit err 1))))
  (try
    (create-notify-icon hInstance msg-hwnd argv0)
    ((err fib)
     (show-error-and-exit err 1)))

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


(defn ui-manager-destroy [self]
  (ui-manager-post-message self WM_COMMAND ID_MENU_EXIT 0))


(def ui-manager-proto
  @{:initialized ui-manager-initialized
    :post-message ui-manager-post-message
    :set-keymap ui-manager-set-keymap
    :set-hooks ui-manager-set-hooks
    :remove-hooks ui-manager-remove-hooks
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
