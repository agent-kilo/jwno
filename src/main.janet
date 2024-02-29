(use jw32/winuser)
(use jw32/shellapi)
(use jw32/libloaderapi)
(use jw32/combaseapi)
(use jw32/uiautomation)
(use jw32/errhandlingapi)
(use jw32/util)

(use ./resource)


(def notify-icon-id 1)
(def notify-icon-callback-msg (+ WM_APP 1))


(defn show-error-and-exit [msg exit-code]
  (MessageBox nil
              (string/format "Error: %s" msg)
              "Error"
              (bor MB_ICONEXCLAMATION MB_OK))
  (os/exit exit-code))


(defn win-hook-proc [code wparam lparam]
  (print "################## win-hook-proc ##################")
  (printf "code = %p" code)
  (printf "wparam = %p" wparam)
  (printf "lparam = %p" lparam)
  (CallNextHookEx nil code wparam lparam))


(defn msg-loop [&]
  (def msg (MSG))

  (forever
   (case (GetMessage msg nil 0 0)
     0
     (break)

     -1
     (show-error-and-exit "GetMessage() failed" 1)

     (do
       (print "---- New message!")
       (TranslateMessage msg)
       (DispatchMessage msg)
       (buffer/new (* 1024 1024))))))


(defn create-notify-icon-menu []
  (def hMenu (CreatePopupMenu))
  (AppendMenu hMenu MF_STRING ID_MENU_EXIT "E&xit")
  hMenu)


(defn show-notify-icon-menu [hwnd x y]
  (def hMenu (create-notify-icon-menu))
  (printf ".... hMenu = %p" hMenu)
  (SetForegroundWindow hwnd)
  (var uFlags TPM_RIGHTBUTTON)
  (if (not= (GetSystemMetrics SM_MENUDROPALIGNMENT) 0)
    (set uFlags (bor uFlags TPM_RIGHTALIGN))
    (set uFlags (bor uFlags TPM_LEFTALIGN)))
  (def tpm-ret
    (TrackPopupMenuEx hMenu uFlags x y hwnd nil))
  (printf ".... tpm-ret = %p" tpm-ret)
  (DestroyMenu hMenu))


(defn create-notify-icon [hwnd]
  (def nid
    (NOTIFYICONDATA
     :hWnd hwnd
     :uID notify-icon-id
     :uFlags (bor NIF_MESSAGE NIF_ICON NIF_TIP NIF_SHOWTIP)
     :uCallbackMessage notify-icon-callback-msg
     :hIcon (LoadIcon nil IDI_QUESTION)
     :szTip "Jwno"
     :uVersion NOTIFYICON_VERSION_4))
  (when (not= TRUE (Shell_NotifyIcon NIM_ADD nid))
    (error "Failed to create notify icon"))
  (when (not= TRUE (Shell_NotifyIcon NIM_SETVERSION nid))
    (error "Failed to set notify icon version")))


(defn remove-notify-icon [hwnd]
  (def nid
    (NOTIFYICONDATA
     :hWnd hwnd
     :uID notify-icon-id))
  (when (not= TRUE (Shell_NotifyIcon NIM_DELETE nid))
    (error "Failed to delete notify icon")))


(defn wndproc [hwnd msg wparam lparam]
  (print "################## wndproc ##################")
  (printf "hwnd = %p" hwnd)
  (printf "msg = %p" msg)
  (printf "wparam = %p" wparam)
  (printf "lparam = %p" lparam)

  (case msg

    notify-icon-callback-msg
    (do
      (def notif-event (LOWORD lparam))
      (def notif-icon-id (HIWORD lparam))
      (def anchor-x (GET_X_LPARAM wparam))
      (def anchor-y (GET_Y_LPARAM wparam))
      (print "================== notify icon callback ==================")
      (printf "notif-event = %p" notif-event)
      (printf "notif-icon-id = %p" notif-icon-id)
      (printf "anchor-x = %p" anchor-x)
      (printf "anchor-y = %p" anchor-y)

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


(defn create-msg-window [hInstance]
  (def class-name "JwnoMsgWinClass")
  (def wc
    (WNDCLASSEX
     :lpfnWndProc wndproc
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


(defn main [&]
  (def hInstance (GetModuleHandle nil))

  (def hwnd
    (try
      (create-msg-window hInstance)
      ((err fib)
       (show-error-and-exit err 1))))

  (try
    (create-notify-icon hwnd)
    ((err fib)
     (show-error-and-exit err 1)))

  (CoInitializeEx nil COINIT_MULTITHREADED)
  (def uia (CoCreateInstance CLSID_CUIAutomation8 nil CLSCTX_INPROC_SERVER IUIAutomation6))
  (def root (:GetRootElement uia))

  (def cr (:CreateCacheRequest uia))
  (:AddProperty cr UIA_NamePropertyId)
  (:AddProperty cr UIA_ClassNamePropertyId)
  (:AddProperty cr UIA_BoundingRectanglePropertyId)
  (:AddProperty cr UIA_NativeWindowHandlePropertyId)
  (:AddPattern cr UIA_TransformPatternId)

  (def scope
    #(bor TreeScope_Element TreeScope_Children)
    TreeScope_Subtree
    )

  (def win-open-handler
    (:AddAutomationEventHandler
       uia
       UIA_Window_WindowOpenedEventId
       root
       scope
       cr
       (fn [sender event-id]
         (printf "#################### Automation Event ####################")
         (printf "++++ sender: %p" (:get_CachedName sender))
         (printf "++++ class: %p" (:get_CachedClassName sender))
         (printf "++++ event-id: %d" event-id)
         #(error "lalala")
         (def name (:get_CachedName sender))
         (def class-name (:get_CachedClassName sender))
         (when (and (= name "File Explorer") (= class-name "CabinetWClass"))
           (def pat (:GetCurrentPatternAs sender UIA_TransformPatternId IUIAutomationTransformPattern))
           (printf "++++ pat = %p" pat)
           (when pat
             (:Move pat -5 0)
             (:Resize pat 800 800))
           )
         S_OK)))

  (def hook-id (SetWindowsHookEx WH_KEYBOARD_LL win-hook-proc nil 0))
  (printf "---- hook-id = %p" hook-id)

  (msg-loop))
