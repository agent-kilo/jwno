(use jw32/winuser)
(use jw32/combaseapi)
(use jw32/uiautomation)
(use jw32/errhandlingapi)


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
     (do (MessageBox nil "GetMessage() Failed!" "Error!"
                     (bor MB_ICONEXCLAMATION MB_OK))
         (break))

     (do
       (print "---- New message!")
       (TranslateMessage msg)
       (DispatchMessage msg)
       (buffer/new (* 1024 1024))))))


(defn main [&]
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
