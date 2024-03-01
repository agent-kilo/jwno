(use jw32/winuser)
(use jw32/shellapi)
(use jw32/commctrl)
(use jw32/libloaderapi)
(use jw32/combaseapi)
(use jw32/uiautomation)
(use jw32/errhandlingapi)
(use jw32/consoleapi)

(use ./util)

(import ./ui)
(import ./log)


(defn main [& args]
  (log/init :debug)
  (log/debug "in main")
  #(FreeConsole)

  (def hInstance (GetModuleHandle nil))
  (def ui-chan (ev/thread-chan))

  (ev/spawn-thread
   (ui/ui-thread hInstance (args 0) ui-chan))

  (when (not= (ev/take ui-chan) :ok)
    (show-error-and-exit "UI thread initialization failed" 1))
  (ev/give ui-chan :ok)

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
         (log/debug "#################### Automation Event ####################")
         (log/debug "++++ sender: %p" (:get_CachedName sender))
         (log/debug "++++ class: %p" (:get_CachedClassName sender))
         (log/debug "++++ event-id: %d" event-id)
         #(error "lalala")
         (def name (:get_CachedName sender))
         (def class-name (:get_CachedClassName sender))
         (when (and (= name "File Explorer") (= class-name "CabinetWClass"))
           (def pat (:GetCurrentPatternAs sender UIA_TransformPatternId IUIAutomationTransformPattern))
           (log/debug "++++ pat = %p" pat)
           (when pat
             (:Move pat -5 0)
             (:Resize pat 800 800))
           )
         S_OK)))

  (let [msg (ev/take ui-chan)]
    (when (not= msg :done)
      (log/debug "Unknown message from UI channel: %p" msg)))

  (log/deinit))
