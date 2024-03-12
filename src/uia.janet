(use jw32/combaseapi)
(use jw32/uiautomation)
(use jw32/errhandlingapi)

(import ./log)


(defn- handle-window-opened-event [sender event-id chan]
  (log/debug "#################### handle-window-opened-event ####################")
  (log/debug "++++ sender: %p" (:get_CachedName sender))
  (log/debug "++++ class: %p" (:get_CachedClassName sender))
  (log/debug "++++ event-id: %d" event-id)
  (def win-obj @{:name (:get_CachedName sender)
                 :class-name (:get_CachedClassName sender)
                 :native-window-handle (:get_CachedNativeWindowHandle sender)})
  (ev/give chan [:uia/window-opened win-obj])
  S_OK)


(defn uia-init [chan]
  (CoInitializeEx nil COINIT_MULTITHREADED)
  (def uia (CoCreateInstance CLSID_CUIAutomation nil CLSCTX_INPROC_SERVER IUIAutomation))
  (def root (:GetRootElement uia))

  (def cr (:CreateCacheRequest uia))
  (:AddProperty cr UIA_NamePropertyId)
  (:AddProperty cr UIA_ClassNamePropertyId)
  (:AddProperty cr UIA_BoundingRectanglePropertyId)
  (:AddProperty cr UIA_NativeWindowHandlePropertyId)

  (def scope
    #(bor TreeScope_Element TreeScope_Children)
    TreeScope_Subtree
    )

  (def window-opened-handler
    (:AddAutomationEventHandler
       uia
       UIA_Window_WindowOpenedEventId
       root
       scope
       cr
       (fn [sender event-id]
         (handle-window-opened-event sender event-id chan))))

  (:Release cr)

  [uia [(fn []
          (:RemoveAutomationEventHandler
             uia
             UIA_Window_WindowOpenedEventId
             root
             window-opened-handler))
        (fn []
          (:Release root))]])


(defn uia-deinit [uia deinit-fns]
  (each df deinit-fns
    (df))
  (:Release uia)
  (CoUninitialize))
