(use jw32/combaseapi)
(use jw32/uiautomation)
(use jw32/errhandlingapi)
(use jw32/util)

(import ./log)


(defn- handle-window-opened-event [sender event-id chan]
  (log/debug "#################### handle-window-opened-event ####################")
  (log/debug "++++ sender: %p" (:get_CachedName sender))
  (log/debug "++++ class: %p" (:get_CachedClassName sender))
  (log/debug "++++ event-id: %d" event-id)
  (def win-obj @{:name (:get_CachedName sender)
                 :class-name (:get_CachedClassName sender)
                 :native-window-handle (:get_CachedNativeWindowHandle sender)})
  (ev/give chan [:uia/window-opened (:get_CachedNativeWindowHandle sender)])
  S_OK)


(defn- handle-focus-changed-event [sender chan]
  (log/debug "#################### handle-focus-changed-event ####################")
  (ev/give chan :uia/focus-changed)
  S_OK)


(defn uia-init-event-handlers [uia element chan]
  (def window-opened-handler
    # Can't use `with` here, or there will be a "can't marshal alive fiber" error
    (let [cr (:CreateCacheRequest uia)]
      (:AddProperty cr UIA_NamePropertyId)
      (:AddProperty cr UIA_ClassNamePropertyId)
      (:AddProperty cr UIA_NativeWindowHandlePropertyId)
      (def handler
        (:AddAutomationEventHandler
           uia
           UIA_Window_WindowOpenedEventId
           element
           TreeScope_Subtree
           cr
           (fn [sender event-id]
             (handle-window-opened-event sender event-id chan))))
      (:Release cr)
      handler))

  (def focus-changed-handler
    (:AddFocusChangedEventHandler
       uia
       nil
       (fn [sender]
         (handle-focus-changed-event sender chan))))

  [(fn []
     (:RemoveAutomationEventHandler
        uia
        UIA_Window_WindowOpenedEventId
        element
        window-opened-handler))
   (fn []
     (:RemoveFocusChangedEventHandler
        uia
        focus-changed-handler))])


(defn uia-init [chan]
  (CoInitializeEx nil COINIT_MULTITHREADED)
  (def uia
    (CoCreateInstance CLSID_CUIAutomation nil CLSCTX_INPROC_SERVER IUIAutomation))
  (def root
    (with [cr (:CreateCacheRequest uia) (cr :Release)]
      (:AddProperty cr UIA_NativeWindowHandlePropertyId)
      (:GetRootElementBuildCache uia cr)))
  (def deinit-fns
    (uia-init-event-handlers uia root chan))
  (def focus-cr
    (:CreateCacheRequest uia))
  (:AddProperty focus-cr UIA_NativeWindowHandlePropertyId)
  (:AddProperty focus-cr UIA_IsTransformPatternAvailablePropertyId)
  (:AddProperty focus-cr UIA_IsWindowPatternAvailablePropertyId)
  (def control-view-walker (:get_ControlViewWalker uia))
  @{:uia uia
    :root root
    :deinit-fns deinit-fns
    :focus-cr focus-cr
    :control-view-walker control-view-walker})


(defn uia-deinit [uia-context]
  (def {:uia uia
        :root root
        :deinit-fns deinit-fns
        :focus-cr focus-cr
        :control-view-walker control-view-walker}
    uia-context)
  (each df deinit-fns
    (df))
  (:Release control-view-walker)
  (:Release focus-cr)
  (:Release root)
  (:Release uia)
  (CoUninitialize))


(defn get-focused-window [uia-context]
  (def {:uia uia
        :root root
        :focus-cr focus-cr
        :control-view-walker walker}
    uia-context)
  (def root-hwnd (:get_CachedNativeWindowHandle root))
  (let [focused (try
                  # This may fail due to e.g. insufficient privileges
                  (:GetFocusedElementBuildCache uia focus-cr)
                  ((err fib)
                   (log/debug (string/format "GetFocusedElementBuildCache failed: %n" err))
                   nil))]
    (if-not focused
      (break nil))
    (var ret nil)
    (var cur focused)
    (var parent (:GetParentElementBuildCache walker cur focus-cr))
    (while (and parent
                (not= root-hwnd (:get_CachedNativeWindowHandle parent)))
      (:Release cur)
      (set cur parent)
      (set parent (:GetParentElementBuildCache walker cur focus-cr))
      (if parent
        (log/debug "Next parent: %n" (:get_CachedNativeWindowHandle parent))
        (log/debug "Next parent: %n" parent)))
    (if-not parent
      (break nil))
    (let [trans-pat-available (:GetCachedPropertyValue cur UIA_IsTransformPatternAvailablePropertyId)
          win-pat-available (:GetCachedPropertyValue cur UIA_IsWindowPatternAvailablePropertyId)]
      (log/debug "IsTransformPatternAvailable = %n" trans-pat-available)
      (log/debug "IsWindowPatternAvailable = %n" win-pat-available)
      (if (and (not= trans-pat-available 0)
               (not= win-pat-available 0))
        # It's a window that can be resized
        cur
        # Something else we don't care about
        # XXX: Maybe we should care about these, e.g. Save As dialogs?
        (do
          (:Release cur)
          nil)))))
