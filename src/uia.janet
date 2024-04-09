(use jw32/_combaseapi)
(use jw32/_uiautomation)
(use jw32/_errhandlingapi)
(use jw32/_util)

(import ./log)


(defmacro with-uia [[binding ctor dtor] & body]
  ~(do
     (def ,binding ,ctor)
     ,(apply defer [(or dtor ~(fn [x] (when (not (nil? x)) (:Release x)))) binding] body)))


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
    (CoCreateInstance CLSID_CUIAutomation8 nil CLSCTX_INPROC_SERVER IUIAutomation6))
  (:put_AutoSetFocus uia false) # To reduce flicker

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

  (def focused
    (try
      (:GetFocusedElementBuildCache uia focus-cr)
      ((err fib)
       # This may fail due to e.g. insufficient privileges
       (log/debug "GetFocusedElementBuildCache failed: %n" err)
       nil)))
  (if-not focused
    (break nil))

  (var ret nil)
  (var cur focused)
  (var parent
       (try
         (:GetParentElementBuildCache walker cur focus-cr)
         ((err fib)
          # The window or its parent may have vanished
          (log/debug "GetParentElementBuildCache failed: %n" err)
          nil)))

  (while true
    (cond
      (and
        # Has a handle
        (not (null? (:get_CachedNativeWindowHandle cur)))
        # Can be transformed
        (not= 0 (:GetCachedPropertyValue cur UIA_IsTransformPatternAvailablePropertyId))
        # Is a window
        (not= 0 (:GetCachedPropertyValue cur UIA_IsWindowPatternAvailablePropertyId)))
      (do
        (set ret cur)
        (break))

      (nil? parent)
      (do
        (:Release cur)
        (break))

      (= root-hwnd (:get_CachedNativeWindowHandle parent))
      (do
        (:Release cur)
        (break)))

    (:Release cur)
    (set cur parent)
    (set parent
         (try
           (:GetParentElementBuildCache walker cur focus-cr)
           ((err fib)
            (log/debug "GetParentElementBuildCache failed: %n" err)
            nil))))

  ret)


(defn get-window-bounding-rect [uia-context hwnd]
  (def {:uia uia} uia-context)
  (with-uia [cr (:CreateCacheRequest uia)]
    (:AddProperty cr UIA_BoundingRectanglePropertyId)
    (with-uia [uia-win (try
                         (:ElementFromHandleBuildCache uia hwnd cr)
                         ((err fib)
                          (log/debug "ElementFromHandle failed: %n" err)
                          nil))]
      (when uia-win
        (:get_CachedBoundingRectangle uia-win)))))
