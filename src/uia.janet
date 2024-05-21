(use jw32/_combaseapi)
(use jw32/_uiautomation)
(use jw32/_errhandlingapi)
(use jw32/_util)

(use ./util)

(import ./const)
(import ./log)


(defmacro with-uia [[binding ctor dtor] & body]
  ~(do
     (def ,binding ,ctor)
     ,(apply defer [(or dtor ~(fn [x] (when (not (nil? x)) (:Release x)))) binding] body)))


(defmacro is-valid-uia-window? [uia-win]
  ~(and (not= 0 (:GetCachedPropertyValue ,uia-win UIA_IsTransformPatternAvailablePropertyId))
        (not= 0 (:GetCachedPropertyValue ,uia-win UIA_TransformCanMovePropertyId))
        (not= 0 (:GetCachedPropertyValue ,uia-win UIA_IsWindowPatternAvailablePropertyId))))


(defn- handle-window-opened-event [sender event-id chan]
  (def name (:get_CachedName sender))
  (def class-name (:get_CachedClassName sender))
  (def hwnd (:get_CachedNativeWindowHandle sender))

  (log/debug "#################### handle-window-opened-event ####################")
  (log/debug "++++ sender: %n" name)
  (log/debug "++++ class: %n" class-name)
  (log/debug "++++ hwnd: %n" hwnd)
  (log/debug "++++ event-id: %d" event-id)

  (if (null? hwnd)
    (log/warning "NULL hwnd received for window opened event")
    (ev/give chan [:uia/window-opened hwnd]))
  S_OK)


(defn- handle-focus-changed-event [sender chan]
  (log/debug "#################### handle-focus-changed-event ####################")
  (ev/give chan :uia/focus-changed)
  S_OK)


(defn uia-manager-get-parent-window [self uia-elem]
  (def {:root root
        :focus-cr focus-cr
        :control-view-walker walker}
    self)

  (var ret nil)
  (var cur-elem uia-elem)
  (var parent
       (try
         (:GetParentElementBuildCache walker cur-elem focus-cr)
         ((err fib)
          # The window or its parent may have vanished
          (log/debug "GetParentElementBuildCache failed: %n\n%s"
                     err
                     (get-stack-trace fib))
          nil)))
  (def root-hwnd (:get_CachedNativeWindowHandle root))

  (while true
    (def hwnd (:get_CachedNativeWindowHandle cur-elem))
    (cond
      (and
        # Has a handle
        (not (nil? hwnd))
        (not (null? hwnd))
        # Is a valid window?
        (is-valid-uia-window? cur-elem))
      (do
        (set ret cur-elem)
        (break))

      (nil? parent)
      (do
        (:Release cur-elem)
        (break))

      (= root-hwnd (:get_CachedNativeWindowHandle parent))
      (do
        (:Release cur-elem)
        (break)))

    (:Release cur-elem)
    (set cur-elem parent)
    (set parent
         (try
           (:GetParentElementBuildCache walker cur-elem focus-cr)
           ((err fib)
            (log/debug "GetParentElementBuildCache failed: %n\n%s"
                       err
                       (get-stack-trace fib))
            nil))))
  ret)


(defn uia-manager-get-focused-window [self]
  (def {:com uia-com
        :focus-cr focus-cr}
    self)

  (def focused
    (try
      (:GetFocusedElementBuildCache uia-com focus-cr)
      ((err fib)
       # This may fail due to e.g. insufficient privileges
       (log/debug "GetFocusedElementBuildCache failed: %n\n%s"
                  err
                  (get-stack-trace fib))
       nil)))
  (if-not focused
    (break nil))

  (uia-manager-get-parent-window self focused))


(defn uia-manager-get-window-info [self hwnd]
  (def {:com uia-com} self)
  (with-uia [cr (:CreateCacheRequest uia-com)]
    (:AddProperty cr UIA_NamePropertyId)
    (:AddProperty cr UIA_ClassNamePropertyId)
    (with-uia [uia-win (try
                         (:ElementFromHandleBuildCache uia-com hwnd cr)
                         ((err fib)
                          (log/debug "ElementFromHandleBuildCache failed: %n\n%s"
                                     err
                                     (get-stack-trace fib))
                          nil))]
      (when uia-win
        {:name (:get_CachedName uia-win)
         :class-name (:get_CachedClassName uia-win)}))))


(defn uia-manager-get-window-bounding-rect [self hwnd]
  (def {:com uia-com} self)
  (with-uia [cr (:CreateCacheRequest uia-com)]
    (:AddProperty cr UIA_BoundingRectanglePropertyId)
    (with-uia [uia-win (try
                         (:ElementFromHandleBuildCache uia-com hwnd cr)
                         ((err fib)
                          (log/debug "ElementFromHandleBuildCache failed: %n\n%s"
                                     err
                                     (get-stack-trace fib))
                          nil))]
      (when uia-win
        (:get_CachedBoundingRectangle uia-win)))))


(defn uia-manager-set-focus-to-window [self hwnd]
  (def {:com uia-com} self)
  (with-uia [uia-win (try
                       (:ElementFromHandle uia-com hwnd)
                       ((err fib)
                        (log/debug "ElementFromHandle failed: %n\n%s"
                                   err
                                   (get-stack-trace fib))
                        nil))]
    (when uia-win
      (:SetFocus uia-win))))


(defn uia-manager-destroy [self]
  (def {:com uia-com
        :root root
        :deinit-fns deinit-fns
        :focus-cr focus-cr
        :control-view-walker control-view-walker}
    self)
  (each df deinit-fns
    (df))
  (:Release control-view-walker)
  (:Release focus-cr)
  (:Release root)
  (:Release uia-com))


(def- uia-manager-proto
  @{:get-parent-window uia-manager-get-parent-window
    :get-focused-window uia-manager-get-focused-window
    :get-window-info uia-manager-get-window-info
    :get-window-bounding-rect uia-manager-get-window-bounding-rect
    :set-focus-to-window uia-manager-set-focus-to-window
    :destroy uia-manager-destroy})


(defn uia-init-event-handlers [uia-com element chan]
  (def window-opened-handler
    # Can't use `with` here, or there will be a "can't marshal alive fiber" error
    (let [cr (:CreateCacheRequest uia-com)]
      (:AddProperty cr UIA_NamePropertyId)
      (:AddProperty cr UIA_ClassNamePropertyId)
      (:AddProperty cr UIA_NativeWindowHandlePropertyId)
      (def handler
        (:AddAutomationEventHandler
           uia-com
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
       uia-com
       nil
       (fn [sender]
         (handle-focus-changed-event sender chan))))

  [(fn []
     (:RemoveAutomationEventHandler
        uia-com
        UIA_Window_WindowOpenedEventId
        element
        window-opened-handler))
   (fn []
     (:RemoveFocusChangedEventHandler
        uia-com
        focus-changed-handler))])


(defn uia-manager []
  (def chan (ev/thread-chan const/DEFAULT-CHAN-LIMIT))

  (def uia-com
    (CoCreateInstance CLSID_CUIAutomation8 nil CLSCTX_INPROC_SERVER IUIAutomation6))
  (:put_AutoSetFocus uia-com false) # To reduce flicker

  (def root
    (with-uia [cr (:CreateCacheRequest uia-com)]
      (:AddProperty cr UIA_NativeWindowHandlePropertyId)
      (:GetRootElementBuildCache uia-com cr)))

  (def deinit-fns
    (uia-init-event-handlers uia-com root chan))

  (def focus-cr (:CreateCacheRequest uia-com))
  (:AddProperty focus-cr UIA_NativeWindowHandlePropertyId)
  (:AddProperty focus-cr UIA_NamePropertyId)
  (:AddProperty focus-cr UIA_ClassNamePropertyId)
  (:AddProperty focus-cr UIA_BoundingRectanglePropertyId)
  (:AddProperty focus-cr UIA_IsTransformPatternAvailablePropertyId)
  (:AddProperty focus-cr UIA_TransformCanMovePropertyId)
  (:AddProperty focus-cr UIA_IsWindowPatternAvailablePropertyId)

  (def control-view-walker (:get_ControlViewWalker uia-com))

  (table/setproto
   @{:com uia-com
     :root root
     :deinit-fns deinit-fns
     :focus-cr focus-cr
     :control-view-walker control-view-walker
     :chan chan}
   uia-manager-proto))
