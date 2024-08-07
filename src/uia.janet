(use jw32/_combaseapi)
(use jw32/_uiautomation)
(use jw32/_errhandlingapi)
(use jw32/_util)

(use ./util)

(import ./const)
(import ./log)


(defmacro is-valid-uia-window? [uia-win]
  ~(and (not= 0 (:GetCachedPropertyValue ,uia-win UIA_IsTransformPatternAvailablePropertyId))
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


(defn uia-manager-get-root [self uia-elem]
  (def {:root root
        :focus-cr focus-cr
        :control-view-walker walker}
    self)
  (def root-hwnd (:get_CachedNativeWindowHandle root))

  (def release
    (fn [elem]
      (unless (= elem uia-elem)
        (:Release elem))))

  (def get-parent
    (fn [elem]
      (try
        (:GetParentElementBuildCache walker elem focus-cr)
        ((err fib)
         (log/debug "GetParentElementBuildCache failed: %n\n%s"
                    err
                    (get-stack-trace fib))
         nil))))

  (var cur-elem uia-elem)
  (var parent (get-parent cur-elem))

  (while (and (not= root-hwnd (:get_CachedNativeWindowHandle cur-elem))
              (not (nil? parent)))
    (release cur-elem)
    (set cur-elem parent)
    (set parent (get-parent cur-elem)))

  (if (= root-hwnd (:get_CachedNativeWindowHandle cur-elem))
    cur-elem
    (do
      (release cur-elem)
      nil)))


(defn uia-manager-get-parent-window [self uia-elem]
  (def {:root root
        :focus-cr focus-cr
        :control-view-walker walker}
    self)
  (def root-hwnd (:get_CachedNativeWindowHandle root))

  (def release
    (fn [elem]
      (unless (= elem uia-elem)
        (:Release elem))))

  (def get-parent
    (fn [elem]
      (try
        (:GetParentElementBuildCache walker elem focus-cr)
        ((err fib)
         # The window or its parent may have vanished
         (log/debug "GetParentElementBuildCache failed: %n\n%s"
                    err
                    (get-stack-trace fib))
         nil))))

  (var ret nil)
  (var cur-elem uia-elem)
  (var parent (get-parent cur-elem))

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
        (release cur-elem)
        (break))

      (= root-hwnd (:get_CachedNativeWindowHandle parent))
      (do
        (release cur-elem)
        (break)))

    (release cur-elem)
    (set cur-elem parent)
    (set parent (get-parent cur-elem)))

  ret)


(defn uia-manager-get-focused-window [self]
  (def {:com uia-com
        :focus-cr focus-cr}
    self)

  (with-uia [focused (try
                       (:GetFocusedElementBuildCache uia-com focus-cr)
                       ((err fib)
                        # This may fail due to e.g. insufficient privileges
                        (log/debug "GetFocusedElementBuildCache failed: %n\n%s"
                                   err
                                   (get-stack-trace fib))
                        nil))]
    (if focused
      (let [ret (uia-manager-get-parent-window self focused)]
        (when (= ret focused)
          # So that it won't be freed when returned
          (:AddRef focused))
        ret)
      nil)))


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


(defn- init-event-handlers [uia-com element chan]
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


(defn uia-manager-init-event-handlers [self]
  (when (in self :deinit-fns)
    (error "uiautomation event handlers already initialized"))

  (def {:com uia-com
        :root root
        :chan chan}
    self)
  (def deinit-fns
    (init-event-handlers uia-com root chan))
  (put self :deinit-fns deinit-fns))


(defn uia-manager-destroy [self]
  (def {:com uia-com
        :root root
        :deinit-fns deinit-fns
        :focus-cr focus-cr
        :transform-cr transform-cr
        :control-view-walker control-view-walker}
    self)
  (when deinit-fns
    (each df deinit-fns
      (df)))
  (:Release control-view-walker)
  (:Release focus-cr)
  (:Release transform-cr)
  (:Release root)
  (:Release uia-com))


(def- uia-manager-proto
  @{:get-root uia-manager-get-root
    :get-parent-window uia-manager-get-parent-window
    :get-focused-window uia-manager-get-focused-window
    :get-window-info uia-manager-get-window-info
    :get-window-bounding-rect uia-manager-get-window-bounding-rect
    :set-focus-to-window uia-manager-set-focus-to-window
    :init-event-handlers uia-manager-init-event-handlers
    :destroy uia-manager-destroy})


(defn uia-manager []
  (def chan (ev/thread-chan const/DEFAULT-CHAN-LIMIT))

  (def uia-com
    (CoCreateInstance CLSID_CUIAutomation8 nil CLSCTX_INPROC_SERVER IUIAutomation6))
  (:put_AutoSetFocus uia-com false) # To reduce flicker

  (def root
    (with-uia [cr (:CreateCacheRequest uia-com)]
      (:AddProperty cr UIA_NativeWindowHandlePropertyId)
      (:GetRootElementBuildCache uia-com cr)))

  (def focus-cr
    (let [cr (:CreateCacheRequest uia-com)]
      (:AddProperty cr UIA_NativeWindowHandlePropertyId)
      (:AddProperty cr UIA_NamePropertyId)
      (:AddProperty cr UIA_ClassNamePropertyId)
      (:AddProperty cr UIA_BoundingRectanglePropertyId)
      (:AddProperty cr UIA_IsTransformPatternAvailablePropertyId)
      (:AddProperty cr UIA_TransformCanMovePropertyId)
      (:AddProperty cr UIA_IsWindowPatternAvailablePropertyId)
      cr))

  (def transform-cr
    (let [cr (:CreateCacheRequest uia-com)]
      (:AddPattern cr UIA_TransformPatternId)
      (:AddPattern cr UIA_WindowPatternId)
      (:AddProperty cr UIA_TransformCanMovePropertyId)
      (:AddProperty cr UIA_TransformCanResizePropertyId)
      (:AddProperty cr UIA_WindowWindowVisualStatePropertyId)
      (:AddProperty cr UIA_BoundingRectanglePropertyId)
      cr))

  (def control-view-walker (:get_ControlViewWalker uia-com))

  (table/setproto
   @{:com uia-com
     :root root
     :deinit-fns nil # Initialized in uia-manager-init-event-handlers
     :focus-cr focus-cr
     :transform-cr transform-cr
     :control-view-walker control-view-walker
     :chan chan}
   uia-manager-proto))
