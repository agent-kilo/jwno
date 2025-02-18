(use jw32/_winuser)
(use jw32/_combaseapi)
(use jw32/_uiautomation)
(use jw32/_errhandlingapi)
(use jw32/_util)

(use ./util)

(import ./const)
(import ./log)


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


(defn- handle-desktop-name-changed-event [sender prop-id prop-val chan]
  (log/debug "#################### handle-desktop-name-changed-event ####################")
  (log/debug "++++ prop-id: %n" prop-id)
  (log/debug "++++ prop-val: %n" prop-val)
  (when (= prop-id UIA_NamePropertyId)
    (ev/give chan [:uia/desktop-name-changed prop-val]))
  S_OK)


(defn uia-manager-get-root [self uia-elem]
  (def {:root root
        :focus-cr focus-cr
        :control-view-walker walker}
    self)
  (def root-hwnd (:get_CachedNativeWindowHandle root))

  (:AddRef uia-elem)

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
    (:Release cur-elem)
    (set cur-elem parent)
    (set parent (get-parent cur-elem)))

  (if (= root-hwnd (:get_CachedNativeWindowHandle cur-elem))
    cur-elem
    (do
      (:Release cur-elem)
      nil)))


(defn- get-uia-direct-parent [elem walker cr]
  (try
    (:GetParentElementBuildCache walker elem cr)
    ((err fib)
     # The window or its parent may have vanished
     (log/debug "GetParentElementBuildCache failed: %n\n%s"
                err
                (get-stack-trace fib))
     nil)))


(defn uia-manager-get-parent-window [self uia-elem &opt top-level? cr]
  (default top-level? true)
  (default cr (in self :focus-cr))

  (def {:root root
        :control-view-walker walker}
    self)
  (def root-hwnd (:get_CachedNativeWindowHandle root))

  (:AddRef uia-elem)

  (var ret nil)
  (var cur-elem uia-elem)
  (var parent (get-uia-direct-parent cur-elem walker cr))

  (while true
    (def hwnd (try
                (:get_CachedNativeWindowHandle cur-elem)
                ((err fib)
                 (log/debug "get_CachedNativeWindowHandle failed: %n\n%s"
                            err
                            (get-stack-trace fib))
                 nil)))
    (cond
      (and
        # Has a handle
        (not (nil? hwnd))
        (not (null? hwnd))
        # Is a window control?
        (= UIA_WindowControlTypeId (:GetCachedPropertyValue cur-elem UIA_ControlTypePropertyId))
        # Is Visible?
        (= 0 (:GetCachedPropertyValue cur-elem UIA_IsOffscreenPropertyId))
        # Are we looking for a top-level window?
        (if top-level?
          (= (int/u64 0)
             (band WS_CHILD
                   (signed-to-unsigned-32 (GetWindowLong hwnd GWL_STYLE))))
          true))
      (do
        (set ret cur-elem)
        (break))

      (nil? parent)
      (do
        (:Release cur-elem)
        (break))

      true
      (let [parent-hwnd (try
                          (:get_CachedNativeWindowHandle parent)
                          ((err fib)
                           (log/debug "get_CachedNativeWindowHandle failed: %n\n%s"
                                      err
                                      (get-stack-trace fib))
                           nil))]
        (when (or (nil? parent-hwnd)
                  (= root-hwnd parent-hwnd))
          # cur-elem is a top-level thingy, but not a valid window by our
          # standards. Return it anyway, to let the caller check it and
          # decide what to do.
          (set ret cur-elem)
          (break))))

    (:Release cur-elem)
    (set cur-elem parent)
    (set parent (get-uia-direct-parent cur-elem walker cr)))

  ret)


(defn uia-manager-get-focused-window [self &opt top-level? cr]
  (default top-level? true)
  (default cr (in self :focus-cr))

  (def {:com uia-com} self)

  (with-uia [focused (try
                       (:GetFocusedElementBuildCache uia-com cr)
                       ((err fib)
                        # This may fail due to e.g. insufficient privileges
                        (log/debug "GetFocusedElementBuildCache failed: %n\n%s"
                                   err
                                   (get-stack-trace fib))
                        nil))]
    (if focused
      (:get-parent-window self focused top-level? cr)
      nil)))


(defn uia-manager-get-window-info [self hwnd]
  (def {:com uia-com} self)
  (with-uia [cr (:create-cache-request self
                                       [UIA_NamePropertyId
                                        UIA_ClassNamePropertyId])]
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
  (with-uia [cr (:create-cache-request self [UIA_BoundingRectanglePropertyId])]
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
  (log/debug "setting focus to window: %n" hwnd)
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


(defn uia-manager-create-condition [self spec]
  (def com (in self :com))
  (match spec
    :true
    (:CreateTrueCondition com)

    :false
    (:CreateFalseCondition com)

    [:property prop-id prop-val]
    (:CreatePropertyCondition com prop-id prop-val)

    [:and & spec-list]
    (do
      (def cond-list (map |(:create-condition self $) spec-list))
      (def ret (:CreateAndConditionFromArray com cond-list))
      (each c cond-list
        (:Release c))
      ret)

    [:or & spec-list]
    (do
      (def cond-list (map |(:create-condition self $) spec-list))
      (def ret (:CreateOrConditionFromArray com cond-list))
      (each c cond-list
        (:Release c))
      ret)

    [:not spec]
    (do
      (def orig-cond (:create-condition self spec))
      (def ret (:CreateNotCondition com orig-cond))
      (:Release orig-cond)
      ret)

    _
    (errorf "unknown condition spec: %n" spec)))


(defn uia-manager-create-cache-request [self &opt prop-list pat-list]
  (default prop-list [])
  (default pat-list [])

  (def com (in self :com))
  (def cr (:CreateCacheRequest com))
  (each prop prop-list
    (:AddProperty cr prop))
  (each pat pat-list
    (:AddPattern cr pat))
  cr)


(defn uia-manager-create-tree-walker [self cond-or-spec]
  (def com (in self :com))

  (cond
    (or (keyword? cond-or-spec)
        (indexed? cond-or-spec))
    # it's a spec
    (with-uia [cond (:create-condition self cond-or-spec)]
      # XXX: We shouldn't need (:AddRef cond) right? RIGHT?
      (:CreateTreeWalker com cond))

    (and (table? cond-or-spec)
         (= "IUIAutomationCondition" (in cond-or-spec :__if_name)))
    # it's a condition object
    (:CreateTreeWalker com cond-or-spec)

    true
    (errorf "expected a condition spec or condition object, got %n" cond-or-spec)))


(defn uia-manager-create-control-view-walker [self]
  (:get_ControlViewWalker (in self :com)))


(defn uia-manager-create-content-view-walker [self]
  (:get_ContentViewWalker (in self :com)))


(defn uia-manager-create-raw-view-walker [self]
  (:get_RawViewWalker (in self :com)))


(defn uia-manager-enumerate-children [self elem enum-fn &opt walker? cr?]
  (with-uia [walker (if (nil? walker?)
                      (:create-raw-view-walker self)
                      (do
                        (:AddRef walker?)
                        walker?))]
    (var next-child (:GetFirstChildElementBuildCache walker elem cr?))
    (while next-child
      (with-uia [child next-child]
        (enum-fn child)
        (set next-child (:GetNextSiblingElementBuildCache walker child cr?))))))


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
    (let [cr (:CreateCacheRequest uia-com)]
      (:AddProperty cr UIA_ControlTypePropertyId)
      (def handler
        (:AddFocusChangedEventHandler
           uia-com
           cr
           (fn [sender]
             (handle-focus-changed-event sender chan))))
      (:Release cr)
      handler))

  (def desktop-name-changed-handler
    (:AddPropertyChangedEventHandler
       uia-com
       element
       TreeScope_Element
       nil
       (fn [sender prop-id prop-val]
         (handle-desktop-name-changed-event sender prop-id prop-val chan))
       [UIA_NamePropertyId]))

  [(fn []
     (:RemoveAutomationEventHandler
        uia-com
        UIA_Window_WindowOpenedEventId
        element
        window-opened-handler))
   (fn []
     (:RemoveFocusChangedEventHandler
        uia-com
        focus-changed-handler))
   (fn []
     (:RemovePropertyChangedEventHandler
        uia-com
        element
        desktop-name-changed-handler))])


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
    :create-condition uia-manager-create-condition
    :create-cache-request uia-manager-create-cache-request
    :create-tree-walker uia-manager-create-tree-walker
    :create-control-view-walker uia-manager-create-control-view-walker
    :create-content-view-walker uia-manager-create-content-view-walker
    :create-raw-view-walker uia-manager-create-raw-view-walker
    :enumerate-children uia-manager-enumerate-children
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

  (def defview
    (with-uia [con (:CreatePropertyCondition uia-com UIA_ClassNamePropertyId "SHELLDLL_DefView")]
      (with-uia [walker (:CreateTreeWalker uia-com con)]
        (with-uia [cr (:CreateCacheRequest uia-com)]
          (:AddProperty cr UIA_NativeWindowHandlePropertyId)
          (with-uia [dve (:GetFirstChildElementBuildCache walker root cr)]
            (let [dv-hwnd (:get_CachedNativeWindowHandle dve)
                  dv-path (get-hwnd-path dv-hwnd)]
              # XXX: More strict checks?
              (if (and dv-path
                       (string/has-suffix? "\\explorer.exe" dv-path))
                (do
                  (:AddRef dve)
                  dve)
                (error "failed to get SHELLDLL_DefView window"))))))))

  (def focus-cr
    (let [cr (:CreateCacheRequest uia-com)]
      (:AddProperty cr UIA_NativeWindowHandlePropertyId)
      (:AddProperty cr UIA_NamePropertyId)
      (:AddProperty cr UIA_ClassNamePropertyId)
      (:AddProperty cr UIA_BoundingRectanglePropertyId)
      (:AddProperty cr UIA_IsTransformPatternAvailablePropertyId)
      (:AddProperty cr UIA_TransformCanMovePropertyId)
      (:AddProperty cr UIA_IsWindowPatternAvailablePropertyId)
      (:AddProperty cr UIA_WindowWindowVisualStatePropertyId)
      (:AddProperty cr UIA_IsOffscreenPropertyId)
      (:AddProperty cr UIA_ControlTypePropertyId)
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
     :def-view defview
     :deinit-fns nil # Initialized in uia-manager-init-event-handlers
     :focus-cr focus-cr
     :transform-cr transform-cr
     :control-view-walker control-view-walker
     :chan chan}
   uia-manager-proto))
