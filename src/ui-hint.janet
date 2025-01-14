(use jw32/_winuser)
(use jw32/_uiautomation)

(use ./util)

(import ./log)


# XXX: When there're lots of hints, a shorter prefix can dramatically
#      increase performance
(def HINT-PREFIX "$")
(def HINT-ANCHOR :left)


(defn calc-hint-coord [rect]
  (def center-y (math/round (/ (+ (in rect :top) (in rect :bottom)) 2)))
  [(in rect :left) center-y])


(defn tooltip-id-from-label [label]
  (keyword HINT-PREFIX label))


(defn make-condition [uia-com spec]
  (match spec
    [:property prop-id prop-val]
    (:CreatePropertyCondition uia-com prop-id prop-val)

    [:and & spec-list]
    (do
      (def cond-list (map |(make-condition uia-com $) spec-list))
      (def ret (:CreateAndConditionFromArray uia-com cond-list))
      (each c cond-list
        (:Release c))
      ret)

    [:or & spec-list]
    (do
      (def cond-list (map |(make-condition uia-com $) spec-list))
      (def ret (:CreateOrConditionFromArray uia-com cond-list))
      (each c cond-list
        (:Release c))
      ret)

    [:not spec]
    (do
      (def orig-cond (make-condition uia-com spec))
      (def ret (:CreateNotCondition uia-com orig-cond))
      (:Release orig-cond)
      ret)

    _
    (errorf "unknown condition spec: %n" spec)))


(defn calc-label-len [elem-count key-count]
  (max 1 (math/ceil (/ (math/log elem-count) (math/log key-count)))))


(defn make-label-coro [key-list &opt next-coro]
  (fn []
    (coro
     (each k key-list
       (if next-coro
         (let [c (next-coro)]
           (while (def n (resume c))
             (def buf (buffer/new-filled 1 k))
             (buffer/push buf n)
             (yield buf)))
         (yield (buffer/new-filled 1 k)))))))


(defn make-label-coro-for-label-len [label-len key-list]
  (var coro-ctor nil)
  (for i 0 label-len
    (set coro-ctor (make-label-coro key-list coro-ctor)))
  coro-ctor)


(defn generate-labels [elem-list key-list]
  (def label-len (calc-label-len (length elem-list) (length key-list)))
  (def coro-ctor (make-label-coro-for-label-len label-len key-list))
  (def label-coro (coro-ctor))
  (def labeled @{})
  (each e elem-list
    (put labeled (resume label-coro) e))
  labeled)


(defn filter-hint-labels [current-keys labeled-elems ui-man]
  (def filtered @{})
  (def to-show @[])
  (def to-hide @[])

  (eachp [l e] labeled-elems
    (if (string/has-prefix? current-keys l)
      (let [rect (:get_CachedBoundingRectangle e)]
        (def new-label (slice l (length current-keys)))
        (put filtered l [new-label e])
        (array/push to-show [(tooltip-id-from-label l) new-label ;(calc-hint-coord rect)]))
      (array/push to-hide (tooltip-id-from-label l))))

  (when (< 0 (length to-show))
    (:show-tooltip-group ui-man to-show 0 HINT-ANCHOR))
  (when (< 0 (length to-hide))
    (:hide-tooltip-group ui-man to-hide))

  filtered)


(defn clean-up [labeled-elems hook-fn context]
  (def {:hook-manager hook-man
        :ui-manager ui-man
        :key-manager key-man}
    context)
  (:set-key-mode key-man :command)
  (:remove-hook hook-man :key-pressed hook-fn)
  (def to-hide @[])
  (eachp [l e] labeled-elems
    (:Release e)
    (array/push to-hide (tooltip-id-from-label l)))
  (:hide-tooltip-group ui-man to-hide)
  (:hide-tooltip ui-man :ui-hint))


(defn process-filter-result [current-keys filtered labeled-elems hook-fn context]
  (case (length filtered)
    1
    # Reached the target
    (let [[_short_label target] (in filtered (first (keys filtered)))]
      (:AddRef target)
      # Our message loop may get suspended when invoking certain UI elements,
      # e.g. a button that opens a dialog box or a pop-up menu, so we send
      # the clean up messages before invoking anything.
      (clean-up labeled-elems hook-fn context)
      (if (not= 0 (:GetCachedPropertyValue target UIA_IsInvokePatternAvailablePropertyId))
        (try
          (do
            (:SetFocus target)
            (with-uia [invoke-pat (:GetCachedPatternAs target
                                                       UIA_InvokePatternId
                                                       IUIAutomationInvokePattern)]
              (:Invoke invoke-pat)))
          ((err _fib)
           (log/debug "failed to invoke UI element: %n, name: %n, control type: %n"
                      err
                      (:get_CachedName target)
                      (:get_CachedControlType target))))

        # No invoke pattern, focus it only
        (try
          (:SetFocus target)
          ((err _fib)
           (log/debug "failed to focus UI element: %n, name: %n, control type: %n"
                      err
                      (:get_CachedName target)
                      (:get_CachedControlType target)))))
      (:Release target))

    0
    # The prefix does not exist, clean up so we don't confuse the user
    (clean-up labeled-elems hook-fn context)

    # Other values mean that we still have multiple choices, wait
    # for more input.
    (:show-tooltip (in context :ui-manager)
                   :ui-hint
                   (string/format "Current filter: %s\nMatching elements: %d"
                                  current-keys
                                  (length filtered))
                   nil
                   nil
                   0)))


(defn handle-ui-hint-key [key key-list current-keys labeled-elems hook-fn context]
  (def {:ui-manager ui-man} context)
  (def key-code (in key :key))
  # TOOD: Modifiers?

  (cond
    (find |(= $ key-code) key-list)
    # A valid key is pressed
    (do
      (buffer/push-byte current-keys key-code)
      (def filtered (filter-hint-labels current-keys labeled-elems ui-man))
      (process-filter-result current-keys filtered labeled-elems hook-fn context))

    (or (= key-code VK_BACK)
        (= key-code VK_DELETE))
    (do
      (buffer/popn current-keys 1)
      (def filtered (filter-hint-labels current-keys labeled-elems ui-man))
      (process-filter-result current-keys filtered labeled-elems hook-fn context))

    (or (= key-code VK_ESCAPE)
        (= key-code VK_RETURN))
    (clean-up labeled-elems hook-fn context)))


(defn normalize-key-list [key-list]
  (string/ascii-upper key-list))


(defn cmd-ui-hint [context raw-key-list &opt cond-spec]
  (default cond-spec
    [:or
     [:property UIA_IsKeyboardFocusablePropertyId true]
     [:property UIA_IsInvokePatternAvailablePropertyId true]])

  (def
    {:hook-manager hook-man
     :key-manager key-man
     :ui-manager ui-man
     :uia-manager uia-man}
    context)

  (def uia-com (in uia-man :com))

  (def elem-list @[])

  (with-uia [uia-win (:get-focused-window uia-man)]
    (when uia-win
      # XXX: Always ignore disabled and off-screen elements
      (with-uia [cond (make-condition uia-com [:and
                                               [:property UIA_IsOffscreenPropertyId false]
                                               [:property UIA_IsEnabledPropertyId true]
                                               cond-spec])]
        (with-uia [cr (:CreateCacheRequest uia-com)]
          (:AddProperty cr UIA_NamePropertyId)
          (:AddProperty cr UIA_ControlTypePropertyId)
          (:AddProperty cr UIA_BoundingRectanglePropertyId)
          (:AddProperty cr UIA_IsInvokePatternAvailablePropertyId)
          (:AddPattern cr UIA_InvokePatternId)

          (with-uia [elem-arr (:FindAllBuildCache uia-win TreeScope_Subtree cond cr)]
            (for i 0 (:get_Length elem-arr)
              (array/push elem-list (:GetElement elem-arr i))))))))

  (def elem-count (length elem-list))
  (log/debug "Found %n UI elements" elem-count)

  (when (>= 0 elem-count)
    (:show-tooltip ui-man :ui-hint "No matching UI element.")
    (break))

  (def key-list (normalize-key-list raw-key-list))
  (def labeled-elems
    (generate-labels elem-list key-list))
  (def current-keys @"")

  (var hook-fn nil)
  (set hook-fn
    (:add-hook hook-man :key-pressed
       (fn [key]
         (handle-ui-hint-key key key-list current-keys labeled-elems hook-fn context))))
  (:set-key-mode key-man :raw)

  (def to-show @[])
  (eachp [l e] labeled-elems
    (def rect (:get_CachedBoundingRectangle e))
    (array/push to-show [(tooltip-id-from-label l) l ;(calc-hint-coord rect)]))
  (:show-tooltip-group ui-man to-show 0 HINT-ANCHOR)
  (:show-tooltip ui-man
                 :ui-hint
                 (string/format "Current filter: %s\nMatching elements: %d"
                                current-keys
                                elem-count)
                 nil
                 nil
                 0))
