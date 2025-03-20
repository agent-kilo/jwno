(import jwno/log)

(use jwno/util)
(use jw32/_uiautomation)
(use jw32/_util)


################## vvvv Runs in UI thread vvvv ##################



################## ^^^^ Runs in UI thread ^^^^ ##################

(defn filter-valid-children [child-arr]
  (def res @[])
  (for i 0 (:get_Length child-arr)
    (with-uia [e (:GetElement child-arr i)]
      (when (and (not= FALSE (:get_CachedIsEnabled e))
                 (= FALSE (:get_CachedIsOffscreen e)))
        (:AddRef e)
        (array/push res e))))
  res)


(defn strip-nested-elements [elem uia-man cr]
  (var cur-elem elem)
  (var cur-children nil)
  (var stop false)

  (:AddRef cur-elem)

  (with-uia [c (:create-condition uia-man :true)]
    (while (not stop)
      (with-uia [child-arr (:FindAllBuildCache cur-elem TreeScope_Children c cr)]
        (set cur-children (filter-valid-children child-arr))
        (cond
          (not= FALSE (:GetCachedPropertyValue cur-elem UIA_IsInvokePatternAvailablePropertyId))
          (set stop true)

          (= 0 (length cur-children))
          (set stop true)

          (= 1 (length cur-children))
          (do
            (def ref-count (:Release cur-elem))
            (log/debug "ref-count after Release: %n" ref-count)
            (set cur-elem (first cur-children)))

          true
          (set stop true)))))

  [cur-elem cur-children])


(defn normalize-key-list [key-list]
  (string/ascii-upper key-list))


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


(defn handle-action-invoke [elem]
  :todo)


(defn gradual-ui-hint-cmd [self raw-key-list &opt action]
  (default action :invoke)

  (when (in self :hook-fn)
    (log/debug "aborting nested :gradual-ui-hint command")
    (break))

  (def {:context context
        :show-msg show-msg}
    self)
  (def
    {:hook-manager hook-man
     :key-manager key-man
     :ui-manager ui-man
     :uia-manager uia-man}
    context)

  (with-uia [cr (:create-cache-request uia-man
                                       [UIA_NativeWindowHandlePropertyId
                                        UIA_ControlTypePropertyId
                                        UIA_BoundingRectanglePropertyId
                                        UIA_IsInvokePatternAvailablePropertyId
                                        UIA_IsOffscreenPropertyId
                                        UIA_IsEnabledPropertyId]
                                       [UIA_InvokePatternId])]
    (with-uia [uia-win (:get-focused-window uia-man true cr)]
      (when uia-win
        (def [elem children] (strip-nested-elements uia-win uia-man cr))
        (put self :key-list (normalize-key-list raw-key-list))
        (put self :element-stack @[elem])
        (put self :children-stack @[children])
        (put self :label-stack
           @[(generate-labels (seq [i :range [0 (length children)]] i) (in self :key-list))])
        (put self :current-keys @"")
        (put self :action action)))))


(defn gradual-ui-hint-enable [self]
  (:disable self)

  (def {:context context} self)
  (def {:ui-manager ui-man
        :command-manager command-man}
    context)

  #(def show-msg (:add-custom-message ui-man handle-show-hint-area))
  #(when (< show-msg (int/s64 0))
  #  (error "failed to register show-hint-area message"))
  #(def hide-msg (:add-custom-message ui-man handle-hide-hint-area))
  #(when (< hide-msg (int/s64 0))
  #  (error "failed to register hide-hint-area message"))

  #(put self :show-msg show-msg)
  #(put self :hide-msg hide-msg)

  (:add-command command-man :gradual-ui-hint
     (fn [key-list &opt action]
       (:cmd self key-list action))))


(defn gradual-ui-hint-disable [self]
  (def {:context context
        :show-msg show-msg
        :hide-msg hide-msg}
    self)
  (def {:ui-manager ui-man
        :command-manager command-man}
    context)

  (:remove-command command-man :gradual-ui-hint)

  (when (in self :hook-fn)
    (log/debug "gradual-ui-hint-disable when :gradual-ui-hint command is in progress")
    (:clean-up self))

  (when show-msg
    (:remove-custom-message ui-man show-msg)
    (put self :show-msg nil))
  (when hide-msg
    (:remove-custom-message ui-man hide-msg)
    (put self :hide-msg nil))

  #(def cleanup-msg
  #  (:add-custom-message ui-man handle-cleanup-hint-area))
  #(if (< cleanup-msg (int/s64 0))
  #  (log/warning "failed to clean up hwnd")
  #  # else
  #  (:send-message ui-man cleanup-msg 0 0))
  )


(def gradual-ui-hint-proto
  @{:on-key-pressed :todo

    :cmd gradual-ui-hint-cmd
    :show-hints :todo
    :process-filter-result :todo
    :clean-up :todo
    :enable gradual-ui-hint-enable
    :disable gradual-ui-hint-disable})


(defn gradual-ui-hint [context]
  (table/setproto
   @{:context context
     :action-handlers @{:invoke handle-action-invoke}

     # Default settings
     :colors @{:text 0x505050
               :background 0xf5f5f5
               :border 0x828282
               :shadow 0x828282
               :key 0x000000}
    }
   gradual-ui-hint-proto))
