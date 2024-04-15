(import ./log)


(defn hook-manager-call-hook [self hook-name & args]
  (def hooks (in self :hooks))
  (def hook-fn-list (in hooks hook-name @[]))
  (each hook-fn hook-fn-list
    (try
      (hook-fn hook-name ;args)
      ((err fib)
       (log/error "Hook function failed: %n" err)))))


(defn hook-manager-add-hook [self hook-name hook-fn]
  (def hooks (in self :hooks))
  (def hook-fn-list (in hooks hook-name @[]))
  (array/push hook-fn-list hook-fn)
  (put hooks hook-name hook-fn-list))


(defn hook-manager-remove-hook [self hook-name hook-fn]
  (def hooks (in self :hooks))
  (def hook-fn-list (in hooks hook-name @[]))
  (put hooks hook-name (filter |(not= $ hook-fn) hook-fn-list)))


(def- hook-manager-proto
  @{:call-hook hook-manager-call-hook
    :add-hook hook-manager-add-hook
    :remove-hook hook-manager-remove-hook})


(defn hook-manager []
  (table/setproto
   @{:hooks @{}}
   hook-manager-proto))
