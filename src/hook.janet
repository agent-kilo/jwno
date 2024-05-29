(use ./util)

(import ./log)


(defn hook-manager-call-hook [self hook-name & args]
  (when (dyn :jwno-no-hooks)
    (log/debug ":jwno-no-hooks set, skipping hooks: %n" hook-name)
    (break))

  (def hooks (in self :hooks))
  (def hook-fn-list (in hooks hook-name @[]))
  (each hook-fn hook-fn-list
    (try
      (hook-fn ;args)
      ((err fib)
       (log/error "Hook function failed: %n\n%s"
                  err
                  (get-stack-trace fib))))))


(defn hook-manager-call-filter-hook [self hook-name & args]
  (if (dyn :jwno-no-hooks)
    (log/debug ":jwno-no-hooks set, skipping hooks: %n" hook-name)
    (break true)) # XXX: Always pass the checks when no-hooks

  (def hooks (in self :hooks))
  (def hook-fn-list (in hooks hook-name @[]))
  (var result true)
  (each hook-fn hook-fn-list
    (set result
      (try
        (hook-fn ;args)
        ((err fib)
         (log/error "Hook function failed: %n\n%s"
                    err
                    (get-stack-trace fib))
         # XXX: defaults to true
         true)))
    (log/debug "result of %n: %n" hook-fn result)
    (if-not result
      (break)))
  result)


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
    :call-filter-hook hook-manager-call-filter-hook
    :add-hook hook-manager-add-hook
    :remove-hook hook-manager-remove-hook})


(defn hook-manager []
  (table/setproto
   @{:hooks @{}}
   hook-manager-proto))
