(use ./util)

(import ./log)


(defn hook-manager-call-hook [self hook-name & args]
  (when (dyn :jwno-no-hooks)
    (log/debug ":jwno-no-hooks set, skipping hooks: %n" hook-name)
    (break))

  (log/debug "#### calling hook: %n" hook-name)

  (def hooks (in self :hooks))
  (def hook-fn-list (in hooks hook-name @[]))
  (each hook-fn hook-fn-list
    (try
      (hook-fn ;args)
      ((err fib)
       (log/error "Hook function failed: %n\n%s"
                  err
                  (get-stack-trace fib))))))


(defn hook-manager-call-filter-hook [self mode hook-name & args]
  (when (and (not= :and mode)
             (not= :or mode))
    (errorf "unknown filter hook mode: %n" mode))

  (def default-res
    (case mode
      :and true
      :or false))

  (when (dyn :jwno-no-hooks)
    (log/debug ":jwno-no-hooks set, skipping hooks: %n" hook-name)
    (break default-res))

  (log/debug "#### calling filter hook in %n mode: %n" mode hook-name)

  (def hooks (in self :hooks))
  (def hook-fn-list (in hooks hook-name @[]))
  (var result default-res)
  (each hook-fn hook-fn-list
    (set result
      (try
        (hook-fn ;args)
        ((err fib)
         (log/error "Hook function failed: %n\n%s"
                    err
                    (get-stack-trace fib))
         default-res)))
    (log/debug "result of %n: %n" hook-fn result)
    # Short circuit
    (case mode
      :and
      (if-not result
        (break))
      :or
      (if result
        (break))))
  result)


(defn hook-manager-add-hook [self hook-name hook-fn]
  (def hooks (in self :hooks))
  # Make a copy of the fn list. If we modified it in-place, when
  # new hook fns get registered in a hook fn under the same name,
  # the new hook fns would get called immediately.
  (def hook-fn-list (array/slice (in hooks hook-name @[])))
  (unless (find |(= $ hook-fn) hook-fn-list)
    (array/push hook-fn-list hook-fn)
    (put hooks hook-name hook-fn-list))
  hook-fn)


(defn hook-manager-add-oneshot-hook [self hook-name hook-fn]
  (def hooks (in self :hooks))
  (def hook-fn-list (in hooks hook-name @[]))
  (var saved-hook-fn nil)
  (def wrapper
    (fn [& args]
      (def ret (hook-fn ;args))
      (log/debug "removing oneshot hook %n from %n" saved-hook-fn hook-name)
      (:remove-hook self hook-name saved-hook-fn)
      ret))
  (set saved-hook-fn (:add-hook self hook-name wrapper))
  saved-hook-fn)


(defn hook-manager-remove-hook [self hook-name hook-fn]
  (def hooks (in self :hooks))
  (def hook-fn-list (in hooks hook-name @[]))
  (put hooks hook-name (filter |(not= $ hook-fn) hook-fn-list)))


(def- hook-manager-proto
  @{:call-hook hook-manager-call-hook
    :call-filter-hook hook-manager-call-filter-hook
    :add-hook hook-manager-add-hook
    :add-oneshot-hook hook-manager-add-oneshot-hook
    :remove-hook hook-manager-remove-hook})


(defn hook-manager []
  (table/setproto
   @{:hooks @{}}
   hook-manager-proto))
