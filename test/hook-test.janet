(use ../src/hook)


(defn test-hook-manager []
  (var hook-called-with nil)

  (def hook-man (hook-manager))
  (def hook-fn
    (fn [& args]
      (set hook-called-with args)))

  (:add-hook hook-man :dummy-hook hook-fn)

  (:call-hook hook-man :dummy-hook)
  (assert (= [] hook-called-with))

  (:call-hook hook-man :dummy-hook 'arg1 'arg2)
  (assert (= ['arg1 'arg2] hook-called-with))

  (:remove-hook hook-man :dummy-hook hook-fn)

  (set hook-called-with nil)
  (:call-hook hook-man :dummy-hook)
  (assert (nil? hook-called-with)))


(defn test-call-filter-hook []

  (def hook-man (hook-manager))

  (var hook-fn1-called-with nil)
  (def hook-fn1
    (fn [& args]
      (set hook-fn1-called-with args)
      true))
  (var hook-fn2-called-with nil)
  (def hook-fn2
    (fn [& args]
      (set hook-fn2-called-with args)
      false))
  (var hook-fn3-called-with nil)
  (def hook-fn3
    (fn [& args]
      (set hook-fn3-called-with args)
      true))

  (:add-hook hook-man :dummy-hook hook-fn1)
  (assert (= true (:call-filter-hook hook-man :dummy-hook 'arg1)))
  (assert (= ['arg1] hook-fn1-called-with))

  (:add-hook hook-man :dummy-hook hook-fn2)
  (assert (= false (:call-filter-hook hook-man :dummy-hook 'other-arg1)))
  (assert (= ['other-arg1] hook-fn1-called-with))
  (assert (= ['other-arg1] hook-fn2-called-with))

  (:add-hook hook-man :dummy-hook hook-fn3)
  (assert (= false (:call-filter-hook hook-man :dummy-hook 'another-arg1)))
  (assert (= ['another-arg1] hook-fn1-called-with))
  (assert (= ['another-arg1] hook-fn2-called-with))
  (assert (nil? hook-fn3-called-with)))


(defn main [&]
  (test-hook-manager)
  (test-call-filter-hook))
