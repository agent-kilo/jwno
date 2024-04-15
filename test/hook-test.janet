(use ../src/hook)


(defn test-hook-manager []
  (var hook-called-with nil)

  (def hook-man (hook-manager))
  (def hook-fn
    (fn [& args]
      (set hook-called-with args)))

  (:add-hook hook-man :dummy-hook hook-fn)

  (:call-hook hook-man :dummy-hook)
  (assert (= [:dummy-hook] hook-called-with))

  (:call-hook hook-man :dummy-hook 'arg1 'arg2)
  (assert (= [:dummy-hook 'arg1 'arg2] hook-called-with))

  (:remove-hook hook-man :dummy-hook hook-fn)

  (set hook-called-with nil)
  (:call-hook hook-man :dummy-hook)
  (assert (nil? hook-called-with)))


(defn main [&]
  (test-hook-manager))
