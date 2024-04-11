(use ../src/uia)


(defn test-with-uia []
  (var release-called-with nil)
  (var body-called-with nil)
  (var ctor-returned nil)
  (def dummy-uia-ctor
    (fn [op]
      (set ctor-returned
           (case op
             :fail
             nil

             :succeed
             @{:Release (fn [x] (set release-called-with x))}

             :error
             (error "dummy error")))
      ctor-returned))

  (with-uia [dummy-uia (dummy-uia-ctor :succeed)]
    (set body-called-with dummy-uia))

  (assert (= release-called-with ctor-returned))
  (assert (= body-called-with ctor-returned))

  (set release-called-with 'place-holder)
  (set body-called-with 'place-holder)

  (with-uia [dummy-uia (dummy-uia-ctor :fail)]
    (set body-called-with dummy-uia))

  (assert (= release-called-with 'place-holder))
  (assert (nil? body-called-with))

  (set release-called-with 'place-holder)
  (set body-called-with 'place-holder)

  (try
    (with-uia [dummy-uia (dummy-uia-ctor :error)]
      (set body-called-with dummy-uia))
    ((err fib)
     (assert (= err "dummy error"))))

  (assert (= release-called-with 'place-holder))
  (assert (= body-called-with 'place-holder))

  (try
    (with-uia [dummy-uia (dummy-uia-ctor :succeed)]
      (set body-called-with dummy-uia)
      (error "dummy error from body"))
    ((err fib)
     (assert (= err "dummy error from body"))))

  (assert (= release-called-with ctor-returned))
  (assert (= body-called-with ctor-returned)))


(defn main [&]
  (test-with-uia))
