(import jwno/util)
(import jwno/log)


(def pointer-peg
  (peg/compile
   ~{:left-delim "<"
     :right-delim ">"
     :hex-digit (choice (range "09") (range "af") (range "AF"))
     :hex-addr (capture (sequence "0x" (some :hex-digit)))
     :main (sequence :left-delim :s* "pointer" :s+ :hex-addr :s* :right-delim)}))


(defn pointer-to-number [pointer]
  (def pointer-str (string pointer))
  (def matched (peg/match pointer-peg pointer-str))
  (parse ;matched))


# Forward declarations
(var dump-node nil)


(defn dump-tag-value [x]
  (def x-type (type x))
  (cond
    (or (= x-type :array)
        (= x-type :tuple))
    (tuple/slice (map |(dump-tag-value $) x))

    (or (= x-type :table)
        (= x-type :struct))
    (do
      (def new-tab @{})
      (eachp [k v] x
             (def new-k (dump-tag-value k))
             (def new-v (dump-tag-value v))
             (put new-tab new-k new-v))
      (table/to-struct new-tab))

    (or (= x-type :core/s64)
        (= x-type :core/u64))
    [x-type (string x)]

    (abstract? x)
    (errorf "non-trivial type: %n" x)

    (or (= x-type :function)
        (= x-type :cfunction)
        (= x-type :fiber))
    (errorf "non-trivial type: %n" x)

    (= x-type :pointer)
    (errorf "non-trivial type: %n" x)

    true
    x))


(defn dump-tags [orig-tags]
  (def tags @{})
  (each k (keys orig-tags)
    (if (or (keyword? k)
            (symbol? k))
      (do
        (def v
          (try
            (dump-tag-value (in orig-tags k))
            ((err fib)
             (if (and (string? err)
                      (string/has-prefix? "non-trivial type:" err))
               (do
                 (log/warning "non-trivial value in tag %n: %n\n%s"
                              k
                              (in orig-tags k)
                              (util/get-stack-trace fib))
                 nil)
               # else
               (do
                 (log/error "failed to dump tag value: %n\n%s"
                            err
                            (util/get-stack-trace fib))
                 (error err))))))
        (put tags k v))

      # else
      (log/warning "ignoring tag: %n" k)))
  (table/to-struct tags))


(defn dump-window [win]
  [:window
   (pointer-to-number (in win :hwnd))
   (dump-tags (in win :tags))])


(defn dump-frame [fr]
  [:frame
   (in fr :rect)
   (dump-tags (in fr :tags))
   (tuple/slice (map |(dump-node $) (in fr :children)))])


(defn dump-layout [lo]
  [:layout
   (in lo :id)
   (in lo :name)
   (tuple/slice (map |(dump-node $) (in lo :children)))])


(defn dump-virtual-desktop-container [vdc]
  [:vdc
   (tuple/slice (map |(dump-node $) (in vdc :children)))])


(varfn dump-node [node]
  (case (in node :type)
    :window
    (dump-window node)

    :frame
    (dump-frame node)

    :layout
    (dump-layout node)

    :virtual-desktop-container
    (dump-virtual-desktop-container node)))
