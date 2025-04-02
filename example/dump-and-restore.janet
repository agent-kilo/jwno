#
# This is an experiment on saving and restoring layout info from
# a sub frame tree. We should be able to e.g. save a frame's layout
# to a file, and later restore it to the same frame (or any other
# frames).
#
# It's currently WIP, and not ready for testing yet.
#


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
    (dump-virtual-desktop-container node)

    (errorf "unknown node type: %n" (in node :type))))


(defn restore-window [win dumped]
  (def [dump-type hwnd-num tags] dumped)

  (unless (= :window dump-type)
    (errorf "can not restore dump type to a window: %n" dump-type))
  (unless (= hwnd-num
             (pointer-to-number (in win :hwnd)))
    (error "tree dump data mismatch"))
  (eachp [k v] tags
    (put (in win :tags) k v)))


(defn calc-split-params [children]
  (var direction nil)
  (var last-rect nil)
  (var total-width 0)
  (var total-height 0)

  (def child-widths @[])
  (def child-heights @[])

  # First pass: Calculate total-width, total-height and direction
  (each c children
    (def child-type (first c))

    (case child-type
      :window
      (do
        (set direction nil)
        # out of each loop
        (break))

      :frame
      (do
        (def [_ rect _tags] c)
        (def [width height] (util/rect-size rect))
        (array/push child-widths width)
        (array/push child-heights height)
        (if last-rect
          (cond
            (= (in rect :top)
               (in last-rect :top))
            (do
              (set direction :horizontal)
              (+= total-width width))

            (= (in rect :left)
               (in last-rect :left))
            (do
              (set direction :vertical)
              (+= total-height height)))
          # else
          (do
            (set total-width width)
            (set total-height height)))
        (set last-rect rect))

      (errorf "unknown child node type: %n" child-type)))

  (unless direction
    # early return
    (break nil))

  # Second pass: Calculate actual ratios
  (def ratios
    (case direction
      :horizontal
       (map |(/ $ total-width) child-widths)

       :vertical
       (map |(/ $ total-height) child-heights)))

  [direction ratios])


(defn restore-frame [fr dumped &opt win-map?]
  (def [dump-type rect tags children] dumped)

  (def win-map
    (if win-map?
      win-map?
      (let [all-windows (:get-all-windows fr)
            new-map @{}]
        (each w all-windows
          (put new-map (pointer-to-number (in w :hwnd)) w))
        new-map)))

  (array/clear (in fr :children))
  (put fr :current-child nil)

  (if-let [split-params (calc-split-params children)]
    (do
      # The frame is not empty, and the children are sub-frames
      (def [direction ratios] split-params)
      (:split fr direction (length children) ratios)
      (map (fn [sub-fr d]
             (restore-frame sub-fr d win-map))
           (in fr :children)
           children))
    # else
    (unless (empty? children)
      # The frame is not empty, and the children are windows
      (each c children
        (def [_ hwnd-num _] c)
        (when-let [win (in win-map hwnd-num)]
          (restore-window win c)
          (:add-child fr win)))))

  (eachp [k v] tags
    (put (in fr :tags) k v)))


(defn restore-node [node dumped]
  (when (not (tuple? dumped))
    (errorf "invalid tree dump: %n" dumped))

  (case (in node :type)
    :window
    (restore-window node dumped)

    :frame
    (restore-frame node dumped)
    )
  )
