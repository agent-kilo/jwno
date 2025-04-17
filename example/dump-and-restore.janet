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
    (log/debug "restoring dump data for 0x%x to %n" hwnd-num (in win :hwnd)))
  (eachp [k v] tags
    (put (in win :tags) k v))

  # Return an empty table, to make it consistent with other
  # restore-* functions
  @{})


(defn win-list-to-map [win-list]
  (cond
    (table? win-list)
    win-list

    (indexed? win-list)
    (let [win-map @{}]
      (each w win-list
        (put win-map (pointer-to-number (in w :hwnd)) w))
      win-map)

    true
    (errorf "unsupported window list: %n" win-list)))


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


(defn restore-frame [fr dumped &opt win-list]
  (def [dump-type rect tags children] dumped)
  (unless (= :frame dump-type)
    (errorf "can not restore dump type to a frame: %n" dump-type))

  # XXX: win-list is also re-used in recursion to pass down win-map,
  # so it can be a table, instead of a list/array.
  (default win-list (:get-all-windows fr))
  (def win-map (win-list-to-map win-list))

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
      (def lo (:get-layout fr))
      (def wm (when lo (:get-window-manager lo)))
      
      (def maybe-restore
        (if lo
          # attached frame, need to check its virtual desktop
          (fn [hwnd-num win c]
            (when-let [vd-info (:get-hwnd-virtual-desktop wm (in win :hwnd))]
              (when (= (in lo :id) (in vd-info :id))
                (put win-map hwnd-num nil)
                (restore-window win c)
                (:add-child fr win))))
          # else, detached frame
          (fn [hwnd-num win c]
            (put win-map hwnd-num nil)
            (restore-window win c)
            (:add-child fr win))))

      (each c children
        (def [_ hwnd-num _] c)
        (when-let [win (in win-map hwnd-num)]
          (maybe-restore hwnd-num win c)))))

  (eachp [k v] tags
    (put (in fr :tags) k v))

  win-map)


(defn find-closest-top-frame [rect all-top-frames]
  (def [center-x center-y] (util/rect-center rect))
  (var min-dist math/int-max)
  (var found nil)
  (var found-idx nil)
  (eachp [idx fr] all-top-frames
    (def mon-rect (get-in fr [:monitor :work-area]))
    (def [mon-center-x mon-center-y] (util/rect-center mon-rect))
    (def dx (- center-x mon-center-x))
    (def dy (- center-y mon-center-y))
    (def dist (+ (* dx dx) (* dy dy)))
    (when (< dist min-dist)
      (set found fr)
      (set found-idx idx)
      (set min-dist dist)))
  [found found-idx min-dist])


(defn restore-layout [lo dumped &opt win-list]
  (def [dump-type lo-id lo-name children] dumped)
  (unless (= :layout dump-type)
    (errorf "can not restore dump type to a layout: %n" dump-type))

  (default win-list (:get-all-windows lo))
  (def win-map (win-list-to-map win-list))

  (def all-top-frames (array/slice (in lo :children)))

  (each c children
    (def [_ rect _ _] c)
    (def [found-fr found-idx dist]
      (find-closest-top-frame rect all-top-frames))
    (if found-fr
      (do
        (log/debug "found top frame to restore, rect = %n, frame rect = %n, dist = %n"
                   rect (in found-fr :rect) dist)
        (array/remove all-top-frames found-idx)
        (restore-frame found-fr c win-map))
      # else
      (log/debug "top frame for dump data not found, rect = %n" rect)))

  (log/debug "top frames not restored: %n" (map |(in $ :rect) all-top-frames))

  win-map)


(defn find-layout [id all-layouts]
  (var found nil)
  (var found-idx nil)
  (eachp [idx lo] all-layouts
    (when (= id (in lo :id))
      (set found lo)
      (set found-idx idx)
      (break)))
  [found found-idx])


(defn restore-virtual-desktop-container [vdc dumped &opt win-list]
  (def [dump-type children] dumped)
  (unless (= :vdc dump-type)
    (errorf "can not restore dump type to a virtual desktop container: %n" dump-type))

  (default win-list (:get-all-windows vdc))
  (def win-map (win-list-to-map win-list))

  (def all-layouts (array/slice (in vdc :children)))

  (each c children
    (def [_ lo-id _ _] c)
    (def [found-lo found-idx] (find-layout lo-id all-layouts))
    (if found-lo
      (do
        (log/debug "found layout to restore, id = %n" lo-id)
        (array/remove all-layouts found-idx)
        (restore-layout found-lo c win-map))
      # else
      (log/debug "layout for dump data not found, lo-id = %n" lo-id)))

  (log/debug "layouts not restored: %n" (map |(in $ :id) all-layouts))

  win-map)


(defn restore-node [node dumped &opt win-list]
  (when (not (tuple? dumped))
    (errorf "invalid tree dump: %n" dumped))

  (case (in node :type)
    :window
    (restore-window node dumped)

    :frame
    (restore-frame node dumped win-list)

    :layout
    (restore-layout node dumped win-list)

    :virtual-desktop-container
    (restore-virtual-desktop-container node dumped win-list)))
