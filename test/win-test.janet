(use ../src/win)

(import ../src/const)


(defn build-dummy-frame-tree [spec &opt monitor]
  (cond
    (keyword? spec)
    (window spec)

    (struct? spec)
    (let [fr (frame spec)]
      (put fr :monitor monitor)
      fr)
    

    (tuple? spec)
    (let [rect (first spec)
          proto (in spec 1)
          child-specs (slice spec 2)]
      (def fr (frame rect))
      (put fr :monitor monitor)
      (when proto
        (table/setproto fr proto))
      (each s child-specs
        (def child (build-dummy-frame-tree s))
        (:add-child fr child))
      fr)))


(defn test-frame-constructor []
  (var dummy-frame (frame {:top 10 :left 10 :bottom 110 :right 110}))
  (assert (= (get-in dummy-frame [:rect :top]) 10))
  (assert (= (get-in dummy-frame [:rect :left]) 10))
  (assert (= (get-in dummy-frame [:rect :bottom]) 110))
  (assert (= (get-in dummy-frame [:rect :right]) 110))
  (assert (nil? (in dummy-frame :parent)))
  (assert (deep= (in dummy-frame :children) @[])))


(defn test-frame-add-child []
  (var dummy-frame (frame {:top 10 :left 10 :bottom 110 :right 110}))
  (var dummy-frame2 (frame {:top 10 :left 110 :bottom 110 :right 210}))

  (def dummy-sub-frame1 (frame {:top 10 :left 10 :bottom 110 :right 60}))
  (def dummy-sub-frame2 (frame {:top 10 :left 60 :bottom 110 :right 110}))

  (:add-child dummy-frame dummy-sub-frame1)
  (assert (= (in dummy-sub-frame1 :parent) dummy-frame))
  (assert (= (length (in dummy-frame :children)) 1))

  (:add-child dummy-frame dummy-sub-frame2)
  (assert (= (in dummy-sub-frame2 :parent) dummy-frame))
  (assert (= (length (in dummy-frame :children)) 2))

  (:add-child dummy-frame2 dummy-sub-frame2)
  (assert (= (in dummy-sub-frame2 :parent) dummy-frame2))
  (assert (= (length (in dummy-frame2 :children)) 1))
  (assert (= (get-in dummy-frame2 [:children 0]) dummy-sub-frame2))
  (assert (= (length (in dummy-frame :children)) 1))
  (assert (= (get-in dummy-frame [:children 0]) dummy-sub-frame1))

  (def dummy-window1 (window :dummy-hwnd))
  (def dummy-window2 (window :dummy-hwnd))

  (try
    (:add-child dummy-frame dummy-window1)
    ((err fib)
     (assert (= err "cannot mix different types of children"))))

  (set dummy-frame (frame {:top 10 :left 10 :bottom 110 :right 110}))

  (:add-child dummy-frame dummy-window1)
  (assert (= (in dummy-window1 :parent) dummy-frame))
  (assert (= (length (in dummy-frame :children)) 1))

  (:add-child dummy-frame dummy-window2)
  (assert (= (in dummy-window2 :parent) dummy-frame))
  (assert (= (length (in dummy-frame :children)) 2))

  (try
    (:add-child dummy-frame dummy-sub-frame1)
    ((err fib)
     (assert (= err "cannot mix different types of children")))))


(defn test-frame-split []
  (def dummy-monitor {:dpi [const/USER-DEFAULT-SCREEN-DPI const/USER-DEFAULT-SCREEN-DPI]})
  (def rect {:top 10 :left 10 :bottom 110 :right 110})

  (var dummy-frame (build-dummy-frame-tree rect dummy-monitor))
  (var err-raised false)

  (try
    (:split dummy-frame :horizontal 1)
    ((err fib)
     (assert (= err "invalid number of sub-frames"))
     (set err-raised true)))
  (assert err-raised)

  (:split dummy-frame :horizontal)
  (assert (= (length (in dummy-frame :children)) 2))

  (assert (= (get-in dummy-frame [:children 0 :type]) :frame))
  (assert (= (get-in dummy-frame [:children 0 :rect :left]) 10))
  (assert (= (get-in dummy-frame [:children 0 :rect :top]) 10))
  (assert (= (get-in dummy-frame [:children 0 :rect :right]) 60))
  (assert (= (get-in dummy-frame [:children 0 :rect :bottom]) 110))

  (assert (= (get-in dummy-frame [:children 1 :type]) :frame))
  (assert (= (get-in dummy-frame [:children 1 :rect :left]) 60))
  (assert (= (get-in dummy-frame [:children 1 :rect :top]) 10))
  (assert (= (get-in dummy-frame [:children 1 :rect :right]) 110))
  (assert (= (get-in dummy-frame [:children 1 :rect :bottom]) 110))

  (set err-raised false)
  (try
    (:split dummy-frame :vertical)
    ((err fib)
     (assert (= err "frame is already split"))
     (set err-raised true)))
  (assert err-raised)

  (set dummy-frame (build-dummy-frame-tree rect dummy-monitor))

  (:split dummy-frame :vertical)
  (assert (= (length (in dummy-frame :children)) 2))

  (assert (= (get-in dummy-frame [:children 0 :type]) :frame))
  (assert (= (get-in dummy-frame [:children 0 :rect :left]) 10))
  (assert (= (get-in dummy-frame [:children 0 :rect :top]) 10))
  (assert (= (get-in dummy-frame [:children 0 :rect :right]) 110))
  (assert (= (get-in dummy-frame [:children 0 :rect :bottom]) 60))

  (assert (= (get-in dummy-frame [:children 1 :type]) :frame))
  (assert (= (get-in dummy-frame [:children 1 :rect :left]) 10))
  (assert (= (get-in dummy-frame [:children 1 :rect :top]) 60))
  (assert (= (get-in dummy-frame [:children 1 :rect :right]) 110))
  (assert (= (get-in dummy-frame [:children 1 :rect :bottom]) 110))

  (set dummy-frame (build-dummy-frame-tree rect dummy-monitor))

  (:split dummy-frame :horizontal 3)
  (assert (= (length (in dummy-frame :children)) 3))

  (assert (= (get-in dummy-frame [:children 0 :type]) :frame))
  (assert (= (get-in dummy-frame [:children 0 :rect :left]) 10))
  (assert (= (get-in dummy-frame [:children 0 :rect :top]) 10))
  (assert (= (get-in dummy-frame [:children 0 :rect :right]) 43))
  (assert (= (get-in dummy-frame [:children 0 :rect :bottom]) 110))

  (assert (= (get-in dummy-frame [:children 1 :type]) :frame))
  (assert (= (get-in dummy-frame [:children 1 :rect :left]) 43))
  (assert (= (get-in dummy-frame [:children 1 :rect :top]) 10))
  (assert (= (get-in dummy-frame [:children 1 :rect :right]) 76))
  (assert (= (get-in dummy-frame [:children 1 :rect :bottom]) 110))

  (assert (= (get-in dummy-frame [:children 2 :type]) :frame))
  (assert (= (get-in dummy-frame [:children 2 :rect :left]) 76))
  (assert (= (get-in dummy-frame [:children 2 :rect :top]) 10))
  (assert (= (get-in dummy-frame [:children 2 :rect :right]) 110))
  (assert (= (get-in dummy-frame [:children 2 :rect :bottom]) 110))

  (set dummy-frame (build-dummy-frame-tree rect dummy-monitor))

  (:split dummy-frame :horizontal 3 [0.5 0.3])
  (assert (= (length (in dummy-frame :children)) 3))

  (assert (= (get-in dummy-frame [:children 0 :type]) :frame))
  (assert (= (get-in dummy-frame [:children 0 :rect :left]) 10))
  (assert (= (get-in dummy-frame [:children 0 :rect :top]) 10))
  (assert (= (get-in dummy-frame [:children 0 :rect :right]) 60))
  (assert (= (get-in dummy-frame [:children 0 :rect :bottom]) 110))

  (assert (= (get-in dummy-frame [:children 1 :type]) :frame))
  (assert (= (get-in dummy-frame [:children 1 :rect :left]) 60))
  (assert (= (get-in dummy-frame [:children 1 :rect :top]) 10))
  (assert (= (get-in dummy-frame [:children 1 :rect :right]) 90))
  (assert (= (get-in dummy-frame [:children 1 :rect :bottom]) 110))

  (assert (= (get-in dummy-frame [:children 2 :type]) :frame))
  (assert (= (get-in dummy-frame [:children 2 :rect :left]) 90))
  (assert (= (get-in dummy-frame [:children 2 :rect :top]) 10))
  (assert (= (get-in dummy-frame [:children 2 :rect :right]) 110))
  (assert (= (get-in dummy-frame [:children 2 :rect :bottom]) 110))

  (set dummy-frame (build-dummy-frame-tree rect dummy-monitor))

  (:split dummy-frame :horizontal 2 [0.555 0.445])
  (assert (= (length (in dummy-frame :children)) 2))

  (assert (= (get-in dummy-frame [:children 0 :type]) :frame))
  (assert (= (get-in dummy-frame [:children 0 :rect :left]) 10))
  (assert (= (get-in dummy-frame [:children 0 :rect :top]) 10))
  (assert (= (get-in dummy-frame [:children 0 :rect :right]) 65))
  (assert (= (get-in dummy-frame [:children 0 :rect :bottom]) 110))

  (assert (= (get-in dummy-frame [:children 1 :type]) :frame))
  (assert (= (get-in dummy-frame [:children 1 :rect :left]) 65))
  (assert (= (get-in dummy-frame [:children 1 :rect :top]) 10))
  (assert (= (get-in dummy-frame [:children 1 :rect :right]) 110))
  (assert (= (get-in dummy-frame [:children 1 :rect :bottom]) 110))

  (set dummy-frame
       (build-dummy-frame-tree
        [rect
         nil
           :dummy-hwnd1
           :dummy-hwnd2
           :dummy-hwnd3]
        dummy-monitor))
  (def dummy-window1 (get-in dummy-frame [:children 0]))
  (:activate dummy-window1)

  (:split dummy-frame :horizontal)

  (assert (= (get-in dummy-frame [:children 0 :type]) :frame))
  (assert (= (get-in dummy-frame [:children 1 :type]) :frame))

  (assert (= (length (get-in dummy-frame [:children 0 :children])) 3))
  (assert (= (get-in dummy-frame [:children 0 :children 0 :type]) :window))
  (assert (= (get-in dummy-frame [:children 0 :children 1 :type]) :window))
  (assert (= (get-in dummy-frame [:children 0 :children 2 :type]) :window))

  (assert (= (length (get-in dummy-frame [:children 1 :children])) 0))

  (set dummy-frame
       (build-dummy-frame-tree
        {:top 10 :left 10 :bottom 11 :right 11}
        dummy-monitor))

  (set err-raised false)
  (try
    (:split dummy-frame :horizontal 2 [0.5])
    ((err fib)
     (assert (= err "cannot create zero-width frames"))
     (set err-raised true)))
  (assert err-raised)
  (assert (= (length (in dummy-frame :children)) 0))

  (set err-raised false)
  (try
    (:split dummy-frame :vertical 2 [0.5])
    ((err fib)
     (assert (= err "cannot create zero-height frames"))
     (set err-raised true)))
  (assert err-raised)
  (assert (= (length (in dummy-frame :children)) 0))

  (set dummy-frame (build-dummy-frame-tree rect dummy-monitor))

  (put (in dummy-frame :tags) :padding 9)
  (:split dummy-frame :horizontal)
  (assert (= 2 (length (in dummy-frame :children))))
  (let [rect0 (get-in dummy-frame [:children 0 :rect])
        rect1 (get-in dummy-frame [:children 1 :rect])]
    (assert (= 19 (in rect0 :top)))
    (assert (= 19 (in rect0 :left)))
    (assert (= 101 (in rect0 :bottom)))
    (assert (= 60 (in rect0 :right)))

    (assert (= 19 (in rect1 :top)))
    (assert (= 60 (in rect1 :left)))
    (assert (= 101 (in rect1 :bottom)))
    (assert (= 101 (in rect1 :right))))

  (set dummy-frame (build-dummy-frame-tree rect dummy-monitor))

  (put (in dummy-frame :tags) :paddings {:top 9 :left 8 :bottom 7 :right 6})
  (:split dummy-frame :vertical)
  (assert (= 2 (length (in dummy-frame :children))))
  (let [rect0 (get-in dummy-frame [:children 0 :rect])
        rect1 (get-in dummy-frame [:children 1 :rect])]
    (assert (= 19 (in rect0 :top)))
    (assert (= 18 (in rect0 :left)))
    (assert (= 61 (in rect0 :bottom)))
    (assert (= 104 (in rect0 :right)))

    (assert (= 61 (in rect1 :top)))
    (assert (= 18 (in rect1 :left)))
    (assert (= 103 (in rect1 :bottom)))
    (assert (= 104 (in rect1 :right))))

  #### Absolute split sizes ####

  (set dummy-frame (build-dummy-frame-tree rect dummy-monitor))

  (:split dummy-frame :horizontal 2 [29])
  (assert (= 2 (length (in dummy-frame :children))))
  (let [rect0 (get-in dummy-frame [:children 0 :rect])
        rect1 (get-in dummy-frame [:children 1 :rect])]
    (assert (= 10 (in rect0 :top)))
    (assert (= 10 (in rect0 :left)))
    (assert (= 110 (in rect0 :bottom)))
    (assert (= 39 (in rect0 :right)))

    (assert (= 10 (in rect1 :top)))
    (assert (= 39 (in rect1 :left)))
    (assert (= 110 (in rect1 :bottom)))
    (assert (= 110 (in rect1 :right))))

  #### Mixed split sizes ####

  (set dummy-frame (build-dummy-frame-tree rect dummy-monitor))

  (:split dummy-frame :horizontal 3 [0.4 29])
  (assert (= 3 (length (in dummy-frame :children))))
  (let [rect0 (get-in dummy-frame [:children 0 :rect])
        rect1 (get-in dummy-frame [:children 1 :rect])
        rect2 (get-in dummy-frame [:children 2 :rect])]
    (assert (= 10 (in rect0 :top)))
    (assert (= 10 (in rect0 :left)))
    (assert (= 110 (in rect0 :bottom)))
    (assert (= 50 (in rect0 :right)))

    (assert (= 10 (in rect1 :top)))
    (assert (= 50 (in rect1 :left)))
    (assert (= 110 (in rect1 :bottom)))
    (assert (= 79 (in rect1 :right)))

    (assert (= 10 (in rect2 :top)))
    (assert (= 79 (in rect2 :left)))
    (assert (= 110 (in rect2 :bottom)))
    (assert (= 110 (in rect2 :right))))

  #### split sizes that are too big ####

  (set dummy-frame (build-dummy-frame-tree rect dummy-monitor))

  (set err-raised false)
  (try
    (:split dummy-frame :horizontal 3 [0.4 60])
    ((err fib)
     (assert (= err "cannot create zero-width frames"))
     (set err-raised true)))
  (assert err-raised)
  (assert (= (length (in dummy-frame :children)) 0)))


(defn test-frame-close []
  (def dummy-monitor {:dpi [const/USER-DEFAULT-SCREEN-DPI const/USER-DEFAULT-SCREEN-DPI]})

  (def rect {:top 10 :left 10 :bottom 110 :right 130})

  #
  # dummy-frame -+- dummy-sub-frame1 -- dummy-window1
  #              |
  #              +- dummy-sub-frame2 -- dummy-window2
  #
  (def rect1 {:top 10 :left 10 :bottom 110 :right 70})
  (def rect2 {:top 10 :left 70 :bottom 110 :right 130})

  (def dummy-frame
    (build-dummy-frame-tree
     [rect
      horizontal-frame-proto
        [rect1
         nil
           :dummy-hwnd1]
        [rect2
         nil
           :dummy-hwnd2]]
     dummy-monitor))

  (def dummy-sub-frame1 (get-in dummy-frame [:children 0]))
  (def dummy-sub-frame2 (get-in dummy-frame [:children 1]))
  (def dummy-window1 (get-in dummy-sub-frame1 [:children 0]))
  (def dummy-window2 (get-in dummy-sub-frame2 [:children 0]))

  (:activate dummy-window1)
  (:close dummy-sub-frame1)

  (def all-children (tuple/slice (in dummy-frame :children)))
  (assert (= 2 (length all-children)))
  (assert (or (= all-children [dummy-window1 dummy-window2])
              (= all-children [dummy-window2 dummy-window1])))
  (assert (= dummy-window1 (in dummy-frame :current-child)))

  #
  # dummy-frame -+- dummy-sub-frame1 -- dummy-window1
  #              |
  #              +- dummy-sub-frame2 -- dummy-window2
  #
  (def rect1 {:top 10 :left 10 :bottom 110 :right 70})
  (def rect2 {:top 10 :left 70 :bottom 110 :right 130})

  (def dummy-frame
    (build-dummy-frame-tree
     [rect
      horizontal-frame-proto
        [rect1
         nil
           :dummy-hwnd1]
        [rect2
         nil
           :dummy-hwnd2]]
     dummy-monitor))

  (def dummy-sub-frame1 (get-in dummy-frame [:children 0]))
  (def dummy-sub-frame2 (get-in dummy-frame [:children 1]))
  (def dummy-window1 (get-in dummy-sub-frame1 [:children 0]))
  (def dummy-window2 (get-in dummy-sub-frame2 [:children 0]))

  (:activate dummy-window1)
  (:close dummy-sub-frame2)

  (def all-children (tuple/slice (in dummy-frame :children)))
  (assert (= 2 (length all-children)))
  (assert (or (= all-children [dummy-window1 dummy-window2])
              (= all-children [dummy-window2 dummy-window1])))
  (assert (= dummy-window1 (in dummy-frame :current-child)))

  #
  # dummy-frame -+- dummy-sub-frame1 -- dummy-window1
  #              |
  #              +- dummy-sub-frame2 -- dummy-window2
  #              |
  #              +- dummy-sub-frame3 -- dummy-window3
  #
  (def rect1 {:top 10 :left 10 :bottom 110 :right 50})
  (def rect2 {:top 10 :left 50 :bottom 110 :right 90})
  (def rect3 {:top 10 :left 90 :bottom 110 :right 130})

  (def dummy-frame
    (build-dummy-frame-tree
     [rect
      horizontal-frame-proto
        [rect1
         nil
           :dummy-hwnd1]
        [rect2
         nil
           :dummy-hwnd2]
        [rect3
         nil
           :dummy-hwnd3]]
     dummy-monitor))

  (def dummy-sub-frame1 (get-in dummy-frame [:children 0]))
  (def dummy-sub-frame2 (get-in dummy-frame [:children 1]))
  (def dummy-sub-frame3 (get-in dummy-frame [:children 2]))
  (def dummy-window1 (get-in dummy-sub-frame1 [:children 0]))
  (def dummy-window2 (get-in dummy-sub-frame2 [:children 0]))
  (def dummy-window3 (get-in dummy-sub-frame3 [:children 0]))

  (:activate dummy-window2)
  (:close dummy-sub-frame2)

  (def all-children (tuple/slice (in dummy-sub-frame3 :children)))
  (assert (= 2 (length all-children)))
  (assert (or (= all-children [dummy-window2 dummy-window3])
              (= all-children [dummy-window3 dummy-window2])))
  (assert (= dummy-sub-frame3 (in dummy-frame :current-child)))
  (assert (= dummy-window2 (:get-current-window dummy-frame))))


(defn test-frame-close-with-unconstrained-parent []
  (def dummy-monitor {:dpi [const/USER-DEFAULT-SCREEN-DPI const/USER-DEFAULT-SCREEN-DPI]})

  (def rect {:top 10 :left 10 :bottom 110 :right 130})
  (def vp-rect {:top 10 :left 35 :bottom 110 :right 85})

  #
  # dummy-frame -+- dummy-sub-frame1 -- dummy-window1
  #              |
  #              +- dummy-sub-frame2 -- dummy-window2
  #              |
  #              +- dummy-sub-frame3 -- dummy-window3
  #
  (def rect1 {:top 10 :left 10 :bottom 110 :right 50})
  (def rect2 {:top 10 :left 50 :bottom 110 :right 90})
  (def rect3 {:top 10 :left 90 :bottom 110 :right 130})

  (def dummy-frame
    (build-dummy-frame-tree
     [rect
      horizontal-frame-proto
        [rect1
         nil
           :dummy-hwnd1]
        [rect2
         nil
           :dummy-hwnd2]
        [rect3
         nil
           :dummy-hwnd3]]
     dummy-monitor))

  (put dummy-frame :viewport vp-rect)

  (def dummy-sub-frame1 (get-in dummy-frame [:children 0]))
  (def dummy-sub-frame2 (get-in dummy-frame [:children 1]))
  (def dummy-sub-frame3 (get-in dummy-frame [:children 2]))
  (def dummy-window1 (get-in dummy-sub-frame1 [:children 0]))
  (def dummy-window2 (get-in dummy-sub-frame2 [:children 0]))
  (def dummy-window3 (get-in dummy-sub-frame3 [:children 0]))

  (:activate dummy-window2)
  (:close dummy-sub-frame2)

  (assert (= 10 (get-in dummy-frame [:rect :left])))
  (assert (= 10 (get-in dummy-frame [:rect :top])))
  (assert (= 90 (get-in dummy-frame [:rect :right])))
  (assert (= 110 (get-in dummy-frame [:rect :bottom])))

  (assert (= 10 (get-in dummy-sub-frame1 [:rect :left])))
  (assert (= 10 (get-in dummy-sub-frame1 [:rect :top])))
  (assert (= 50 (get-in dummy-sub-frame1 [:rect :right])))
  (assert (= 110 (get-in dummy-sub-frame1 [:rect :bottom])))

  (assert (= 50 (get-in dummy-sub-frame3 [:rect :left])))
  (assert (= 10 (get-in dummy-sub-frame3 [:rect :top])))
  (assert (= 90 (get-in dummy-sub-frame3 [:rect :right])))
  (assert (= 110 (get-in dummy-sub-frame3 [:rect :bottom]))))


(defn test-frame-close-with-unconstrained-sibling []
  (def dummy-monitor {:dpi [const/USER-DEFAULT-SCREEN-DPI const/USER-DEFAULT-SCREEN-DPI]})

  (def rect {:top 10 :left 10 :bottom 110 :right 130})

  #
  # dummy-frame -+- dummy-sub-frame1
  #              |
  #              +- dummy-sub-frame2 -+- dummy-sub-frame21
  #                                   |
  #                                   +- dummy-sub-frame22
  #
  (def rect1 {:top 10 :left 10 :bottom 110 :right 70})
  (def vp-rect2 {:top 10 :left 70 :bottom 110 :right 130})
  (def rect2 {:top 0 :left 70 :bottom 120 :right 130})
  (def rect21 {:top 0 :left 70 :bottom 60 :right 130})
  (def rect22 {:top 60 :left 70 :bottom 120 :right 130})

  (def dummy-frame
    (build-dummy-frame-tree
     [rect
      horizontal-frame-proto
        [rect1 nil]
        [rect2
         vertical-frame-proto
           [rect21 nil]
           [rect22 nil]]]
     dummy-monitor))

  (def dummy-sub-frame1 (get-in dummy-frame [:children 0]))
  (def dummy-sub-frame2 (get-in dummy-frame [:children 1]))
  (def dummy-sub-frame21 (get-in dummy-sub-frame2 [:children 0]))
  (def dummy-sub-frame22 (get-in dummy-sub-frame2 [:children 1]))

  (put dummy-sub-frame2 :viewport vp-rect2)

  (:close dummy-sub-frame1)

  (assert (= 2 (length (in dummy-frame :children))))
  (assert (and (= dummy-sub-frame21 (get-in dummy-frame [:children 0]))
               (= dummy-sub-frame22 (get-in dummy-frame [:children 1]))))

  (assert (= 10 (get-in dummy-frame [:rect :left])))
  (assert (= 0 (get-in dummy-frame [:rect :top])))
  (assert (= 130 (get-in dummy-frame [:rect :right])))
  (assert (= 120 (get-in dummy-frame [:rect :bottom])))

  (assert (= 10 (get-in dummy-frame [:viewport :left])))
  (assert (= 10 (get-in dummy-frame [:viewport :top])))
  (assert (= 130 (get-in dummy-frame [:viewport :right])))
  (assert (= 110 (get-in dummy-frame [:viewport :bottom])))

  (assert (= 10 (get-in dummy-sub-frame21 [:rect :left])))
  (assert (= 0 (get-in dummy-sub-frame21 [:rect :top])))
  (assert (= 130 (get-in dummy-sub-frame21 [:rect :right])))
  (assert (= 60 (get-in dummy-sub-frame21 [:rect :bottom])))

  (assert (= 10 (get-in dummy-sub-frame22 [:rect :left])))
  (assert (= 60 (get-in dummy-sub-frame22 [:rect :top])))
  (assert (= 130 (get-in dummy-sub-frame22 [:rect :right])))
  (assert (= 120 (get-in dummy-sub-frame22 [:rect :bottom]))))


(defn test-frame-close-with-unconstrained-parent-and-sibling []
  (def dummy-monitor {:dpi [const/USER-DEFAULT-SCREEN-DPI const/USER-DEFAULT-SCREEN-DPI]})

  (def rect {:top 10 :left 10 :bottom 110 :right 130})
  (def vp-rect {:top 10 :left 10 :bottom 110 :right 110})

  #
  # dummy-frame -+- dummy-sub-frame1
  #              |
  #              +- dummy-sub-frame2 -+- dummy-sub-frame21
  #                                   |
  #                                   +- dummy-sub-frame22
  #
  (def rect1 {:top 10 :left 10 :bottom 110 :right 70})
  (def vp-rect2 {:top 10 :left 70 :bottom 110 :right 130})
  (def rect2 {:top 0 :left 70 :bottom 120 :right 130})
  (def rect21 {:top 0 :left 70 :bottom 60 :right 130})
  (def rect22 {:top 60 :left 70 :bottom 120 :right 130})

  (def dummy-frame
    (build-dummy-frame-tree
     [rect
      horizontal-frame-proto
        [rect1 nil]
        [rect2
         vertical-frame-proto
           [rect21 nil]
           [rect22 nil]]]
     dummy-monitor))

  (def dummy-sub-frame1 (get-in dummy-frame [:children 0]))
  (def dummy-sub-frame2 (get-in dummy-frame [:children 1]))
  (def dummy-sub-frame21 (get-in dummy-sub-frame2 [:children 0]))
  (def dummy-sub-frame22 (get-in dummy-sub-frame2 [:children 1]))

  (put dummy-frame :viewport vp-rect)
  (put dummy-sub-frame2 :viewport vp-rect2)

  (:close dummy-sub-frame1)

  (assert (= 2 (length (in dummy-frame :children))))
  (assert (and (= dummy-sub-frame21 (get-in dummy-frame [:children 0]))
               (= dummy-sub-frame22 (get-in dummy-frame [:children 1]))))

  (assert (= 10 (get-in dummy-frame [:rect :left])))
  (assert (= 0 (get-in dummy-frame [:rect :top])))
  (assert (= 110 (get-in dummy-frame [:rect :right])))
  (assert (= 120 (get-in dummy-frame [:rect :bottom])))

  (assert (= 10 (get-in dummy-frame [:viewport :left])))
  (assert (= 10 (get-in dummy-frame [:viewport :top])))
  (assert (= 110 (get-in dummy-frame [:viewport :right])))
  (assert (= 110 (get-in dummy-frame [:viewport :bottom])))

  (assert (= 10 (get-in dummy-sub-frame21 [:rect :left])))
  (assert (= 0 (get-in dummy-sub-frame21 [:rect :top])))
  (assert (= 110 (get-in dummy-sub-frame21 [:rect :right])))
  (assert (= 60 (get-in dummy-sub-frame21 [:rect :bottom])))

  (assert (= 10 (get-in dummy-sub-frame22 [:rect :left])))
  (assert (= 60 (get-in dummy-sub-frame22 [:rect :top])))
  (assert (= 110 (get-in dummy-sub-frame22 [:rect :right])))
  (assert (= 120 (get-in dummy-sub-frame22 [:rect :bottom]))))


(defn test-frame-insert-sub-frame []
  (def dummy-monitor {:dpi [const/USER-DEFAULT-SCREEN-DPI const/USER-DEFAULT-SCREEN-DPI]})
  (def rect {:top 10 :left 10 :bottom 110 :right 110})

  (var dummy-frame (build-dummy-frame-tree rect dummy-monitor))

  (:insert-sub-frame dummy-frame 0 nil :horizontal)
  (assert (= (length (in dummy-frame :children)) 2))

  (:insert-sub-frame dummy-frame 0)
  (assert (= (length (in dummy-frame :children)) 3))

  (assert (= (get-in dummy-frame [:children 0 :type]) :frame))
  (assert (= (get-in dummy-frame [:children 0 :rect :left]) 10))
  (assert (= (get-in dummy-frame [:children 0 :rect :top]) 10))
  (assert (= (get-in dummy-frame [:children 0 :rect :right]) 43))
  (assert (= (get-in dummy-frame [:children 0 :rect :bottom]) 110))

  (assert (= (get-in dummy-frame [:children 1 :type]) :frame))
  (assert (= (get-in dummy-frame [:children 1 :rect :left]) 43))
  (assert (= (get-in dummy-frame [:children 1 :rect :top]) 10))
  (assert (= (get-in dummy-frame [:children 1 :rect :right]) 76))
  (assert (= (get-in dummy-frame [:children 1 :rect :bottom]) 110))

  (assert (= (get-in dummy-frame [:children 2 :type]) :frame))
  (assert (= (get-in dummy-frame [:children 2 :rect :left]) 76))
  (assert (= (get-in dummy-frame [:children 2 :rect :top]) 10))
  (assert (= (get-in dummy-frame [:children 2 :rect :right]) 110))
  (assert (= (get-in dummy-frame [:children 2 :rect :bottom]) 110))

  (:insert-sub-frame dummy-frame 1 0.5)
  (assert (= (length (in dummy-frame :children)) 4))

  (assert (= (get-in dummy-frame [:children 0 :type]) :frame))
  (assert (= (get-in dummy-frame [:children 0 :rect :left]) 10))
  (assert (= (get-in dummy-frame [:children 0 :rect :top]) 10))
  (assert (= (get-in dummy-frame [:children 0 :rect :right]) 26))
  (assert (= (get-in dummy-frame [:children 0 :rect :bottom]) 110))

  (assert (= (get-in dummy-frame [:children 1 :type]) :frame))
  (assert (= (get-in dummy-frame [:children 1 :rect :left]) 26))
  (assert (= (get-in dummy-frame [:children 1 :rect :top]) 10))
  (assert (= (get-in dummy-frame [:children 1 :rect :right]) 76))
  (assert (= (get-in dummy-frame [:children 1 :rect :bottom]) 110))

  (assert (= (get-in dummy-frame [:children 2 :type]) :frame))
  (assert (= (get-in dummy-frame [:children 2 :rect :left]) 76))
  (assert (= (get-in dummy-frame [:children 2 :rect :top]) 10))
  (assert (= (get-in dummy-frame [:children 2 :rect :right]) 92))
  (assert (= (get-in dummy-frame [:children 2 :rect :bottom]) 110))

  (assert (= (get-in dummy-frame [:children 3 :type]) :frame))
  (assert (= (get-in dummy-frame [:children 3 :rect :left]) 92))
  (assert (= (get-in dummy-frame [:children 3 :rect :top]) 10))
  (assert (= (get-in dummy-frame [:children 3 :rect :right]) 110))
  (assert (= (get-in dummy-frame [:children 3 :rect :bottom]) 110))

  (:insert-sub-frame dummy-frame -1)
  (assert (= (length (in dummy-frame :children)) 5))

  (assert (= (get-in dummy-frame [:children 4 :type]) :frame))
  # XXX: The last frame's width has accumulated rounding error
  (assert (= (get-in dummy-frame [:children 4 :rect :left]) 88))
  (assert (= (get-in dummy-frame [:children 4 :rect :top]) 10))
  (assert (= (get-in dummy-frame [:children 4 :rect :right]) 110))
  (assert (= (get-in dummy-frame [:children 4 :rect :bottom]) 110))

  #### Absolute size values ####
  (:insert-sub-frame dummy-frame 0 9)
  (assert (= (length (in dummy-frame :children)) 6))

  (assert (= (get-in dummy-frame [:children 0 :rect :left]) 10))
  (assert (= (get-in dummy-frame [:children 0 :rect :top]) 10))
  (assert (= (get-in dummy-frame [:children 0 :rect :right]) 19))
  (assert (= (get-in dummy-frame [:children 0 :rect :bottom]) 110))

  (assert (= (get-in dummy-frame [:children 5 :rect :right]) 110))
  (assert (= (get-in dummy-frame [:children 5 :rect :bottom]) 110))

  #### Absolute sizes that are too large ####
  (var err-raised false)
  (try
    (:insert-sub-frame dummy-frame -1 100)
    ((err fib)
     (assert (= err "cannot create zero-width frames"))
     (set err-raised true)))
  (assert err-raised)
  (assert (= (length (in dummy-frame :children)) 6))
  (def total-width
    (+ ;(map |(- (get-in $ [:rect :right]) (get-in $ [:rect :left]))
             (in dummy-frame :children))))
  (assert (= 100 total-width))

  (set dummy-frame
       (build-dummy-frame-tree
        [rect
         nil
           :dummy-hwnd1
           :dummy-hwnd2]
        dummy-monitor))
  (def dummy-window1 (get-in dummy-frame [:children 0]))
  (def dummy-window2 (get-in dummy-frame [:children 1]))

  (:insert-sub-frame dummy-frame 0 nil :vertical)
  (assert (empty? (get-in dummy-frame [:children 0 :children])))
  (def win-list (get-in dummy-frame [:children 1 :children]))
  (assert (= 2 (length win-list)))
  (assert (= dummy-window1 (in win-list 0)))
  (assert (= dummy-window2 (in win-list 1)))

  (:insert-sub-frame dummy-frame 2 nil :vertical)
  (assert (empty? (get-in dummy-frame [:children 0 :children])))
  (assert (empty? (get-in dummy-frame [:children 2 :children])))
  (def win-list (get-in dummy-frame [:children 1 :children]))
  (assert (= 2 (length win-list)))
  (assert (= dummy-window1 (in win-list 0)))
  (assert (= dummy-window2 (in win-list 1)))

  (def dummy-sub-frame1 (get-in dummy-frame [:children 0]))
  (def dummy-sub-frame2 (get-in dummy-frame [:children 1]))
  (def dummy-sub-frame3 (get-in dummy-frame [:children 2]))

  # Inserting with a direction that's different from the frame's current direction
  (:insert-sub-frame dummy-frame 0 nil :horizontal)
  (assert (= 2 (length (in dummy-frame :children))))
  (assert (= 0 (length (get-in dummy-frame [:children 0 :children]))))
  (assert (= 3 (length (get-in dummy-frame [:children 1 :children]))))
  (assert (= dummy-sub-frame1 (get-in dummy-frame [:children 1 :children 0])))
  (assert (= dummy-sub-frame2 (get-in dummy-frame [:children 1 :children 1])))
  (assert (= dummy-sub-frame3 (get-in dummy-frame [:children 1 :children 2])))
  (assert (= {:left 10 :top 10 :right 60 :bottom 110}
             (get-in dummy-frame [:children 0 :rect])))
  (assert (= {:left 60 :top 10 :right 110 :bottom 43}
             (get-in dummy-frame [:children 1 :children 0 :rect])))
  (assert (= {:left 60 :top 43 :right 110 :bottom 76}
             (get-in dummy-frame [:children 1 :children 1 :rect])))
  (assert (= {:left 60 :top 76 :right 110 :bottom 110}
             (get-in dummy-frame [:children 1 :children 2 :rect]))))


(defn test-tree-node-activate []
  (def rect {:top 10 :left 10 :bottom 110 :right 110})
  (def rect1 {:top 10 :left 10 :bottom 110 :right 60})
  (def rect2 {:top 10 :left 60 :bottom 110 :right 110})

  #
  # dummy-frame -+- dummy-sub-frame1 -- dummy-window1
  #              |
  #              +- dummy-sub-frame2 -- dummy-window2
  #
  (def dummy-frame
    (build-dummy-frame-tree
     [rect
      horizontal-frame-proto
        [rect1
         nil
           :dummy-hwnd1]
        [rect2
         nil
           :dummy-hwnd2]]))

  (def dummy-sub-frame1 (get-in dummy-frame [:children 0]))
  (def dummy-sub-frame2 (get-in dummy-frame [:children 1]))
  (def dummy-window1 (get-in dummy-sub-frame1 [:children 0]))
  (def dummy-window2 (get-in dummy-sub-frame2 [:children 0]))

  (:activate dummy-window1)
  (assert (= (in dummy-sub-frame1 :current-child) dummy-window1))
  (assert (= (in dummy-frame :current-child) dummy-sub-frame1))

  (:activate dummy-sub-frame2)
  (assert (= (in dummy-frame :current-child) dummy-sub-frame2))
  (assert (= (in dummy-sub-frame2 :current-child) dummy-window2))
  (assert (= (in dummy-sub-frame1 :current-child) dummy-window1)))


(defn test-frame-find-hwnd []
  #
  # dummy-frame -+- dummy-sub-frame1 -- dummy-window1
  #              |
  #              +- dummy-sub-frame2 -- dummy-window2
  #
  (def dummy-frame
    (build-dummy-frame-tree
     [{:top 10 :left 10 :bottom 110 :right 110}
      horizontal-frame-proto
        [{:top 10 :left 10 :bottom 110 :right 60}
         nil
           :dummy-hwnd1]
        [{:top 10 :left 60 :bottom 110 :right 110}
         nil
           :dummy-hwnd2]]))

  (def dummy-window1 (get-in dummy-frame [:children 0 :children 0]))
  (var dummy-window2 (get-in dummy-frame [:children 1 :children 0]))

  (:activate dummy-window1)

  (assert (= dummy-window1 (:find-hwnd dummy-frame :dummy-hwnd1)))
  (assert (= dummy-window2 (:find-hwnd dummy-frame :dummy-hwnd2)))
  (assert (nil? (:find-hwnd dummy-frame :dummy-hwnd3))))


(defn test-frame-get-current-frame []
  #
  # dummy-frame -+- dummy-sub-frame1 -- dummy-window1
  #              |
  #              +- dummy-sub-frame2 -- dummy-window2
  #
  (var dummy-frame (frame {:top 10 :left 10 :bottom 110 :right 110}))
  (assert (= dummy-frame (:get-current-frame dummy-frame)))

  (set dummy-frame
    (build-dummy-frame-tree
     [{:top 10 :left 10 :bottom 110 :right 110}
      horizontal-frame-proto
        [{:top 10 :left 10 :bottom 110 :right 60}
         nil
           :dummy-hwnd1]
        [{:top 10 :left 60 :bottom 110 :right 110}
         nil
           :dummy-hwnd2]]))

  (def dummy-sub-frame1 (get-in dummy-frame [:children 0]))
  (def dummy-sub-frame2 (get-in dummy-frame [:children 1]))

  (assert (= dummy-sub-frame1 (:get-current-frame dummy-frame)))

  (put dummy-frame :current-child nil)
  (var error-raised nil)
  (try
    (:get-current-frame dummy-frame)
    ((err fib)
     (assert (= err "inconsistent states for frame tree"))
     (set error-raised true)))
  (assert (= true error-raised))

  (def dummy-window1 (get-in dummy-frame [:children 0 :children 0]))
  (:activate dummy-window1)
  (assert (= dummy-sub-frame1 (:get-current-frame dummy-frame)))

  (def dummy-window2 (get-in dummy-frame [:children 1 :children 0]))
  (:activate dummy-window2)
  (assert (= dummy-sub-frame2 (:get-current-frame dummy-frame))))


(defn test-frame-transform []
  (def dummy-monitor {:dpi [const/USER-DEFAULT-SCREEN-DPI const/USER-DEFAULT-SCREEN-DPI]})
  (def rect {:top 10 :left 10 :bottom 110 :right 110})
  (def rect1 {:top 10 :left 10 :bottom 110 :right 60})
  (def rect2 {:top 10 :left 60 :bottom 110 :right 110})

  #
  # dummy-frame -+- dummy-sub-frame1 -- dummy-window1
  #              |
  #              +- dummy-sub-frame2 -- dummy-window2
  #
  (var dummy-frame
       (build-dummy-frame-tree
        [rect
         horizontal-frame-proto
           [rect1
            nil
              :dummy-hwnd1]
           [rect2
            nil
              :dummy-hwnd2]]
        dummy-monitor))

  (var dummy-sub-frame1 (get-in dummy-frame [:children 0]))
  (var dummy-sub-frame2 (get-in dummy-frame [:children 1]))

  (var resized-frames
    (:transform dummy-frame rect @[]))

  (assert (empty? resized-frames))

  (set resized-frames
    (:transform dummy-frame {:top 10 :left 20 :bottom 110 :right 100} @[]))

  (assert (= 2 (length resized-frames)))
  (assert (= dummy-sub-frame1 (in resized-frames 0)))
  (assert (= dummy-sub-frame2 (in resized-frames 1)))

  (assert (= 10 (get-in dummy-frame [:rect :top])))
  (assert (= 20 (get-in dummy-frame [:rect :left])))
  (assert (= 110 (get-in dummy-frame [:rect :bottom])))
  (assert (= 100 (get-in dummy-frame [:rect :right])))

  (assert (= 10 (get-in dummy-sub-frame1 [:rect :top])))
  (assert (= 20 (get-in dummy-sub-frame1 [:rect :left])))
  (assert (= 110 (get-in dummy-sub-frame1 [:rect :bottom])))
  (assert (= 60 (get-in dummy-sub-frame1 [:rect :right])))

  (assert (= 10 (get-in dummy-sub-frame2 [:rect :top])))
  (assert (= 60 (get-in dummy-sub-frame2 [:rect :left])))
  (assert (= 110 (get-in dummy-sub-frame2 [:rect :bottom])))
  (assert (= 100 (get-in dummy-sub-frame2 [:rect :right])))

  (set dummy-frame
       (build-dummy-frame-tree
        [rect
         horizontal-frame-proto
           {:top 19 :left 19 :bottom 101 :right 60}
           {:top 19 :left 60 :bottom 101 :right 101}]
        dummy-monitor))
  (put (in dummy-frame :tags) :padding 9)

  (set dummy-sub-frame1 (get-in dummy-frame [:children 0]))
  (set dummy-sub-frame2 (get-in dummy-frame [:children 1]))

  (:transform dummy-frame {:top 13 :left 20 :bottom 107 :right 100})

  (assert (= 22 (get-in dummy-sub-frame1 [:rect :top])))
  (assert (= 29 (get-in dummy-sub-frame1 [:rect :left])))
  (assert (= 98 (get-in dummy-sub-frame1 [:rect :bottom])))
  (assert (= 60 (get-in dummy-sub-frame1 [:rect :right])))

  (assert (= 22 (get-in dummy-sub-frame2 [:rect :top])))
  (assert (= 60 (get-in dummy-sub-frame2 [:rect :left])))
  (assert (= 98 (get-in dummy-sub-frame2 [:rect :bottom])))
  (assert (= 91 (get-in dummy-sub-frame2 [:rect :right])))

  (set dummy-frame
       (build-dummy-frame-tree
        [rect
         vertical-frame-proto
           {:top 19 :left 18 :bottom 61 :right 104}
           {:top 61 :left 18 :bottom 103 :right 104}]
        dummy-monitor))
  (put (in dummy-frame :tags) :paddings {:top 9 :left 8 :bottom 7 :right 6})

  (set dummy-sub-frame1 (get-in dummy-frame [:children 0]))
  (set dummy-sub-frame2 (get-in dummy-frame [:children 1]))

  (:transform dummy-frame {:top 13 :left 20 :bottom 107 :right 100})

  (assert (= 22 (get-in dummy-sub-frame1 [:rect :top])))
  (assert (= 28 (get-in dummy-sub-frame1 [:rect :left])))
  (assert (= 61 (get-in dummy-sub-frame1 [:rect :bottom])))
  (assert (= 94 (get-in dummy-sub-frame1 [:rect :right])))

  (assert (= 61 (get-in dummy-sub-frame2 [:rect :top])))
  (assert (= 28 (get-in dummy-sub-frame2 [:rect :left])))
  (assert (= 100 (get-in dummy-sub-frame2 [:rect :bottom])))
  (assert (= 94 (get-in dummy-sub-frame2 [:rect :right])))

  #
  # dummy-frame -+- dummy-sub-frame1 -- dummy-window1
  #              |
  #              +- dummy-sub-frame2 -- dummy-window2
  #
  (set dummy-frame
       (build-dummy-frame-tree
        [rect
         horizontal-frame-proto
         [rect1
          nil
          :dummy-hwnd1]
         [rect2
          nil
          :dummy-hwnd2]]
        dummy-monitor))
  (set dummy-sub-frame1 (get-in dummy-frame [:children 0]))
  (set dummy-sub-frame2 (get-in dummy-frame [:children 1]))

  (put (in dummy-frame :tags) :padding 9)
  # Re-calculate children geometries after updating :padding
  (:transform dummy-frame (in dummy-frame :rect))

  (assert (= 19  (get-in dummy-sub-frame1 [:rect :top])))
  (assert (= 19  (get-in dummy-sub-frame1 [:rect :left])))
  (assert (= 101 (get-in dummy-sub-frame1 [:rect :bottom])))
  (assert (= 60  (get-in dummy-sub-frame1 [:rect :right])))

  (assert (= 19  (get-in dummy-sub-frame2 [:rect :top])))
  (assert (= 60  (get-in dummy-sub-frame2 [:rect :left])))
  (assert (= 101 (get-in dummy-sub-frame2 [:rect :bottom])))
  (assert (= 101 (get-in dummy-sub-frame2 [:rect :right])))

  (def dummy-rect    {:top -10 :left 10 :bottom 90 :right 70})
  (def dummy-vp-rect {:top 10  :left 10 :bottom 70 :right 70})
  (def dummy-rect1   {:top -10 :left 10 :bottom 40 :right 70})
  (def dummy-rect2   {:top 40  :left 10 :bottom 90 :right 70})
  #
  # dummy-frame -+- dummy-sub-frame1 -- dummy-window1
  #              |
  #              +- dummy-sub-frame2 -- dummy-window2
  #
  (set dummy-frame
       (build-dummy-frame-tree
        [dummy-rect
         vertical-frame-proto
         [dummy-rect1
          nil
          :dummy-hwnd1]
         [dummy-rect2
          nil
          :dummy-hwnd2]]
        dummy-monitor))
  (set dummy-sub-frame1 (get-in dummy-frame [:children 0]))
  (set dummy-sub-frame2 (get-in dummy-frame [:children 1]))

  (put dummy-frame :viewport dummy-vp-rect)

  # Transforming with viewport
  (def transform-rect {:left 20 :top 20 :right 140 :bottom 140})
  (:transform dummy-frame transform-rect)

  (assert (= transform-rect (in dummy-frame :viewport)))
  (assert (= 20 (get-in dummy-frame [:rect :left])))
  (assert (= -20 (get-in dummy-frame [:rect :top])))
  (assert (= 140 (get-in dummy-frame [:rect :right])))
  (assert (= 180 (get-in dummy-frame [:rect :bottom])))

  (assert (= 20 (get-in dummy-sub-frame1 [:rect :left])))
  (assert (= -20 (get-in dummy-sub-frame1 [:rect :top])))
  (assert (= 140 (get-in dummy-sub-frame1 [:rect :right])))
  (assert (= 80 (get-in dummy-sub-frame1 [:rect :bottom])))

  (assert (= 20 (get-in dummy-sub-frame2 [:rect :left])))
  (assert (= 80 (get-in dummy-sub-frame2 [:rect :top])))
  (assert (= 140 (get-in dummy-sub-frame2 [:rect :right])))
  (assert (= 180 (get-in dummy-sub-frame2 [:rect :bottom]))))


(defn test-frame-balance []
  (def dummy-monitor {:dpi [const/USER-DEFAULT-SCREEN-DPI const/USER-DEFAULT-SCREEN-DPI]})
  (def rect {:top 10 :left 10 :bottom 110 :right 110})

  #
  # dummy-frame -+- dummy-sub-frame1
  #              |
  #              +- dummy-sub-frame2
  #
  (var dummy-frame
       (build-dummy-frame-tree
        [rect
         horizontal-frame-proto
           {:top 10 :left 10 :bottom 110 :right 60}
           {:top 10 :left 60 :bottom 110 :right 110}]
        dummy-monitor))

  (var dummy-sub-frame1 (get-in dummy-frame [:children 0]))
  (var dummy-sub-frame2 (get-in dummy-frame [:children 1]))

  (def resized-frames (:balance dummy-frame true @[]))

  (assert (empty? resized-frames))

  (assert (= 10 (get-in dummy-sub-frame1 [:rect :top])))
  (assert (= 10 (get-in dummy-sub-frame1 [:rect :left])))
  (assert (= 110 (get-in dummy-sub-frame1 [:rect :bottom])))
  (assert (= 60 (get-in dummy-sub-frame1 [:rect :right])))

  (assert (= 10 (get-in dummy-sub-frame2 [:rect :top])))
  (assert (= 60 (get-in dummy-sub-frame2 [:rect :left])))
  (assert (= 110 (get-in dummy-sub-frame2 [:rect :bottom])))
  (assert (= 110 (get-in dummy-sub-frame2 [:rect :right])))

  (set dummy-frame
       (build-dummy-frame-tree
        [rect
         horizontal-frame-proto
           {:top 10 :left 10 :bottom 110 :right 50}
           {:top 10 :left 50 :bottom 110 :right 110}]
        dummy-monitor))

  (set dummy-sub-frame1 (get-in dummy-frame [:children 0]))
  (set dummy-sub-frame2 (get-in dummy-frame [:children 1]))

  (def resized-frames (:balance dummy-frame true @[]))

  (assert (= 2 (length resized-frames)))
  (assert (= dummy-sub-frame1 (in resized-frames 0)))
  (assert (= dummy-sub-frame2 (in resized-frames 1)))

  (assert (= 10 (get-in dummy-sub-frame1 [:rect :top])))
  (assert (= 10 (get-in dummy-sub-frame1 [:rect :left])))
  (assert (= 110 (get-in dummy-sub-frame1 [:rect :bottom])))
  (assert (= 60 (get-in dummy-sub-frame1 [:rect :right])))

  (assert (= 10 (get-in dummy-sub-frame2 [:rect :top])))
  (assert (= 60 (get-in dummy-sub-frame2 [:rect :left])))
  (assert (= 110 (get-in dummy-sub-frame2 [:rect :bottom])))
  (assert (= 110 (get-in dummy-sub-frame2 [:rect :right])))

  #
  # dummy-frame -+- dummy-sub-frame1 -+- dummy-sub-frame-3
  #              |                    |
  #              +- dummy-sub-frame2  +- dummy-sub-frame-4
  #
  (set dummy-frame
       (build-dummy-frame-tree
        [rect
         horizontal-frame-proto
           [{:top 10 :left 10 :bottom 110 :right 50}
            horizontal-frame-proto
              {:top 10 :left 10 :bottom 110 :right 30}
              {:top 10 :left 30 :bottom 110 :right 50}]
           {:top 10 :left 50 :bottom 110 :right 110}]
        dummy-monitor))

  (set dummy-sub-frame1 (get-in dummy-frame [:children 0]))
  (set dummy-sub-frame2 (get-in dummy-frame [:children 1]))
  (var dummy-sub-frame3 (get-in dummy-sub-frame1 [:children 0]))
  (var dummy-sub-frame4 (get-in dummy-sub-frame1 [:children 1]))

  (def resized-frames (:balance dummy-frame true @[]))

  (assert (= 3 (length resized-frames)))
  (assert (= dummy-sub-frame3 (in resized-frames 0)))
  (assert (= dummy-sub-frame4 (in resized-frames 1)))
  (assert (= dummy-sub-frame2 (in resized-frames 2)))

  (assert (= 10 (get-in dummy-sub-frame2 [:rect :top])))
  (assert (= 60 (get-in dummy-sub-frame2 [:rect :left])))
  (assert (= 110 (get-in dummy-sub-frame2 [:rect :bottom])))
  (assert (= 110 (get-in dummy-sub-frame2 [:rect :right])))

  (assert (= 10 (get-in dummy-sub-frame3 [:rect :top])))
  (assert (= 10 (get-in dummy-sub-frame3 [:rect :left])))
  (assert (= 110 (get-in dummy-sub-frame3 [:rect :bottom])))
  (assert (= 35 (get-in dummy-sub-frame3 [:rect :right])))

  (assert (= 10 (get-in dummy-sub-frame4 [:rect :top])))
  (assert (= 35 (get-in dummy-sub-frame4 [:rect :left])))
  (assert (= 110 (get-in dummy-sub-frame4 [:rect :bottom])))
  (assert (= 60 (get-in dummy-sub-frame4 [:rect :right])))

  #
  # dummy-frame -+- dummy-sub-frame1 -+- dummy-sub-frame-3
  #              |                    |
  #              +- dummy-sub-frame2  +- dummy-sub-frame-4
  #
  (set dummy-frame
       (build-dummy-frame-tree
        [rect
         horizontal-frame-proto
           [{:top 10 :left 10 :bottom 110 :right 60}
            horizontal-frame-proto
              {:top 10 :left 10 :bottom 110 :right 30}
              {:top 10 :left 30 :bottom 110 :right 60}]
           {:top 10 :left 60 :bottom 110 :right 110}]
        dummy-monitor))

  (set dummy-sub-frame1 (get-in dummy-frame [:children 0]))
  (set dummy-sub-frame2 (get-in dummy-frame [:children 1]))
  (set dummy-sub-frame3 (get-in dummy-sub-frame1 [:children 0]))
  (set dummy-sub-frame4 (get-in dummy-sub-frame1 [:children 1]))

  (def resized-frames (:balance dummy-frame true @[]))

  (assert (= 2 (length resized-frames)))
  (assert (= dummy-sub-frame3 (in resized-frames 0)))
  (assert (= dummy-sub-frame4 (in resized-frames 1)))

  (assert (= 10 (get-in dummy-sub-frame3 [:rect :top])))
  (assert (= 10 (get-in dummy-sub-frame3 [:rect :left])))
  (assert (= 110 (get-in dummy-sub-frame3 [:rect :bottom])))
  (assert (= 35 (get-in dummy-sub-frame3 [:rect :right])))

  (assert (= 10 (get-in dummy-sub-frame4 [:rect :top])))
  (assert (= 35 (get-in dummy-sub-frame4 [:rect :left])))
  (assert (= 110 (get-in dummy-sub-frame4 [:rect :bottom])))
  (assert (= 60 (get-in dummy-sub-frame4 [:rect :right]))))


(defn test-frame-rotate-children []
  (def dummy-monitor {:dpi [const/USER-DEFAULT-SCREEN-DPI const/USER-DEFAULT-SCREEN-DPI]})
  (def rect {:top 10 :left 10 :bottom 130 :right 130})
  (def rect1 {:top 10 :left 10 :bottom 130 :right 50})
  (def rect2 {:top 10 :left 50 :bottom 130 :right 90})
  (def rect3 {:top 10 :left 90 :bottom 130 :right 130})

  #
  # dummy-frame -+- dummy-sub-frame1
  #              |
  #              +- dummy-sub-frame2
  #              |
  #              +- dummy-sub-frame3
  #
  (def dummy-frame
    (build-dummy-frame-tree
     [rect
      horizontal-frame-proto
        rect1
        rect2
        rect3]
     dummy-monitor))
  (def dummy-sub-frame1 (get-in dummy-frame [:children 0]))
  (def dummy-sub-frame2 (get-in dummy-frame [:children 1]))
  (def dummy-sub-frame3 (get-in dummy-frame [:children 2]))

  (assert (= dummy-sub-frame1 (in dummy-frame :current-child)))

  (:rotate-children dummy-frame :forward)

  (assert (= dummy-sub-frame1 (in dummy-frame :current-child)))

  (assert (= dummy-sub-frame2 (get-in dummy-frame [:children 0])))
  (assert (= rect1 (in dummy-sub-frame2 :rect)))

  (assert (= dummy-sub-frame3 (get-in dummy-frame [:children 1])))
  (assert (= rect2 (in dummy-sub-frame3 :rect)))

  (assert (= dummy-sub-frame1 (get-in dummy-frame [:children 2])))
  (assert (= rect3 (in dummy-sub-frame1 :rect)))

  (:rotate-children dummy-frame :forward)

  (assert (= dummy-sub-frame1 (in dummy-frame :current-child)))

  (assert (= dummy-sub-frame3 (get-in dummy-frame [:children 0])))
  (assert (= rect1 (in dummy-sub-frame3 :rect)))

  (assert (= dummy-sub-frame1 (get-in dummy-frame [:children 1])))
  (assert (= rect2 (in dummy-sub-frame1 :rect)))

  (assert (= dummy-sub-frame2 (get-in dummy-frame [:children 2])))
  (assert (= rect3 (in dummy-sub-frame2 :rect)))

  (:rotate-children dummy-frame :backward)

  (assert (= dummy-sub-frame1 (in dummy-frame :current-child)))

  (assert (= dummy-sub-frame2 (get-in dummy-frame [:children 0])))
  (assert (= rect1 (in dummy-sub-frame2 :rect)))

  (assert (= dummy-sub-frame3 (get-in dummy-frame [:children 1])))
  (assert (= rect2 (in dummy-sub-frame3 :rect)))

  (assert (= dummy-sub-frame1 (get-in dummy-frame [:children 2])))
  (assert (= rect3 (in dummy-sub-frame1 :rect)))

  (:rotate-children dummy-frame :backward)

  (assert (= dummy-sub-frame1 (in dummy-frame :current-child)))

  (assert (= dummy-sub-frame1 (get-in dummy-frame [:children 0])))
  (assert (= rect1 (in dummy-sub-frame1 :rect)))

  (assert (= dummy-sub-frame2 (get-in dummy-frame [:children 1])))
  (assert (= rect2 (in dummy-sub-frame2 :rect)))

  (assert (= dummy-sub-frame3 (get-in dummy-frame [:children 2])))
  (assert (= rect3 (in dummy-sub-frame3 :rect))))


(defn test-frame-reverse-children []
  (def dummy-monitor {:dpi [const/USER-DEFAULT-SCREEN-DPI const/USER-DEFAULT-SCREEN-DPI]})
  (def rect {:top 10 :left 10 :bottom 130 :right 130})
  (def rect1 {:top 10 :left 10 :bottom 130 :right 50})
  (def rect2 {:top 10 :left 50 :bottom 130 :right 90})
  (def rect3 {:top 10 :left 90 :bottom 130 :right 130})

  #
  # dummy-frame -+- dummy-sub-frame1
  #              |
  #              +- dummy-sub-frame2
  #              |
  #              +- dummy-sub-frame3
  #
  (def dummy-frame
    (build-dummy-frame-tree
     [rect
      horizontal-frame-proto
        rect1
        rect2
        rect3]
     dummy-monitor))
  (def dummy-sub-frame1 (get-in dummy-frame [:children 0]))
  (def dummy-sub-frame2 (get-in dummy-frame [:children 1]))
  (def dummy-sub-frame3 (get-in dummy-frame [:children 2]))

  (assert (= dummy-sub-frame1 (in dummy-frame :current-child)))

  (:reverse-children dummy-frame)

  (assert (= dummy-sub-frame1 (in dummy-frame :current-child)))

  (assert (= dummy-sub-frame3 (get-in dummy-frame [:children 0])))
  (assert (= rect1 (in dummy-sub-frame3 :rect)))

  (assert (= dummy-sub-frame2 (get-in dummy-frame [:children 1])))
  (assert (= rect2 (in dummy-sub-frame2 :rect)))

  (assert (= dummy-sub-frame1 (get-in dummy-frame [:children 2])))
  (assert (= rect3 (in dummy-sub-frame1 :rect)))

  (:reverse-children dummy-frame)

  (assert (= dummy-sub-frame1 (in dummy-frame :current-child)))

  (assert (= dummy-sub-frame1 (get-in dummy-frame [:children 0])))
  (assert (= rect1 (in dummy-sub-frame1 :rect)))

  (assert (= dummy-sub-frame2 (get-in dummy-frame [:children 1])))
  (assert (= rect2 (in dummy-sub-frame2 :rect)))

  (assert (= dummy-sub-frame3 (get-in dummy-frame [:children 2])))
  (assert (= rect3 (in dummy-sub-frame3 :rect))))


(defn test-frame-set-direction []
  (def dummy-monitor {:dpi [const/USER-DEFAULT-SCREEN-DPI const/USER-DEFAULT-SCREEN-DPI]})
  (def rect {:top 10 :left 10 :bottom 110 :right 110})
  (def rect1 {:top 10 :left 10 :bottom 110 :right 60})
  (def rect2 {:top 10 :left 60 :bottom 110 :right 110})

  #
  # dummy-frame -+- dummy-sub-frame1
  #              |
  #              +- dummy-sub-frame2
  #
  (var dummy-frame
    (build-dummy-frame-tree
     [rect
      horizontal-frame-proto
        rect1
        rect2]
     dummy-monitor))
  (var dummy-sub-frame1 (get-in dummy-frame [:children 0]))
  (var dummy-sub-frame2 (get-in dummy-frame [:children 1]))

  (var error-raised nil)
  (try
    (:set-direction dummy-frame :north-west)
    ((err _fib)
     (assert (= err "can not change direction from :horizontal to :north-west"))
     (set error-raised true)))
  (assert (= true error-raised))
  
  (set error-raised nil)
  (try
    (:set-direction dummy-sub-frame1 :vertical)
    ((err _fib)
     (assert (= err "can not change direction from nil to :vertical"))
     (set error-raised true)))
  (assert (= true error-raised))

  (:set-direction dummy-frame :vertical)

  (assert (= rect (in dummy-frame :rect)))
  (assert (= (in dummy-sub-frame1 :rect)
             {:top 10 :left 10 :bottom 60 :right 110}))
  (assert (= (in dummy-sub-frame2 :rect)
             {:top 60 :left 10 :bottom 110 :right 110}))

  (:set-direction dummy-frame :horizontal)

  (assert (= rect (in dummy-frame :rect)))
  (assert (= rect1 (in dummy-sub-frame1 :rect)))
  (assert (= rect2 (in dummy-sub-frame2 :rect)))

  (def rect3 {:top 10 :left 10 :bottom 60 :right 60})
  (def rect4 {:top 30 :left 10 :bottom 110 :right 60})

  #
  # dummy-frame --+- dummy-sub-frame1 --+- dummy-sub-frame3
  #               |                     |
  #               +- dummy-sub-frame2   +- dummy-sub-frame4
  #
  (set dummy-frame
    (build-dummy-frame-tree
     [rect
      horizontal-frame-proto
        [rect1
         vertical-frame-proto
           rect3
           rect4]
        rect2]
     dummy-monitor))
  (set dummy-sub-frame1 (get-in dummy-frame [:children 0]))
  (set dummy-sub-frame2 (get-in dummy-frame [:children 1]))
  (var dummy-sub-frame3 (get-in dummy-sub-frame1 [:children 0]))
  (var dummy-sub-frame4 (get-in dummy-sub-frame1 [:children 1]))

  (:set-direction dummy-frame :vertical true)

  (assert (= rect (in dummy-frame :rect)))
  (assert (= (in dummy-sub-frame1 :rect)
             {:top 10 :left 10 :bottom 60 :right 110}))
  (assert (= (in dummy-sub-frame2 :rect)
             {:top 60 :left 10 :bottom 110 :right 110}))
  (assert (= (in dummy-sub-frame3 :rect)
             {:top 10 :left 10 :bottom 35 :right 110}))
  (assert (= (in dummy-sub-frame4 :rect)
             {:top 35 :left 10 :bottom 60 :right 110}))

  (:set-direction dummy-frame :horizontal true)

  (assert (= rect (in dummy-frame :rect)))
  (assert (= rect1 (in dummy-sub-frame1 :rect)))
  (assert (= rect2 (in dummy-sub-frame2 :rect)))
  (assert (= (in dummy-sub-frame3 :rect)
             {:top 10 :left 10 :bottom 110 :right 35}))
  (assert (= (in dummy-sub-frame4 :rect)
             {:top 10 :left 35 :bottom 110 :right 60})))


(defn test-frame-toggle-direction []
  (def dummy-monitor {:dpi [const/USER-DEFAULT-SCREEN-DPI const/USER-DEFAULT-SCREEN-DPI]})
  (def rect {:top 10 :left 10 :bottom 110 :right 110})
  (def rect1 {:top 10 :left 10 :bottom 110 :right 60})
  (def rect2 {:top 10 :left 60 :bottom 110 :right 110})
  (def rect3 {:top 10 :left 10 :bottom 60 :right 60})
  (def rect4 {:top 60 :left 10 :bottom 110 :right 60})

  #
  # (horizontal)       (vertical)
  # dummy-frame --+- dummy-sub-frame1 --+- dummy-sub-frame3
  #               |                     |
  #               +- dummy-sub-frame2   +- dummy-sub-frame4
  #
  (def dummy-frame
    (build-dummy-frame-tree
     [rect
      horizontal-frame-proto
        [rect1
         vertical-frame-proto
           rect3
           rect4]
        rect2]
     dummy-monitor))
  (def dummy-sub-frame1 (get-in dummy-frame [:children 0]))
  (def dummy-sub-frame2 (get-in dummy-frame [:children 1]))
  (def dummy-sub-frame3 (get-in dummy-sub-frame1 [:children 0]))
  (def dummy-sub-frame4 (get-in dummy-sub-frame1 [:children 1]))

  (:toggle-direction dummy-frame)

  (assert (= rect (in dummy-frame :rect)))
  (assert (= (in dummy-sub-frame1 :rect)
             {:top 10 :left 10 :bottom 60 :right 110}))
  (assert (= (in dummy-sub-frame2 :rect)
             {:top 60 :left 10 :bottom 110 :right 110}))
  (assert (= (in dummy-sub-frame3 :rect)
             {:top 10 :left 10 :bottom 35 :right 110}))
  (assert (= (in dummy-sub-frame4 :rect)
             {:top 35 :left 10 :bottom 60 :right 110}))

  (:toggle-direction dummy-frame)

  (assert (= rect (in dummy-frame :rect)))
  (assert (= rect1 (in dummy-sub-frame1 :rect)))
  (assert (= rect2 (in dummy-sub-frame2 :rect)))
  (assert (= rect3 (in dummy-sub-frame3 :rect)))
  (assert (= rect4 (in dummy-sub-frame4 :rect)))

  (:toggle-direction dummy-frame true)

  (assert (= rect (in dummy-frame :rect)))
  (assert (= (in dummy-sub-frame1 :rect)
             {:top 10 :left 10 :bottom 60 :right 110}))
  (assert (= (in dummy-sub-frame2 :rect)
             {:top 60 :left 10 :bottom 110 :right 110}))
  (assert (= (in dummy-sub-frame3 :rect)
             {:top 10 :left 10 :bottom 60 :right 60}))
  (assert (= (in dummy-sub-frame4 :rect)
             {:top 10 :left 60 :bottom 60 :right 110}))

  (:toggle-direction dummy-frame true)

  (assert (= rect (in dummy-frame :rect)))
  (assert (= rect1 (in dummy-sub-frame1 :rect)))
  (assert (= rect2 (in dummy-sub-frame2 :rect)))
  (assert (= rect3 (in dummy-sub-frame3 :rect)))
  (assert (= rect4 (in dummy-sub-frame4 :rect))))


(defn test-layout-get-adjacent-frame []
  (def dummy-monitor {:dpi [const/USER-DEFAULT-SCREEN-DPI const/USER-DEFAULT-SCREEN-DPI]})
  (var dummy-frame (frame {:top 10 :left 10 :bottom 110 :right 110}))
  (put dummy-frame :monitor dummy-monitor)
  (var dummy-layout (layout :dummy-id "dummy-name" nil [dummy-frame]))
  (:split dummy-frame :horizontal 3 [0.3 0.4 0.3])

  (assert (= 3 (length (in dummy-frame :children))))

  (assert (= (:get-adjacent-frame (get-in dummy-frame [:children 0]) :right)
             (get-in dummy-frame [:children 1])))
  (assert (= (:get-adjacent-frame (get-in dummy-frame [:children 1]) :right)
             (get-in dummy-frame [:children 2])))
  (assert (= (:get-adjacent-frame (get-in dummy-frame [:children 2]) :left)
             (get-in dummy-frame [:children 1])))
  (assert (nil? (:get-adjacent-frame (get-in dummy-frame [:children 2]) :right)))
  (assert (nil? (:get-adjacent-frame (get-in dummy-frame [:children 0]) :up)))
  (assert (nil? (:get-adjacent-frame (get-in dummy-frame [:children 0]) :down)))

  (set dummy-frame (frame {:top 10 :left 10 :bottom 110 :right 110}))
  (var dummy-frame2 (frame {:top 10 :left -90 :bottom 110 :right 10}))
  (set dummy-layout (layout :dummy-id "dummy-name" nil [dummy-frame dummy-frame2]))

  (assert (= (:get-adjacent-frame dummy-frame :left)
             dummy-frame2))
  (assert (= (:get-adjacent-frame dummy-frame2 :right)
             dummy-frame))

  # out-of-order top-level frames
  (set dummy-frame (frame {:top 0 :left 0 :bottom 100 :right 100}))
  (var dummy-frame3 (frame {:top 0 :left 100 :bottom 100 :right 200}))
  (set dummy-frame2 (frame {:top 0 :left 200 :bottom 100 :right 300}))
  (set dummy-layout (layout :dummy-id "dummy-name" nil [dummy-frame dummy-frame2 dummy-frame3]))

  (assert (= (:get-adjacent-frame dummy-frame :right)
             dummy-frame3))
  (assert (= (:get-adjacent-frame dummy-frame3 :right)
             dummy-frame2))
  (assert (nil? (:get-adjacent-frame dummy-frame2 :right)))
  (assert (nil? (:get-adjacent-frame dummy-frame :left))))


(defn test-tree-node-attached? []
  (def dummy-frame (frame {:top 10 :left 10 :bottom 110 :right 110}))
  (assert (= false (:attached? dummy-frame)))

  (def dummy-layout (layout :dummy-id "dummy-name" nil [dummy-frame]))
  (assert (= false (:attached? dummy-layout)))
  (assert (= false (:attached? dummy-frame)))

  (def dummy-vdc (virtual-desktop-container :dummy-wm [dummy-layout]))
  (assert (= true (:attached? dummy-vdc)))
  (assert (= true (:attached? dummy-layout)))
  (assert (= true (:attached? dummy-frame))))


(defn main [&]
  (test-tree-node-activate)
  (test-frame-constructor)
  (test-frame-add-child)
  (test-frame-split)
  (test-frame-close)
  (test-frame-close-with-unconstrained-parent)
  (test-frame-close-with-unconstrained-sibling)
  (test-frame-close-with-unconstrained-parent-and-sibling)
  (test-frame-insert-sub-frame)
  (test-frame-find-hwnd)
  (test-frame-get-current-frame)
  (test-frame-transform)
  (test-frame-balance)
  (test-frame-rotate-children)
  (test-frame-reverse-children)
  (test-frame-set-direction)
  (test-frame-toggle-direction)
  (test-layout-get-adjacent-frame)
  (test-tree-node-attached?))
