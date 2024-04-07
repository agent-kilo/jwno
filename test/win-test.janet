(use ../src/win)


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
     (assert (= err "cannot mix frames and windows"))))

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
     (assert (= err "cannot mix frames and windows")))))


(defn test-frame-split []
  (var dummy-frame (frame {:top 10 :left 10 :bottom 110 :right 110}))

  (try
    (:split dummy-frame :horizontal 1)
    ((err fib)
     (assert (= err "invalid number of sub-frames"))))

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

  (try
    (:split dummy-frame :vertical)
    ((err fib)
     (assert (= err "frame is already split"))))

  (set dummy-frame (frame {:top 10 :left 10 :bottom 110 :right 110}))
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

  (set dummy-frame (frame {:top 10 :left 10 :bottom 110 :right 110}))
  (try
    (:split dummy-frame :horizontal 3)
    ((err fib)
     (assert (= err "not enough ratios provided"))))
  (assert (= (length (in dummy-frame :children)) 0))

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

  (set dummy-frame (frame {:top 10 :left 10 :bottom 110 :right 110}))
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

  (set dummy-frame (frame {:top 10 :left 10 :bottom 110 :right 110}))
  (def dummy-window1 (window :dummy-hwnd1))
  (def dummy-window2 (window :dummy-hwnd2))
  (:add-child dummy-frame dummy-window1)
  (:add-child dummy-frame dummy-window2)
  (:activate dummy-window1)

  (:split dummy-frame :horizontal)

  (assert (nil? (in dummy-frame :current-child)))

  (assert (= (get-in dummy-frame [:children 0 :type]) :frame))
  (assert (= (get-in dummy-frame [:children 1 :type]) :frame))

  (assert (= (length (get-in dummy-frame [:children 0 :children])) 2))
  (assert (= (get-in dummy-frame [:children 0 :children 0 :type]) :window))
  (assert (= (get-in dummy-frame [:children 0 :children 1 :type]) :window))

  (assert (= (length (get-in dummy-frame [:children 1 :children])) 0))

  (set dummy-frame (frame {:top 10 :left 10 :bottom 11 :right 11}))
  (try
    (:split dummy-frame :horizontal 2 [0.5])
    ((err fib)
     (assert (= err "cannot create zero-width frames"))))
  (assert (= (length (in dummy-frame :children)) 0))
  (try
    (:split dummy-frame :vertical 2 [0.5])
    ((err fib)
     (assert (= err "cannot create zero-height frames"))))
  (assert (= (length (in dummy-frame :children)) 0)))


(defn test-tree-node-activate []
  #
  # dummy-frame -+- dummy-sub-frame1 -- dummy-window1
  #              |
  #              +- dummy-sub-frame2 -- dummy-window2
  #
  (var dummy-frame (frame {:top 10 :left 10 :bottom 110 :right 110}))
  (var dummy-sub-frame1 (frame {:top 10 :left 10 :bottom 110 :right 60}))
  (var dummy-sub-frame2 (frame {:top 10 :left 60 :bottom 110 :right 110}))
  (var dummy-window1 (window :dummy-hwnd))
  (var dummy-window2 (window :dummy-hwnd))

  (:add-child dummy-frame dummy-sub-frame1)
  (:add-child dummy-frame dummy-sub-frame2)

  (:add-child dummy-sub-frame1 dummy-window1)
  (:add-child dummy-sub-frame2 dummy-window2)

  (:activate dummy-window1)
  (assert (= (in dummy-sub-frame1 :current-child) dummy-window1))
  (assert (= (in dummy-frame :current-child) dummy-sub-frame1))

  (:activate dummy-sub-frame2)
  (assert (= (in dummy-frame :current-child) dummy-sub-frame2))
  (assert (nil? (in dummy-sub-frame2 :current-child)))
  (assert (= (in dummy-sub-frame1 :current-child) dummy-window1)))


(defn test-frame-find-window []
  #
  # dummy-frame -+- dummy-sub-frame1 -- dummy-window1
  #              |
  #              +- dummy-sub-frame2 -- dummy-window2
  #
  (var dummy-frame (frame {:top 10 :left 10 :bottom 110 :right 110}))
  (var dummy-sub-frame1 (frame {:top 10 :left 10 :bottom 110 :right 60}))
  (var dummy-sub-frame2 (frame {:top 10 :left 60 :bottom 110 :right 110}))
  (var dummy-window1 (window :dummy-hwnd1))
  (var dummy-window2 (window :dummy-hwnd2))

  (:add-child dummy-frame dummy-sub-frame1)
  (:add-child dummy-frame dummy-sub-frame2)

  (:add-child dummy-sub-frame1 dummy-window1)
  (:add-child dummy-sub-frame2 dummy-window2)

  (:activate dummy-window1)

  (assert (= dummy-window1 (:find-window dummy-frame :dummy-hwnd1)))
  (assert (= dummy-window2 (:find-window dummy-frame :dummy-hwnd2)))
  (assert (nil? (:find-window dummy-frame :dummy-hwnd3)))
  )


(defn test-frame-find-frame-for-window []
  #
  # dummy-frame -+- dummy-sub-frame1 -- dummy-window1
  #              |
  #              +- dummy-sub-frame2 -- dummy-window2
  #
  (var dummy-frame (frame {:top 10 :left 10 :bottom 110 :right 110}))
  (var dummy-sub-frame1 (frame {:top 10 :left 10 :bottom 110 :right 60}))
  (var dummy-sub-frame2 (frame {:top 10 :left 60 :bottom 110 :right 110}))
  (var dummy-window1 (window :dummy-hwnd1))
  (var dummy-window2 (window :dummy-hwnd2))
  (var dummy-window3 (window :dummy-hwnd3))

  (assert (= dummy-frame (:find-frame-for-window dummy-frame dummy-window3)))

  (:add-child dummy-frame dummy-sub-frame1)
  (:add-child dummy-frame dummy-sub-frame2)

  (:add-child dummy-sub-frame1 dummy-window1)
  (:add-child dummy-sub-frame2 dummy-window2)

  (try
    (:find-frame-for-window dummy-frame dummy-window3)
    ((err fib)
     (assert (= err "inconsistent states for frame tree"))))

  (:activate dummy-window1)
  (assert (= dummy-sub-frame1 (:find-frame-for-window dummy-frame dummy-window3)))

  (:activate dummy-window2)
  (assert (= dummy-sub-frame2 (:find-frame-for-window dummy-frame dummy-window3))))


(defn test-frame-resize []
  #
  # dummy-frame -+- dummy-sub-frame1 -- dummy-window1
  #              |
  #              +- dummy-sub-frame2 -- dummy-window2
  #
  (var dummy-frame (frame {:top 10 :left 10 :bottom 110 :right 110}))
  (var dummy-sub-frame1 (frame {:top 10 :left 10 :bottom 110 :right 60}))
  (var dummy-sub-frame2 (frame {:top 10 :left 60 :bottom 110 :right 110}))
  (var dummy-window1 (window :dummy-hwnd1))
  (var dummy-window2 (window :dummy-hwnd2))
  (:add-child dummy-frame dummy-sub-frame1)
  (:add-child dummy-frame dummy-sub-frame2)
  (:add-child dummy-sub-frame1 dummy-window1)
  (:add-child dummy-sub-frame2 dummy-window2)

  (:resize dummy-frame {:top 10 :left 20 :bottom 110 :right 100})

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
  (assert (= 100 (get-in dummy-sub-frame2 [:rect :right]))))


(defn main [&]
  (test-tree-node-activate)
  (test-frame-constructor)
  (test-frame-add-child)
  (test-frame-split)
  (test-frame-find-window)
  (test-frame-find-frame-for-window)
  (test-frame-resize))
