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

  (def dummy-sub-frame1 (frame {:top 10 :left 10 :bottom 110 :right 60}))
  (def dummy-sub-frame2 (frame {:top 10 :left 60 :bottom 110 :right 110}))

  (:add-child dummy-frame dummy-sub-frame1)
  (assert (= (in dummy-sub-frame1 :parent) dummy-frame))
  (assert (= (length (in dummy-frame :children)) 1))

  (:add-child dummy-frame dummy-sub-frame2)
  (assert (= (in dummy-sub-frame2 :parent) dummy-frame))
  (assert (= (length (in dummy-frame :children)) 2))

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
  (:add-child dummy-frame (window :dummy-hwnd1))
  (:add-child dummy-frame (window :dummy-hwnd2))

  (:split dummy-frame :horizontal)

  (assert (= (get-in dummy-frame [:children 0 :type]) :frame))
  (assert (= (get-in dummy-frame [:children 1 :type]) :frame))

  (assert (= (length (get-in dummy-frame [:children 0 :children])) 2))
  (assert (= (get-in dummy-frame [:children 0 :children 0 :type]) :window))
  (assert (= (get-in dummy-frame [:children 0 :children 1 :type]) :window))

  (assert (= (length (get-in dummy-frame [:children 1 :children])) 0)))


(defn main [&]
  (test-frame-constructor)
  (test-frame-add-child)
  (test-frame-split))
