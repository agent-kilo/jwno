(use ../src/win)

(import ../src/const)


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

  (var dummy-frame (frame {:top 10 :left 10 :bottom 110 :right 110}))
  (put dummy-frame :monitor dummy-monitor)

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
  (put dummy-frame :monitor dummy-monitor)
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
  (put dummy-frame :monitor dummy-monitor)
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

  (set dummy-frame (frame {:top 10 :left 10 :bottom 110 :right 110}))
  (put dummy-frame :monitor dummy-monitor)
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
  (put dummy-frame :monitor dummy-monitor)
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
  (put dummy-frame :monitor dummy-monitor)
  (def dummy-window1 (window :dummy-hwnd1))
  (def dummy-window2 (window :dummy-hwnd2))
  (def dummy-window3 (window :dummy-hwnd3))
  (:add-child dummy-frame dummy-window1)
  (:add-child dummy-frame dummy-window2)
  (:add-child dummy-frame dummy-window3)
  (:activate dummy-window1)

  (:split dummy-frame :horizontal)

  (assert (= (get-in dummy-frame [:children 0 :type]) :frame))
  (assert (= (get-in dummy-frame [:children 1 :type]) :frame))

  (assert (= (length (get-in dummy-frame [:children 0 :children])) 3))
  (assert (= (get-in dummy-frame [:children 0 :children 0 :type]) :window))
  (assert (= (get-in dummy-frame [:children 0 :children 1 :type]) :window))
  (assert (= (get-in dummy-frame [:children 0 :children 2 :type]) :window))

  (assert (= (length (get-in dummy-frame [:children 1 :children])) 0))

  (set dummy-frame (frame {:top 10 :left 10 :bottom 11 :right 11}))
  (put dummy-frame :monitor dummy-monitor)
  (try
    (:split dummy-frame :horizontal 2 [0.5])
    ((err fib)
     (assert (= err "cannot create zero-width frames"))))
  (assert (= (length (in dummy-frame :children)) 0))
  (try
    (:split dummy-frame :vertical 2 [0.5])
    ((err fib)
     (assert (= err "cannot create zero-height frames"))))
  (assert (= (length (in dummy-frame :children)) 0))

  (set dummy-frame (frame {:top 10 :left 10 :bottom 110 :right 110}))
  (put dummy-frame :monitor dummy-monitor)
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

  (set dummy-frame (frame {:top 10 :left 10 :bottom 110 :right 110}))
  (put dummy-frame :monitor dummy-monitor)
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
    (assert (= 104 (in rect1 :right)))))


(defn test-frame-insert-sub-frame []
  (def dummy-monitor {:dpi [const/USER-DEFAULT-SCREEN-DPI const/USER-DEFAULT-SCREEN-DPI]})

  (var dummy-frame (frame {:top 10 :left 10 :bottom 110 :right 110}))
  (put dummy-frame :monitor dummy-monitor)

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


  (set dummy-frame (frame {:top 10 :left 10 :bottom 110 :right 110}))
  (put dummy-frame :monitor dummy-monitor)

  (def dummy-window1 (window :dummy-hwnd1))
  (def dummy-window2 (window :dummy-hwnd2))

  (:add-child dummy-frame dummy-window1)
  (:add-child dummy-frame dummy-window2)

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
  (assert (= dummy-window2 (in win-list 1))))


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
  (assert (= (in dummy-sub-frame2 :current-child) dummy-window2))
  (assert (= (in dummy-sub-frame1 :current-child) dummy-window1)))


(defn test-frame-find-hwnd []
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
  (var dummy-sub-frame1 (frame {:top 10 :left 10 :bottom 110 :right 60}))
  (var dummy-sub-frame2 (frame {:top 10 :left 60 :bottom 110 :right 110}))
  (var dummy-window1 (window :dummy-hwnd1))
  (var dummy-window2 (window :dummy-hwnd2))

  (assert (= dummy-frame (:get-current-frame dummy-frame)))

  (:add-child dummy-frame dummy-sub-frame1)
  (:add-child dummy-frame dummy-sub-frame2)

  (:add-child dummy-sub-frame1 dummy-window1)
  (:add-child dummy-sub-frame2 dummy-window2)

  (try
    (:get-current-frame dummy-frame)
    ((err fib)
     (assert (= err "inconsistent states for frame tree"))))

  (:activate dummy-window1)
  (assert (= dummy-sub-frame1 (:get-current-frame dummy-frame)))

  (:activate dummy-window2)
  (assert (= dummy-sub-frame2 (:get-current-frame dummy-frame))))


(defn test-frame-transform []
  (def dummy-monitor {:dpi [const/USER-DEFAULT-SCREEN-DPI const/USER-DEFAULT-SCREEN-DPI]})
  #
  # dummy-frame -+- dummy-sub-frame1 -- dummy-window1
  #              |
  #              +- dummy-sub-frame2 -- dummy-window2
  #
  (var dummy-frame (frame {:top 10 :left 10 :bottom 110 :right 110}))
  (put dummy-frame :monitor dummy-monitor)
  # XXX: wrap this up?
  (table/setproto dummy-frame horizontal-frame-proto)
  (var dummy-sub-frame1 (frame {:top 10 :left 10 :bottom 110 :right 60}))
  (var dummy-sub-frame2 (frame {:top 10 :left 60 :bottom 110 :right 110}))
  (var dummy-window1 (window :dummy-hwnd1))
  (var dummy-window2 (window :dummy-hwnd2))
  (:add-child dummy-frame dummy-sub-frame1)
  (:add-child dummy-frame dummy-sub-frame2)
  (:add-child dummy-sub-frame1 dummy-window1)
  (:add-child dummy-sub-frame2 dummy-window2)

  (var resized-frames
    (:transform dummy-frame {:top 10 :left 10 :bottom 110 :right 110} nil @[]))

  (assert (empty? resized-frames))

  (set resized-frames
    (:transform dummy-frame {:top 10 :left 20 :bottom 110 :right 100} nil @[]))

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

  (set dummy-frame (frame {:top 10 :left 10 :bottom 110 :right 110}))
  (put dummy-frame :monitor dummy-monitor)
  (put (in dummy-frame :tags) :padding 9)
  (table/setproto dummy-frame horizontal-frame-proto)
  (set dummy-sub-frame1 (frame {:top 19 :left 19 :bottom 101 :right 60}))
  (set dummy-sub-frame2 (frame {:top 19 :left 60 :bottom 101 :right 101}))
  (:add-child dummy-frame dummy-sub-frame1)
  (:add-child dummy-frame dummy-sub-frame2)

  (:transform dummy-frame {:top 13 :left 20 :bottom 107 :right 100})

  (assert (= 22 (get-in dummy-sub-frame1 [:rect :top])))
  (assert (= 29 (get-in dummy-sub-frame1 [:rect :left])))
  (assert (= 98 (get-in dummy-sub-frame1 [:rect :bottom])))
  (assert (= 60 (get-in dummy-sub-frame1 [:rect :right])))

  (assert (= 22 (get-in dummy-sub-frame2 [:rect :top])))
  (assert (= 60 (get-in dummy-sub-frame2 [:rect :left])))
  (assert (= 98 (get-in dummy-sub-frame2 [:rect :bottom])))
  (assert (= 91 (get-in dummy-sub-frame2 [:rect :right])))

  (set dummy-frame (frame {:top 10 :left 10 :bottom 110 :right 110}))
  (put dummy-frame :monitor dummy-monitor)
  (put (in dummy-frame :tags) :paddings {:top 9 :left 8 :bottom 7 :right 6})
  (table/setproto dummy-frame vertical-frame-proto)
  (set dummy-sub-frame1 (frame {:top 19 :left 18 :bottom 61 :right 104}))
  (set dummy-sub-frame2 (frame {:top 61 :left 18 :bottom 103 :right 104}))
  (:add-child dummy-frame dummy-sub-frame1)
  (:add-child dummy-frame dummy-sub-frame2)

  (:transform dummy-frame {:top 13 :left 20 :bottom 107 :right 100})

  (assert (= 22 (get-in dummy-sub-frame1 [:rect :top])))
  (assert (= 28 (get-in dummy-sub-frame1 [:rect :left])))
  (assert (= 61 (get-in dummy-sub-frame1 [:rect :bottom])))
  (assert (= 94 (get-in dummy-sub-frame1 [:rect :right])))

  (assert (= 61 (get-in dummy-sub-frame2 [:rect :top])))
  (assert (= 28 (get-in dummy-sub-frame2 [:rect :left])))
  (assert (= 100 (get-in dummy-sub-frame2 [:rect :bottom])))
  (assert (= 94 (get-in dummy-sub-frame2 [:rect :right]))))


(defn test-frame-balance []
  (def dummy-monitor {:dpi [const/USER-DEFAULT-SCREEN-DPI const/USER-DEFAULT-SCREEN-DPI]})
  #
  # dummy-frame -+- dummy-sub-frame1
  #              |
  #              +- dummy-sub-frame2
  #
  (var dummy-frame (frame {:top 10 :left 10 :bottom 110 :right 110}))
  (put dummy-frame :monitor dummy-monitor)
  (table/setproto dummy-frame horizontal-frame-proto)
  (var dummy-sub-frame1 (frame {:top 10 :left 10 :bottom 110 :right 60}))
  (var dummy-sub-frame2 (frame {:top 10 :left 60 :bottom 110 :right 110}))
  (:add-child dummy-frame dummy-sub-frame1)
  (:add-child dummy-frame dummy-sub-frame2)

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

  (set dummy-frame (frame {:top 10 :left 10 :bottom 110 :right 110}))
  (put dummy-frame :monitor dummy-monitor)
  (table/setproto dummy-frame horizontal-frame-proto)
  (set dummy-sub-frame1 (frame {:top 10 :left 10 :bottom 110 :right 50}))
  (set dummy-sub-frame2 (frame {:top 10 :left 50 :bottom 110 :right 110}))
  (:add-child dummy-frame dummy-sub-frame1)
  (:add-child dummy-frame dummy-sub-frame2)

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
  (set dummy-frame (frame {:top 10 :left 10 :bottom 110 :right 110}))
  (put dummy-frame :monitor dummy-monitor)
  (table/setproto dummy-frame horizontal-frame-proto)
  (set dummy-sub-frame1 (frame {:top 10 :left 10 :bottom 110 :right 50}))
  (table/setproto dummy-sub-frame1 horizontal-frame-proto)
  (set dummy-sub-frame2 (frame {:top 10 :left 50 :bottom 110 :right 110}))
  (var dummy-sub-frame3 (frame {:top 10 :left 10 :bottom 110 :right 30}))
  (var dummy-sub-frame4 (frame {:top 10 :left 30 :bottom 110 :right 50}))
  (:add-child dummy-frame dummy-sub-frame1)
  (:add-child dummy-frame dummy-sub-frame2)
  (:add-child dummy-sub-frame1 dummy-sub-frame3)
  (:add-child dummy-sub-frame1 dummy-sub-frame4)

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
  (set dummy-frame (frame {:top 10 :left 10 :bottom 110 :right 110}))
  (put dummy-frame :monitor dummy-monitor)
  (table/setproto dummy-frame horizontal-frame-proto)
  (set dummy-sub-frame1 (frame {:top 10 :left 10 :bottom 110 :right 60}))
  (table/setproto dummy-sub-frame1 horizontal-frame-proto)
  (set dummy-sub-frame2 (frame {:top 10 :left 60 :bottom 110 :right 110}))
  (var dummy-sub-frame3 (frame {:top 10 :left 10 :bottom 110 :right 30}))
  (var dummy-sub-frame4 (frame {:top 10 :left 30 :bottom 110 :right 60}))
  (:add-child dummy-frame dummy-sub-frame1)
  (:add-child dummy-frame dummy-sub-frame2)
  (:add-child dummy-sub-frame1 dummy-sub-frame3)
  (:add-child dummy-sub-frame1 dummy-sub-frame4)

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
             dummy-frame)))


(defn test-tree-node-has-child? []
  (def dummy-frame (frame {:top 10 :left 10 :bottom 110 :right 110}))
  (assert (= false (:attached? dummy-frame)))

  (def dummy-layout (layout :dummy-id "dummy-name" nil [dummy-frame]))
  (assert (= false (:attached? dummy-layout)))
  (assert (= false (:attached? dummy-frame)))

  (def dummy-vdc (virtual-desktop-container :dummy-wm :dummy-hm [dummy-layout]))
  (assert (= true (:attached? dummy-vdc)))
  (assert (= true (:attached? dummy-layout)))
  (assert (= true (:attached? dummy-frame))))


(defn main [&]
  (test-tree-node-activate)
  (test-frame-constructor)
  (test-frame-add-child)
  (test-frame-split)
  (test-frame-insert-sub-frame)
  (test-frame-find-hwnd)
  (test-frame-get-current-frame)
  (test-frame-transform)
  (test-frame-balance)
  (test-layout-get-adjacent-frame)
  (test-tree-node-has-child?))
