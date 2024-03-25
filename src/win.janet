(import ./log)


######### Generic tree node #########

(defn tree-node-activate [self]
  (var child self)
  (var parent (in self :parent))
  (while parent
    (put parent :current-child child)
    (set child parent)
    (set parent (in parent :parent)))
  self)


(def- tree-node-proto
  @{:activate tree-node-activate})


(defn tree-node [parent children &keys extra-fields]
  (let [node (table/setproto
              @{:parent parent
                :children children}
              tree-node-proto)]
    (eachp [k v] extra-fields
      (put node k v))
    node))


######### Window object #########

(def- window-proto
  (table/setproto
   @{}
   tree-node-proto))


(defn window [hwnd &opt parent]
  (let [node (tree-node parent nil
                        :type :window
                        :hwnd hwnd)]
    (table/setproto node window-proto)))


######### Frame object #########

# Forward declaration of the frame constructor
(var frame nil)


(defn frame-add-child [self child]
  (let [children (in self :children)
        old-parent (in child :parent)]
    (cond
      (empty? children)
      (do
        (put child :parent self)
        (array/push children child))

      (= (in child :type) :window)
      (if (= (get-in children [0 :type]) :window)
        (do
          (put child :parent self)
          (array/push children child))
        (error "cannot mix frames and windows"))

      (= (in child :type) :frame)
      (if (= (get-in children [0 :type]) :frame)
        (do
          (put child :parent self)
          (array/push children child))
        (error "cannot mix frames and windows")))

    # Do the removal after a successful insertion, so
    # that we won't end up in an inconsistent state
    (when old-parent
      (put old-parent :children
         (filter |(not= $ child) (in old-parent :children)))))
  self)


(defn frame-split [self direction &opt n ratios]
  (default n 2)
  (default ratios [0.5])
  (if (and (not (empty? (in self :children)))
           (= (get-in self [:children 0 :type]) :frame))
    (error "frame is already split"))
  (if (<= n 1)
    (error "invalid number of sub-frames"))
  (if (< (length ratios) (- n 1))
    (error "not enough ratios provided"))
  (def full-ratios
    (if (> (length ratios) (- n 1))
      (slice ratios 0 (- n 1))
      ratios))

  (let [rect (in self :rect)
        top (in rect :top)
        bottom (in rect :bottom)
        left (in rect :left)
        right (in rect :right)
        height (- bottom top)
        width (- right left)
        new-frames @[]]

    (var current-top top)
    (var current-left left)

    (case direction
      :horizontal
      (for i 0 n
        (def current-width
          (if (>= i (length full-ratios)) # Is this the last sub-frame?
            (- (+ width left) current-left)
            (math/floor (* width (in full-ratios i)))))
        (if (<= current-width 0)
          (error "cannot create zero-width frames"))
        (array/push new-frames
                    (frame {:top current-top
                            :left current-left
                            :right (+ current-left current-width)
                            :bottom bottom}
                           self))
        (+= current-left current-width))

      :vertical
      (for i 0 n
        (def current-height
          (if (>= i (length full-ratios)) # Is this the last sub-frame?
            (- (+ height top) current-top)
            (math/floor (* height (in full-ratios i)))))
        (if (<= current-height 0)
          (error "cannot create zero-height frames"))
        (array/push new-frames
                    (frame {:top current-top
                            :left current-left
                            :right right
                            :bottom (+ current-top current-height)}
                           self))
        (+= current-top current-height)))

    (def old-children (in self :children))
    (def old-active-child (in self :current-child))
    (put self :children new-frames)
    (def first-sub-frame (in new-frames 0))
    # XXX: Move all window children to the first sub-frame
    (each win old-children
      (:add-child first-sub-frame win))
    (put first-sub-frame :current-child old-active-child))

  self)


(def- frame-proto
  (table/setproto
   @{:add-child frame-add-child
     :split frame-split}
   tree-node-proto))


(varfn frame [rect &opt parent children]
  (default children @[])
  (let [node (tree-node parent children
                        :type :frame
                        :rect rect
                        :current-child (if-not (empty? children)
                                         (in children 0)))]
    (table/setproto node frame-proto)))


######### Window manager object #########

(defn wm-focus-changed [self hwnd]
  (if-not (get-in self [:managed-windows hwnd])
    # new window
    (do
      (log/debug "new window: %n" hwnd)
      (put (self :managed-windows) hwnd true))))


(def- wm-proto
  @{:focus-changed wm-focus-changed})


(defn window-manager []
  (table/setproto
   @{:frame-tree @[]
     :managed-windows @{}}
   wm-proto))
