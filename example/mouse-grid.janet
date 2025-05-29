#
# A simple script for Jwno, to click the mouse with the keyboard,
# by extending the built-in :ui-hint command.
#
# Inspired by Warpd (https://github.com/rvaiya/warpd) Grid mode
#
# To use it in your config, save this script alongside the config
# file, then:
#
#   (import mouse-grid)
#   (def mouse-grid-hinter (mouse-grid/mouse-grid-hinter))
#   # For example, bind the command to `Win + A`
#   (:define-key some-keymap
#                "Win + A"
#                [:ui-hint "UIJK" mouse-grid-hinter])
#


(use jw32/_winuser)
(use jw32/_errhandlingapi)
(use jw32/_util)

(import jwno/util)
(import jwno/log)


(defn calc-grid [rect &opt grid-mode]
  (default grid-mode :diagonal)

  (def [center-x center-y] (util/rect-center rect))
  (case grid-mode
    :diagonal
    [# Top left
     {:left   (in rect :left)
      :top    (in rect :top)
      :right  center-x
      :bottom center-y}
     # Top right
     {:left   center-x
      :top    (in rect :top)
      :right  (in rect :right)
      :bottom center-y}
     # Bottom left
     {:left   (in rect :left)
      :top    center-y
      :right  center-x
      :bottom (in rect :bottom)}
     # Bottom right
     {:left   center-x
      :top    center-y
      :right  (in rect :right)
      :bottom (in rect :bottom)}]

    :orthogonal
    [# Left
     {:left   (in rect :left)
      :top    (in rect :top)
      :right  center-x
      :bottom (in rect :bottom)}
     # Right
     {:left   center-x
      :top    (in rect :top)
      :right  (in rect :right)
      :bottom (in rect :bottom)}
     # Top
     {:left   (in rect :left)
      :top    (in rect :top)
      :right  (in rect :right)
      :bottom center-y}
     # Bottom
     {:left   (in rect :left)
      :top    center-y
      :right  (in rect :right)
      :bottom (in rect :bottom)}]

    (errorf "invalid grid mode: %n" grid-mode)))


(defn grid-to-hint-info [grid-rects]
  (def [min-rect-width min-rect-height]
    (reduce (fn [[mw mh] r]
              (def [w h] (util/rect-size r))
              [(if (< w mw) w mw)
               (if (< h mh) h mh)])
            [math/int-max math/int-max]
            grid-rects))
  (def min-val (min min-rect-width min-rect-height))
  (def [label-scale line-width]
    (cond
      (< 400 min-val) [4 3]
      (< 100 min-val) [2 2]
      (< 50 min-val)  [1 1]
      true            [0.7 1]))
  {:highlight-rects grid-rects
   :elements (map |(tuple $ $) grid-rects)
   :label-scale label-scale
   :line-width line-width
   :label-anchor :center})


(defn mouse-grid-hinter-init [self ui-hint]
  (def {:stack stack
        :grid-mode grid-mode}
    self)
  (if-let [cur-rect (last stack)]
    (grid-to-hint-info (calc-grid cur-rect grid-mode))
    (do
      (def {:context context} ui-hint)
      (def wm (in context :window-manager))
      (def top-fr (:get-current-top-frame (in wm :root)))
      (when top-fr
        (def mon (in top-fr :monitor))
        (def mon-rect (in mon :rect))
        (def grid-rects (calc-grid mon-rect grid-mode))
        (array/push stack mon-rect)
        (grid-to-hint-info grid-rects)))))


(defn mouse-grid-hinter-select [self elem]
  (def {:stack stack
        :grid-mode grid-mode}
    self)
  (def grid-rects (calc-grid elem grid-mode))
  (array/push stack elem)
  (grid-to-hint-info grid-rects))


(defn mouse-grid-hinter-return [self]
  (def {:stack stack
        :grid-mode grid-mode}
    self)
  (when (< 1 (length stack))
    (array/pop stack))
  (when-let [cur-rect (last stack)]
    (grid-to-hint-info (calc-grid cur-rect grid-mode))))


(defn mouse-grid-hinter-confirm [self]
  (when-let [cur-rect (last (in self :stack))]
    (:cancel self)
    (def center (util/rect-center cur-rect))
    (log/debug "mouse-grid-hinter: center = %n" center)
    (def spcp-ret (SetPhysicalCursorPos ;center))
    (if (= spcp-ret FALSE)
      (log/debug "mouse-grid-hinter: SetPhysicalCursorPos failed: %n" (GetLastError))
      # else
      (ev/spawn
       # XXX: The mouse would click on the hint window, if we sent these
       # events immediately. Should set WS_EX_TRANSPARENT on the hint window.
       (ev/sleep 0.2)
       (SendInput [(INPUT :type INPUT_MOUSE
                          :mi.dwFlags MOUSEEVENTF_LEFTDOWN)
                   (INPUT :type INPUT_MOUSE
                          :mi.dwFlags MOUSEEVENTF_LEFTUP)]))))
  nil)


(defn mouse-grid-hinter-cancel [self]
  (array/clear (in self :stack)))


(def mouse-grid-hinter-proto
  @{:init mouse-grid-hinter-init
    :select mouse-grid-hinter-select
    :return mouse-grid-hinter-return
    :confirm mouse-grid-hinter-confirm
    :cancel mouse-grid-hinter-cancel})


(defn mouse-grid-hinter [&named grid-mode]
  (default grid-mode :diagonal)

  (unless (find |(= $ grid-mode) [:diagonal :orthogonal])
    (errorf "invalid grid mode: %n" grid-mode))

  (table/setproto
   @{:stack @[]
     :grid-mode grid-mode}
   mouse-grid-hinter-proto))
