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


(defn calc-grid [rect]
  (def [center-x center-y] (util/rect-center rect))
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
    :bottom (in rect :bottom)}])


(defn grid-to-hint-info [grid-rects]
  (def min-rect-height
    (reduce (fn [mh r]
              (let [h (util/rect-height r)]
                (if (< h mh)
                  h
                  mh)))
            math/int-max
            grid-rects))
  (def [label-scale line-width]
    (cond
      (< 400 min-rect-height) [4 4]
      (< 100 min-rect-height) [2 3]
      (< 50 min-rect-height)  [1 2]
      true                    [0.7 2]))
  {:highlight-rects grid-rects
   :elements (map |(tuple $ $) grid-rects)
   :label-scale label-scale
   :line-width line-width
   :label-anchor :center})


(defn mouse-grid-hinter-init [self ui-hint]
  (if-let [grid (last (in self :stack))]
    (grid-to-hint-info grid)
    (do
      (def {:context context} ui-hint)
      (def wm (in context :window-manager))
      (def top-fr (:get-current-top-frame (in wm :root)))
      (when top-fr
        (def mon (in top-fr :monitor))
        (def mon-rect (in mon :rect))
        (def grid-rects (calc-grid mon-rect))
        (array/push (in self :stack) mon-rect)
        (grid-to-hint-info grid-rects)))))


(defn mouse-grid-hinter-select [self elem]
  (def grid-rects (calc-grid elem))
  (array/push (in self :stack) elem)
  (grid-to-hint-info grid-rects))


(defn mouse-grid-hinter-return [self]
  (def {:stack stack} self)
  (when (< 1 (length stack))
    (array/pop stack))
  (when-let [cur-rect (last stack)]
    (def grid-rects (calc-grid cur-rect))
    (grid-to-hint-info grid-rects)))


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


(defn mouse-grid-hinter []
  (table/setproto
   @{:stack @[]}
   mouse-grid-hinter-proto))
