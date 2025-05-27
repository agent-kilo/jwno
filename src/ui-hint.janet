(use jw32/_winuser)
(use jw32/_wingdi)
(use jw32/_libloaderapi)
(use jw32/_errhandlingapi)
(use jw32/_uiautomation)
(use jw32/_dwmapi)
(use jw32/_util)

(use ./input)
(use ./util)

(import ./log)


################## vvvv Runs in UI thread vvvv ##################

(def HINT-AREA-WINDOW-CLASS-NAME "jwno-hint-area-window")
(var global-hint-state nil)


(defn create-cached-font [scale cache]
  (when-let [cached (in cache scale)]
    # Early return
    (break cached))

  # We simply copy a system font, and apply the scaling here
  (def [spi-ret spi-ncm] (SystemParametersInfo SPI_GETNONCLIENTMETRICS 0 nil 0))
  (when (= FALSE spi-ret)
    (errorf "SystemParametersInfo failed: 0x%x" (GetLastError)))
  (def cap-font (in spi-ncm :lfCaptionFont))
  (def hfont (CreateFont (math/floor (* scale (in cap-font :lfHeight)))
                         0  # Font width matches device aspect ratio
                         (in cap-font :lfEscapement)
                         (in cap-font :lfOrientation)
                         700 # Bold #(in cap-font :lfWeight)
                         (in cap-font :lfItalic)
                         (in cap-font :lfUnderline)
                         (in cap-font :lfStrikeOut)
                         (in cap-font :lfCharSet)
                         (in cap-font :lfOutPrecision)
                         (in cap-font :lfClipPrecision)
                         (in cap-font :lfQuality)
                         (in cap-font :lfPitchAndFamily)
                         (in cap-font :lfFaceName)))
  (when (null? hfont)
    (errorf "CreateFont failed"))

  (put cache scale hfont)
  hfont)


(defn draw-highlight-rect [hdc rect border-color shadow-color virt-line-width client-rect]
  (def [scale-x scale-y] (calc-pixel-scale rect))

  (def orig-pen (SelectObject hdc (GetStockObject NULL_PEN)))

  # The lines will vanish when line-width is < 1
  (def line-width (max 1 (math/floor (* virt-line-width scale-x))))
  (def shadow-offset (math/floor (* 2 scale-x)))

  (def offset-x (in client-rect :left))
  (def offset-y (in client-rect :top))
  (def left   (- (in rect 0) offset-x))
  (def top    (- (in rect 1) offset-y))
  (def right  (- (in rect 2) offset-x))
  (def bottom (- (in rect 3) offset-y))

  (defer
    (SelectObject hdc orig-pen)

    (do
      (unless (= :none shadow-color)
        # Shadow
        (SetDCBrushColor hdc shadow-color)
        (Rectangle hdc
                   left
                   top
                   (+ left line-width 1 shadow-offset) # Plus 1 for the null border of the rectangle
                   bottom)
        (Rectangle hdc
                   left
                   top
                   right
                   (+ top line-width 1 shadow-offset))
        (Rectangle hdc
                   (- right line-width 1 shadow-offset)
                   top
                   right
                   bottom)
        (Rectangle hdc
                   left
                   (- bottom line-width 1 shadow-offset)
                   right
                   bottom))

      # Box
      (SetDCBrushColor hdc border-color)
      (Rectangle hdc
                 left
                 top
                 (+ left line-width 1)
                 bottom)
      (Rectangle hdc
                 left
                 top
                 right
                 (+ top line-width 1))
      (Rectangle hdc
                 (- right line-width 1)
                 top
                 right
                 bottom)
      (Rectangle hdc
                 left
                 (- bottom line-width 1)
                 right
                 bottom))))


(def TEXT-ANCHOR-FNS
  {:left
     (fn [left top right bottom text-width text-height padding-x padding-y]
       (def text-x (+ left padding-x))
       (def text-y (brshift (+ top bottom (- text-height)) 1))
       [text-x text-y (+ text-x text-width) (+ text-y text-height)])
   :top-left
     (fn [left top right bottom text-width text-height padding-x padding-y]
       (def text-x (+ left padding-x))
       (def text-y (+ top padding-y))
       [text-x text-y (+ text-x text-width) (+ text-y text-height)])
   :bottom-left
     (fn [left top right bottom text-width text-height padding-x padding-y]
       (def text-x (+ left padding-x))
       (def text-y (- bottom text-height padding-y))
       [text-x text-y (+ text-x text-width) (+ text-y text-height)])
   :center
     (fn [left top right bottom text-width text-height padding-x padding-y]
       (def text-x (brshift (+ left right (- text-width)) 1))
       (def text-y (brshift (+ top bottom (- text-height)) 1))
       [text-x text-y (+ text-x text-width) (+ text-y text-height)])
   :top
     (fn [left top right bottom text-width text-height padding-x padding-y]
       (def text-x (brshift (+ left right (- text-width)) 1))
       (def text-y (+ top padding-y))
       [text-x text-y (+ text-x text-width) (+ text-y text-height)])
   :bottom
     (fn [left top right bottom text-width text-height padding-x padding-y]
       (def text-x (brshift (+ left right (- text-width)) 1))
       (def text-y (- bottom text-height padding-y))
       [text-x text-y (+ text-x text-width) (+ text-y text-height)])
   :right
     (fn [left top right bottom text-width text-height padding-x padding-y]
       (def text-x (- right text-width padding-x))
       (def text-y (brshift (+ top bottom (- text-height)) 1))
       [text-x text-y (+ text-x text-width) (+ text-y text-height)])
   :top-right
     (fn [left top right bottom text-width text-height padding-x padding-y]
       (def text-x (- right text-width padding-x))
       (def text-y (+ top padding-y))
       [text-x text-y (+ text-x text-width) (+ text-y text-height)])
   :bottom-right
     (fn [left top right bottom text-width text-height padding-x padding-y]
       (def text-x (- right text-width padding-x))
       (def text-y (- bottom text-height padding-y))
       [text-x text-y (+ text-x text-width) (+ text-y text-height)])})


(defn draw-label [hdc label rect text-color bg-color border-color shadow-color client-rect font-cache &opt label-scale anchor-fn]
  (default label-scale 1)
  (default anchor-fn (in TEXT-ANCHOR-FNS :left))

  (def [scale-x scale-y] (calc-pixel-scale rect))

  (def hfont (create-cached-font (* label-scale scale-y) font-cache))
  (def orig-hfont (SelectObject hdc hfont))

  (defer
    (do
      (SelectObject hdc orig-hfont))

    # body
    (def dt-format (bor DT_SINGLELINE DT_NOCLIP))
    (def [height text-rect] (DrawText hdc label [0 0 0 0] (bor dt-format DT_CALCRECT)))
    (when (>= 0 height)
      (error "DrawText DT_CALCRECT failed"))
    (def {:right text-width
          :bottom text-height}
      text-rect)

    (def padding 2)
    (def padding-x (math/floor (* padding scale-x label-scale)))
    (def padding-y (math/floor (* padding scale-y label-scale)))

    (def shadow-offset 2)
    (def shadow-offset-x (math/floor (* shadow-offset scale-x)))
    (def shadow-offset-y (math/floor (* shadow-offset scale-y)))

    (def offset-x (in client-rect :left))
    (def offset-y (in client-rect :top))
    (def left   (- (in rect 0) offset-x))
    (def top    (- (in rect 1) offset-y))
    (def right  (- (in rect 2) offset-x))
    (def bottom (- (in rect 3) offset-y))

    (def [text-x text-y text-right text-bottom]
      (anchor-fn left top right bottom
                 text-width text-height
                 padding-x padding-y))

    (def box-x (- text-x padding-x))
    (def box-y (- text-y padding-y))
    (def box-right (+ text-right padding-x))
    (def box-bottom (+ text-bottom padding-y))

    # Shadow
    (unless (= :none shadow-color)
      (SetDCBrushColor hdc shadow-color)
      (SetDCPenColor hdc shadow-color)
      (Rectangle hdc
                 (+ box-x shadow-offset-x)
                 (+ box-y shadow-offset-y)
                 (+ box-right shadow-offset-x)
                 (+ box-bottom shadow-offset-y)))

    # Background
    (SetDCBrushColor hdc bg-color)
    (SetDCPenColor hdc border-color)
    (Rectangle hdc box-x box-y box-right box-bottom)

    # Text
    (SetTextColor hdc text-color)
    (SetBkMode hdc TRANSPARENT)
    (DrawText hdc label [text-x text-y text-right text-bottom] dt-format)))


(defn hint-area-wndproc [hwnd msg wparam lparam]
  (case msg

    WM_PAINT
    (let [ps (PAINTSTRUCT)
          hdc (BeginPaint hwnd ps)]
      (if (null? hdc)
        (log/error "BeginPaint failed")
        # else
        (defer
          (EndPaint hwnd ps)
          (when global-hint-state
            (log/debug "global-hint-state = %n" global-hint-state)
            (def {:area-rect client-rect
                  :highlight-rects highlight-rects
                  :line-width default-line-width
                  :hint-list hint-list
                  :label-scale label-scale
                  :label-anchor label-anchor}
              global-hint-state)

            (def colors (in global-hint-state :colors @{}))
            (def text-color (in colors :text 0x505050))
            (def bg-color (in colors :background 0xf5f5f5))
            (def border-color (in colors :border 0x828282))
            (def shadow-color (in colors :shadow 0x828282))
            (def highlight-color (in colors :highlight 0x00a1ff))

            (SelectObject hdc (GetStockObject DC_BRUSH))
            (SelectObject hdc (GetStockObject DC_PEN))

            (def font-cache @{})

            (defer
              (do
                (log/debug "releasing %n cached fonts" (length font-cache))
                (each f font-cache
                  (DeleteObject f)))

              # body
              (each hlr highlight-rects
                (match hlr
                  [[_ _ _ _] color line-width]
                  (draw-highlight-rect hdc
                                       (first hlr)
                                       (if color color highlight-color)
                                       shadow-color
                                       (if line-width line-width default-line-width)
                                       client-rect)

                  [_ _ _ _]
                  (draw-highlight-rect hdc
                                       hlr
                                       highlight-color
                                       shadow-color
                                       default-line-width
                                       client-rect)))

              (def anchor-fn
                (if-let [afn (in TEXT-ANCHOR-FNS label-anchor)]
                  afn
                  (do
                    (log/warning "unknown anchor option %n, using default value" label-anchor)
                    nil)))

              (each [label rect] hint-list
                (draw-label hdc
                            label
                            rect
                            text-color
                            bg-color
                            border-color
                            shadow-color
                            client-rect
                            font-cache
                            label-scale
                            anchor-fn))))))
      0)

    WM_CLOSE
    (do
      (DestroyWindow hwnd)
      0)

    (DefWindowProc hwnd msg wparam lparam)))


(defn create-hint-area-window [key-color win-hbr]
  (def wc
    (WNDCLASSEX
     :style CS_OWNDC
     :lpfnWndProc hint-area-wndproc
     :hInstance (GetModuleHandle nil)
     :lpszClassName HINT-AREA-WINDOW-CLASS-NAME
     :hCursor (LoadCursor nil IDC_ARROW)
     :hbrBackground win-hbr))
  (when (null? (RegisterClassEx wc))
    (errorf "window class registration failed: 0x%x" (GetLastError)))

  (def new-hwnd
    (CreateWindowEx (bor WS_EX_LAYERED
                         WS_EX_TRANSPARENT
                         WS_EX_TOOLWINDOW
                         WS_EX_NOACTIVATE
                         WS_EX_TOPMOST)
                    HINT-AREA-WINDOW-CLASS-NAME
                    ""
                    (bor WS_POPUP)
                    0
                    0
                    100
                    100
                    nil
                    nil (GetModuleHandle nil) nil))
  (when (null? new-hwnd)
    (errorf "failed to create window: 0x%x" (GetLastError)))

  (SetLayeredWindowAttributes new-hwnd key-color 0 LWA_COLORKEY)

  new-hwnd)


(defn handle-set-hint-colors [_hwnd _msg wparam _lparam _hook-handler state]
  (def colors (unmarshal-and-free wparam))
  (if (or (struct? colors)
          (table? colors))
    (let [hint-state (in state :hint-state @{})]
      (put hint-state :colors colors)
      (put state :hint-state hint-state))

    # else
    (log/error "Invalid colors value: %n" colors))
  0)


(defn calc-spanning-rect [hint-list highlight-rects]
  (var ret-left math/int-max)
  (var ret-top  math/int-max)
  (var ret-right  math/int-min)
  (var ret-bottom math/int-min)

  (defn check-rect [r]
    (def [left top right bottom] r)
    (when (< left ret-left)
      (set ret-left left))
    (when (< top ret-top)
      (set ret-top top))
    (when (> right ret-right)
      (set ret-right right))
    (when (> bottom ret-bottom)
      (set ret-bottom bottom)))

  (each [_ r] hint-list
    (check-rect r))
  (each hlr highlight-rects
    (match hlr
      [[_ _ _ _] _ _]
      (check-rect (first hlr))

      [_ _ _ _]
      (check-rect hlr)))

  (when (and (<= ret-left ret-right)
             (<= ret-top ret-bottom))
    {:left ret-left
     :top  ret-top
     :right  ret-right
     :bottom ret-bottom}))


(defn handle-show-hint-area [_hwnd _msg wparam _lparam _hook-handler state]
  (def [hint-list
        label-scale
        label-anchor
        highlight-rects
        line-width]
    (unmarshal-and-free wparam))
  (def hint-state (in state :hint-state @{}))

  (def hint-hwnd
    (if-let [old-hwnd (in hint-state :area-hwnd)]
      old-hwnd
      # else
      (let [colors (in hint-state :colors @{})
            key-color (in colors :key 0x000000)
            win-hbr (CreateSolidBrush key-color)
            new-hwnd (create-hint-area-window key-color win-hbr)]
        (put hint-state :area-hwnd new-hwnd)
        (put hint-state :win-hbr win-hbr)
        new-hwnd)))

  (def spanning-rect (calc-spanning-rect hint-list highlight-rects))
  (log/debug "spanning-rect = %n" spanning-rect)
  (unless spanning-rect
    (log/warning "empty spanning rect for hint-list: %n" hint-list)
    # Early return
    (break 0))

  (put hint-state :area-rect spanning-rect)
  (put hint-state :hint-list hint-list)
  (put hint-state :highlight-rects highlight-rects)
  (put hint-state :label-scale label-scale)
  (put hint-state :label-anchor label-anchor)
  (put hint-state :line-width line-width)
  (put state :hint-state hint-state)
  (set global-hint-state hint-state)

  (def [width height] (rect-size spanning-rect))
  (SetWindowPos hint-hwnd
                HWND_TOPMOST
                (in spanning-rect :left)
                (in spanning-rect :top)
                width
                height
                (bor SWP_NOACTIVATE SWP_NOREDRAW))
  (InvalidateRect hint-hwnd nil true)
  (ShowWindow hint-hwnd SW_SHOW)
  0)


(defn handle-hide-hint-area [_hwnd _msg _wparam _lparam _hook-handler state]
  (def hint-state (in state :hint-state @{}))

  # Clear the states, so that GC can reclaim memory
  # :hint-list and :highlight-rects can be quite large
  # see handle-show-hint-area for the list of saved states
  (put hint-state :area-rect nil)
  (put hint-state :hint-list nil)
  (put hint-state :highlight-rects nil)
  (put hint-state :label-scale nil)
  (put hint-state :label-anchor nil)
  (put hint-state :line-width nil)
  (put state :hint-state hint-state)

  (when-let [hint-hwnd (in hint-state :area-hwnd)]
    (ShowWindow hint-hwnd SW_HIDE))
  0)


(defn handle-cleanup-hint-area [_hwnd msg _wparam _lparam _hook-handler state]
  (def hint-state (in state :hint-state @{}))
  (set global-hint-state nil)
  (put state :hint-state nil)

  (when-let [hint-hwnd (in hint-state :area-hwnd)]
    (DestroyWindow hint-hwnd))

  (def unreg-ret
    (UnregisterClass HINT-AREA-WINDOW-CLASS-NAME (GetModuleHandle nil)))
  (when (= FALSE unreg-ret)
    (log/debug "Failed to unregister hint window class: 0x%x" (GetLastError)))

  (when-let [hbr (in hint-state :win-hbr)]
    (DeleteObject hbr))

  (when-let [custom-msgs (in state :custom-messages)
             cleanup-fn (in custom-msgs msg)]
    (put custom-msgs msg nil))
  0)

################## ^^^^ Runs in UI thread ^^^^ ##################


(defn clip-rect [clipped clipping]
  (def {:left clipped-left
        :top clipped-top
        :right clipped-right
        :bottom clipped-bottom}
    clipped)
  (def {:left clipping-left
        :top clipping-top
        :right clipping-right
        :bottom clipping-bottom}
    clipping)

  {:left   (if (< clipped-left clipping-left)
             clipping-left
             clipped-left)
   :top    (if (< clipped-top clipping-top)
             clipping-top
             clipped-top)
   :right  (if (> clipped-right clipping-right)
             clipping-right
             clipped-right)
   :bottom (if (> clipped-bottom clipping-bottom)
             clipping-bottom
             clipped-bottom)})


#
# ------------------- uia-hinter -------------------
#

(defn- create-uia-hinter-cache-request [uia-man]
  (:create-cache-request
     uia-man
     [UIA_NamePropertyId
      UIA_ClassNamePropertyId
      UIA_FrameworkIdPropertyId
      UIA_NativeWindowHandlePropertyId
      UIA_ControlTypePropertyId
      UIA_BoundingRectanglePropertyId
      UIA_IsInvokePatternAvailablePropertyId
      UIA_IsOffscreenPropertyId
      UIA_IsEnabledPropertyId]
     [UIA_InvokePatternId]))


#
# Firefox keeps all of its offscreen tabs in the UI tree, and FindAll is
# not smart enough to skip them, even if we specify [:property UIA_IsOffscreenPropertyId false]
# in the condition. Traversing complex web pages is reeeeaaaally slow.
# This is to filter out all the offscreen tabs. It should deal with other
# browsers based on Firefox too. E.g. Zen browser.
#
(defn find-firefox-init-uia-elements [uia-win uia-man &opt cr?]
  (log/debug "find-firefox-init-uia-elements for window: %n" (:get_CachedName uia-win))

  (def elem-list @[])

  (with-uia [cr (if cr?
                  (do
                    (:AddRef cr?)
                    cr?)
                  (:create-cache-request uia-man [UIA_ControlTypePropertyId
                                                  UIA_IsOffscreenPropertyId
                                                  UIA_IsEnabledPropertyId]))]
    (with-uia [walker (:create-raw-view-walker uia-man)]
      (:enumerate-children
         uia-man
         uia-win

         (fn [child]  # Top-level children
           (cond
             (= FALSE (:get_CachedIsEnabled child))
             :nop

             (not= FALSE (:get_CachedIsOffscreen child))
             :nop

             (= UIA_GroupControlTypeId (:get_CachedControlType child))
             # For some unknown reason, FindAll* don't work well with
             # [:property UIA_IsControlElementPropertyId false] elements,
             # so we have to walk the tree ourselves.
             (do
               (def group-members @[])
               (:enumerate-children
                  uia-man
                  child

                  (fn [group-child]  # Children in top-level group controls
                    (when (and (not= FALSE (:get_CachedIsEnabled group-child))
                               (= FALSE (:get_CachedIsOffscreen group-child)))
                      (:AddRef group-child)
                      (array/push group-members group-child))
                    true)

                  walker
                  cr)
               (log/debug "found %n on-screen group members" (length group-members))
               (array/concat elem-list group-members))

             true
             (do
               (:AddRef child)
               (array/push elem-list child)))
           true)

         walker
         cr)))

  elem-list)


#
# Some WinUI apps (?) may have top-level elements with identical runtime IDs, for example
# the Photos app from Win 10. This seems to confuse IUIAutomationElement::FindAll and
# friends, causing them to only return the first element with the same runtime ID.
# This function is used to find all top-level elements, whether they have same runtime
# IDs or not.
#
# As stated in Microsoft's docs: "The identifier is only guaranteed to be unique to
# the UI of the desktop on which it was generated. Identifiers can be reused over
# time." (https://learn.microsoft.com/en-us/windows/win32/api/uiautomationclient/nf-uiautomationclient-iuiautomationelement-getruntimeid)
# So the runtime IDs in the same process should all be different from each other, right?
# Is this a bug in UIAutomation or WinUI?
#
(defn find-win-ui-init-uia-elements [uia-win uia-man &opt cr?]
  (log/debug "find-win-ui-init-uia-elements for window: %n" (:get_CachedName uia-win))

  (def elem-list @[])

  (with-uia [cr (if cr?
                  (do
                    (:AddRef cr?)
                    cr?)
                  (:create-cache-request uia-man [UIA_IsOffscreenPropertyId
                                                  UIA_IsEnabledPropertyId]))]
    (:enumerate-children
       uia-man
       uia-win
       (fn [child]
         (cond
           (= FALSE (:get_CachedIsEnabled child))
           :nop

           (not= FALSE (:get_CachedIsOffscreen child))
           :nop

           true
           (do
             (:AddRef child)
             (array/push elem-list child)))
         true)
       nil
       cr))

  (log/debug "find-win-ui-init-uia-elements: found %n children" (length elem-list))
  elem-list)


(defn find-uia-elements [uia-man uia-win cond-spec &opt cr filter-runtime-ids]
  (default filter-runtime-ids false)

  (def init-elems
    (cond
      (and (= UIA_WindowControlTypeId (:get_CachedControlType uia-win))
           (= "Gecko" (:get_CachedFrameworkId uia-win))
           (= "MozillaWindowClass" (:get_CachedClassName uia-win)))
      (find-firefox-init-uia-elements uia-win uia-man cr)

      (= "WinUIDesktopWin32WindowClass" (:get_CachedClassName uia-win))
      (find-win-ui-init-uia-elements uia-win uia-man cr)

      true
      (do
        (:AddRef uia-win)
        @[uia-win])))

  (def elem-list @[])
  # XXX: FindAll sometimes returns duplicate entries for certain UIs
  # (e.g. Copilot), so we use this to filter out those duplicates.
  (def seen-runtime-ids
    (when filter-runtime-ids
      @{}))

  (def collect-unseen
    (fn [found]
      (def runtime-id (:GetRuntimeId found))
      (def seen
        # XXX: Some elements return empty runtime IDs.
        # Treat all empty runtime IDs as unseen.
        (and (not (empty? runtime-id))
             (has-key? seen-runtime-ids runtime-id)))
      (unless seen
        (:AddRef found)
        (put seen-runtime-ids runtime-id true)
        (array/push elem-list found))))

  (def collect-any
    (fn [found]
      (:AddRef found)
      (array/push elem-list found)))

  (def collect
    (if filter-runtime-ids
      collect-unseen
      collect-any))

  (with-uia [cond (:create-condition uia-man cond-spec)]
    (for i 0 (length init-elems)
      (with-uia [e (in init-elems i)]
        (try
          (with-uia [elem-arr (if cr
                                (:FindAllBuildCache e TreeScope_Subtree cond cr)
                                (:FindAll e TreeScope_Subtree cond))]
            (for i 0 (:get_Length elem-arr)
              (with-uia [found (:GetElement elem-arr i)]
                (collect found))))
          ((err fib)
           (log/debug "Failed to find UI elements: %n\n%s"
                      err
                      (get-stack-trace fib)))))))
  elem-list)


(defn uia-hinter-init [self ui-hint]
  (log/debug "-- uia-hinter-init --")

  (def {:cond-spec cond-spec
        :color color
        :show-highlights show-highlights
        :line-width line-width}
    self)
  (def {:context context} ui-hint)
  (def {:uia-manager uia-man
        :window-manager window-man}
    context)

  (when-let [elem-list (in self :elem-list)]
    # Early return
    (break elem-list))

  (def [elem-list win-rect]
    (with-uia [cr (create-uia-hinter-cache-request uia-man)]
      (with-uia [uia-win (:get-focused-window uia-man true cr)]
        (unless uia-win
          # Out of with-uia
          (break [[] nil]))

        (def win-hwnd (:get_CachedNativeWindowHandle uia-win))
        (when (or (nil? win-hwnd)
                  (null? win-hwnd))
          (log/debug "Invalid HWND for uia-hinter: %n" win-hwnd)
          # Out of with-uia
          (break [[] nil]))

        [(find-uia-elements uia-man
                            uia-win
                            # Always ignore disabled and off-screen elements
                            [:and
                             [:property UIA_IsOffscreenPropertyId false]
                             [:property UIA_IsEnabledPropertyId true]
                             cond-spec]
                            cr)
         (:get-hwnd-rect window-man win-hwnd true)])))

  (log/debug "Found %n UI elements" (length elem-list))

  (put self :elem-list elem-list)

  (when (empty? elem-list)
    # Early return
    (break nil))

  (def hl-rects @[])
  (def elems @[])

  (def visit-fn
    (if show-highlights
      (let [[scale-x scale-y] (calc-pixel-scale win-rect)
            box-margins {:left   (math/floor (* line-width scale-x))
                         :top    (math/floor (* line-width scale-y))
                         :right  (math/floor (* line-width scale-x))
                         :bottom (math/floor (* line-width scale-y))}]
        (fn [e]
          (def r (clip-rect (:get_CachedBoundingRectangle e) win-rect))
          (array/push hl-rects (expand-rect r box-margins))
          (array/push elems [r e])))
      # else
      (fn [e]
        (def r (clip-rect (:get_CachedBoundingRectangle e) win-rect))
        (array/push elems [r e]))))

  (each e elem-list
    (visit-fn e))

  {:colors {:background color}
   :line-width line-width
   :highlight-rects hl-rects
   :elements elems})


(defn uia-hinter-select [self elem]
  (log/debug "-- uia-hinter-select --")

  (def {:action action
        :action-handlers action-handlers}
    self)

  (unless (in self :elem-list)
    (log/warning "uia-hinter: selection after cancellation")
    # Early return
    (break))

  (:AddRef elem)

  # Our message loop may get suspended when invoking certain UI elements,
  # e.g. a button that opens a dialog box or a pop-up menu, so we do the
  # clean-up before invoking anything.
  (:cancel self)

  (def handler
    (if (or (function? action)
            (cfunction? action))
      action
      # else
      (in action-handlers action)))

  (if handler
    (try
      (handler self elem)
      ((err fib)
       (log/error "uia-hinter action %n failed for element (%n, %n): %n\n%s"
                  action
                  (:get_CachedName elem)
                  (:get_CachedControlType elem)
                  err
                  (get-stack-trace fib))))
    # else
    (log/error "uia-hinter action not found: %n" action))

  (:Release elem)

  # !!! IMPORTANT
  nil)


(defn- release [elem]
  (def name (:get_CachedName elem))
  (def control-type (:get_CachedControlType elem))
  (def refc (:Release elem))
  (unless (= refc (int/u64 0))
    (log/warning "---- bad ref count: %n (%n, %n)\n%s"
                 refc
                 name
                 control-type
                 (get-stack-trace (fiber/current)))))


(defn uia-hinter-cancel [self]
  (log/debug "-- uia-hinter-cancel --")
  (when-let [elem-list (in self :elem-list)]
    (put self :elem-list nil)
    (each e elem-list
      (release e))))


(def uia-hinter-proto
  @{:init uia-hinter-init
    :select uia-hinter-select
    :cancel uia-hinter-cancel})


(defn get-click-point [target]
  (def [point ret] (:GetClickablePoint target))
  (if (= ret FALSE)
    (do
      (log/debug "failed to get clickable point for (%n, %n), falling back to center point"
                 (:get_CachedName target)
                 (:get_CachedControlType target))
      (rect-center (:get_CachedBoundingRectangle target)))
    # else
    point))


(defn move-mouse-cursor [x y]
  (def spcp-ret (SetPhysicalCursorPos x y))
  (when (= spcp-ret FALSE)
    (errorf "SetPhysicalCursorPos failed: %n" (GetLastError))))


(defn handle-action-invoke [_hinter target]
  (if (not= 0 (:GetCachedPropertyValue target UIA_IsInvokePatternAvailablePropertyId))
    (do
      (try
        # Some elements can't have focus, and this will return 0x80020003
        # (Member not found) for them. We just ignore it here.
        (:SetFocus target)
        ((_err _fib)
         :ignored))
      (with-uia [invoke-pat (:GetCachedPatternAs target
                                                 UIA_InvokePatternId
                                                 IUIAutomationInvokePattern)]
        (:Invoke invoke-pat)))

    # else: No invoke pattern, focus it only
    (:SetFocus target)))


(defn handle-action-focus [_hinter target]
  (:SetFocus target))


(defn handle-action-move-cursor [_hinter target]
  (def rect (:get_CachedBoundingRectangle target))
  (move-mouse-cursor ;(rect-center rect)))


(defn handle-action-click [_hinter target]
  (move-mouse-cursor ;(get-click-point target))
  (send-input (mouse-button-input :left :down)
              (mouse-button-input :left :up)))


(defn handle-action-middle-click [_hinter target]
  (move-mouse-cursor ;(get-click-point target))
  (send-input (mouse-button-input :middle :down)
              (mouse-button-input :middle :up)))


(defn handle-action-right-click [_hinter target]
  (move-mouse-cursor ;(get-click-point target))
  (send-input (mouse-button-input :right :down)
              (mouse-button-input :right :up)))


(defn handle-action-double-click [_hinter target]
  (move-mouse-cursor ;(get-click-point target))
  (send-input (mouse-button-input :left :down)
              (mouse-button-input :left :up)
              (mouse-button-input :left :down)
              (mouse-button-input :left :up)))


(defn uia-hinter [&named condition action color show-highlights line-width]
  (default condition
    [:or
     [:property UIA_IsKeyboardFocusablePropertyId true]
     [:property UIA_IsInvokePatternAvailablePropertyId true]])
  (default action :invoke)
  (default show-highlights false)
  (default line-width 2)

  (table/setproto
   @{:action-handlers @{:invoke       handle-action-invoke
                        :focus        handle-action-focus
                        :move-cursor  handle-action-move-cursor
                        :click        handle-action-click
                        :middle-click handle-action-middle-click
                        :right-click  handle-action-right-click
                        :double-click handle-action-double-click}
     :cond-spec condition
     :action action
     :color color
     :show-highlights show-highlights
     :line-width line-width}
   uia-hinter-proto))


#
# ------------------- gradual-uia-hinter -------------------
#

(defn- create-gradual-uia-hinter-cache-request [uia-man]
  (:create-cache-request
     uia-man
     [UIA_NamePropertyId
      UIA_ClassNamePropertyId
      UIA_NativeWindowHandlePropertyId
      UIA_ControlTypePropertyId
      UIA_BoundingRectanglePropertyId
      UIA_IsInvokePatternAvailablePropertyId
      UIA_IsKeyboardFocusablePropertyId
      UIA_IsOffscreenPropertyId
      UIA_IsEnabledPropertyId]
     [UIA_InvokePatternId]))


(defn filter-valid-children [child-arr]
  (def res @[])
  (for i 0 (:get_Length child-arr)
    (with-uia [e (:GetElement child-arr i)]
      (cond
        (= FALSE (:get_CachedIsEnabled e))
        :nop

        (not= FALSE (:get_CachedIsOffscreen e))
        :nop

        (= "ApplicationFrameInputSinkWindow" (:get_CachedClassName e))
        :nop

        true
        (do
          (:AddRef e)
          (array/push res e)))))
  res)


(defn strip-nested-elements [elem uia-man cr]
  (var cur-elem elem)
  (var cur-children nil)
  (var stop false)

  (:AddRef cur-elem)

  (with-uia [c (:create-condition uia-man :true)]
    (while (not stop)
      (with-uia [child-arr (:FindAllBuildCache cur-elem TreeScope_Children c cr)]
        (set cur-children (filter-valid-children child-arr))
        (cond
          (not= FALSE (:GetCachedPropertyValue cur-elem UIA_IsInvokePatternAvailablePropertyId))
          (set stop true)

          (not= FALSE (:get_CachedIsKeyboardFocusable cur-elem))
          (set stop true)

          (= 0 (length cur-children))
          (set stop true)

          (= 1 (length cur-children))
          (do
            (release cur-elem)
            (set cur-elem (first cur-children)))

          true
          (set stop true)))))

  [cur-elem cur-children])


(defn- get-highlight-color [gradual-uia-hinter elem]
  (def colors (in gradual-uia-hinter :colors))
  (cond
    (not= FALSE
          (:GetCachedPropertyValue
             elem
             UIA_IsInvokePatternAvailablePropertyId))
    (do
      (log/debug "get-highlight-color: element is INVOKABLE")
      (in colors :invokable))

    (not= FALSE (:get_CachedIsKeyboardFocusable elem))
    (do
      (log/debug "get-highlight-color: element is FOCUSABLE")
      (in colors :focusable))

    true
    (do
      (log/debug "get-highlight-color: element is NORMAL")
      # Use ui-hint's default highlight color
      nil)))


(defn- calc-hint-info [gradual-uia-hinter stack]
  (def [root _] (first stack))
  (def [elem children] (last stack))

  (def {:window-manager window-man
        :show-highlights show-highlights
        :line-width line-width}
    gradual-uia-hinter)

  (def orig-elem-rect (:get_CachedBoundingRectangle elem))

  (def [scale-x scale-y] (calc-pixel-scale orig-elem-rect))
  (def box-margins {:left   (math/floor (* line-width scale-x))
                    :top    (math/floor (* line-width scale-y))
                    :right  (math/floor (* line-width scale-x))
                    :bottom (math/floor (* line-width scale-y))})

  (def root-rect
    (if-let [root-hwnd (:get_CachedNativeWindowHandle root)]
      (if (not (null? root-hwnd))
        (:get-hwnd-rect window-man root-hwnd true)
        # else
        (:get_CachedBoundingRectangle root))
      # else
      (:get_CachedBoundingRectangle root)))

  (def elem-rect (clip-rect orig-elem-rect root-rect))

  (def hl-rects @[])
  (def elems @[])

  (def visit-fn
    (if show-highlights
      (fn [c]
        (def r (clip-rect (:get_CachedBoundingRectangle c) root-rect))
        (array/push hl-rects
                    {:rect  (expand-rect r box-margins)
                     :color (get-highlight-color gradual-uia-hinter c)
                     :line-width line-width})
        (array/push elems [r c]))
      (fn [c]
        (def r (clip-rect (:get_CachedBoundingRectangle c) root-rect))
        (array/push elems [r c]))))

  (each c children
    (visit-fn c))

  (def hint-info
    {:highlight-rects [;hl-rects
                       {:rect  (expand-rect elem-rect box-margins)
                        :color (get-highlight-color gradual-uia-hinter elem)
                        :line-width line-width}]
     :elements elems})

  (log/debug "calc-hint-info: hint-info = %n" hint-info)
  hint-info)


(defn gradual-uia-hinter-init [self ui-hint]
  (log/debug "-- gradual-uia-hinter-init --")

  (def {:context context} ui-hint)
  (def {:uia-manager uia-man
        :window-manager window-man}
    context)

  (when-let [stack (in self :stack)]
    (unless (empty? stack)
      # Early return
      (break (calc-hint-info self stack))))

  (with-uia [cr (create-gradual-uia-hinter-cache-request uia-man)]
    (with-uia [uia-win (:get-focused-window uia-man true cr)]
      (unless uia-win
        # Out of with-uia
        (break nil))

      (def [elem children] (strip-nested-elements uia-win uia-man cr))
      (put self :stack @[[elem children]])
      (put self :uia-manager uia-man)
      (put self :window-manager window-man)

      (calc-hint-info self (in self :stack)))))


(defn gradual-uia-hinter-select [self elem]
  (log/debug "-- gradual-uia-hinter-select --")

  (def {:uia-manager uia-man
        :stack stack}
    self)

  (unless (in self :stack)
    (log/warning "gradual-uia-hinter: selection after cancellation")
    # Early return
    (break))

  (def [stripped-elem children]
    (with-uia [cr (create-gradual-uia-hinter-cache-request uia-man)]
      (strip-nested-elements elem uia-man cr)))
  (array/push stack [stripped-elem children])
  (calc-hint-info self stack))


(defn pop-gradual-uia-hinter-stack [stack]
  (if (> (length stack) 1)
    (do
      (def [last-elem last-children] (array/pop stack))
      (each c last-children
        (release c))
      (release last-elem)
      true)
    false))


(defn gradual-uia-hinter-return [self]
  (log/debug "-- gradual-uia-hinter-return --")
  (def {:stack stack} self)
  (pop-gradual-uia-hinter-stack stack)
  (calc-hint-info self stack))


(defn gradual-uia-hinter-confirm [self]
  (log/debug "-- gradual-uia-hinter-confirm --")

  (when-let [top (last (in self :stack))]
    (def [elem children] top)
    (def {:action action
          :action-handlers action-handlers}
      self)

    (:AddRef elem)
    (:cancel self)

    (def handler
      (if (or (function? action)
              (cfunction? action))
        action
        # else
        (in action-handlers action)))

    (if handler
      (try
        (handler self elem)
        ((err fib)
         (log/error "gradual-uia-hinter action %n failed for element (%n, %n): %n\n%s"
                    action
                    (:get_CachedName elem)
                    (:get_CachedControlType elem)
                    err
                    (get-stack-trace fib))))
      # else
      (log/error "gradual-uia-hinter action not found: %n" action))

    (:Release elem)

    # !!! IMPORTANT
    nil))


(defn gradual-uia-hinter-cancel [self]
  (log/debug "-- gradual-uia-hinter-cancel --")
  (when-let [stack (in self :stack)]
    (put self :stack nil)
    (each [elem children] stack
      (release elem)
      (each c children
        (release c)))))


(def gradual-uia-hinter-proto
  @{:init gradual-uia-hinter-init
    :select gradual-uia-hinter-select
    :return gradual-uia-hinter-return
    :confirm gradual-uia-hinter-confirm
    :cancel gradual-uia-hinter-cancel})


(defn gradual-uia-hinter [&named action colors show-highlights line-width]
  (default action :invoke)
  (default colors {})
  (default show-highlights false)
  (default line-width 2)

  (def default-colors
    {:invokable 0x30e400
     :focusable 0xffbf66})

  (table/setproto
   @{:action-handlers @{:invoke       handle-action-invoke
                        :focus        handle-action-focus
                        :move-cursor  handle-action-move-cursor
                        :click        handle-action-click
                        :middle-click handle-action-middle-click
                        :right-click  handle-action-right-click
                        :double-click handle-action-double-click}
     :action action
     :colors (merge default-colors colors)
     :show-highlights show-highlights
     :line-width line-width}
   gradual-uia-hinter-proto))


#
# ------------------- frame-hinter -------------------
#

(defn frame-hinter-init [self ui-hint]
  (log/debug "-- frame-hinter-init --")

  (def {:scale scale
        :color color
        :anchor anchor
        :show-highlights show-highlights
        :line-width line-width}
    self)
  (def {:context context} ui-hint)
  (def {:window-manager window-man} context)

  (def cur-lo (get-in window-man [:root :current-child]))
  (unless cur-lo
    (break []))

  (def frame-list (:get-all-leaf-frames cur-lo))

  {:label-scale scale
   :label-anchor anchor
   :colors {:background color}
   :elements (map |(tuple (in $ :rect) $) frame-list)
   :line-width (when show-highlights line-width)
   :highlight-rects (when show-highlights (map |(in $ :rect) frame-list))})


(defn frame-hinter-select [self fr]
  (log/debug "-- frame-hinter-select --")

  (def {:action-fn action-fn} self)
  (try
    (action-fn fr)
    ((err fib)
     (log/error "action for frame-hinter failed: %n\n%s"
                err
                (get-stack-trace fib))))
  # !!! IMPORTANT
  nil)


(def frame-hinter-proto
  @{:init frame-hinter-init
    :select frame-hinter-select})


(defn frame-hinter [&named action-fn scale color anchor show-highlights line-width]
  (default action-fn
    (fn [fr]
      (let [wm (:get-window-manager fr)]
        (with-activation-hooks wm
          (:set-focus wm fr)))))
  (default scale 3)
  (default anchor :top-left)
  (default show-highlights false)
  (default line-width 2)

  (table/setproto
   @{# Default settings
     :action-fn action-fn
     :scale scale
     :color color
     :anchor anchor
     :show-highlights show-highlights
     :line-width line-width}
   frame-hinter-proto))


#
# ------------------- ui-hint -------------------
#

(def ui-hint-settings
  @{:label-scale true
    :label-anchor true
    :colors (fn [old new] (merge old new))
    :line-width true})


(def UI-HINT-KEYS-PEG
  (peg/compile ~(at-least 2 (choice (range "az") (range "AZ") (range "09")))))

(defn normalize-key-list [key-list]
  (unless (or (string? key-list)
              (buffer? key-list)
              (symbol? key-list)
              (keyword? key-list))
    (errorf "invalid key list: %n" key-list))

  (def unique-keys @"")
  (each k key-list
    (unless (find |(= $ k) unique-keys)
      (buffer/push-byte unique-keys k)))
  (unless (peg/match UI-HINT-KEYS-PEG unique-keys)
    (errorf "invalid key list: %n" key-list))
  (string/ascii-upper unique-keys))


(defn calc-label-len [elem-count key-count]
  (max 1 (math/ceil (/ (math/log elem-count) (math/log key-count)))))


(defn make-label-coro [key-list &opt next-coro]
  (fn []
    (coro
     (each k key-list
       (if next-coro
         (let [c (next-coro)]
           (while (def n (resume c))
             (def buf (buffer/new-filled 1 k))
             (buffer/push buf n)
             (yield buf)))
         (yield (buffer/new-filled 1 k)))))))


(defn make-label-coro-for-label-len [label-len key-list]
  (var coro-ctor nil)
  (for i 0 label-len
    (set coro-ctor (make-label-coro key-list coro-ctor)))
  coro-ctor)


(defn generate-labels [elem-list key-list]
  (def label-len (calc-label-len (length elem-list) (length key-list)))
  (def coro-ctor (make-label-coro-for-label-len label-len key-list))
  (def label-coro (coro-ctor))
  (def labeled @{})
  (each e elem-list
    (put labeled (resume label-coro) e))
  labeled)


(defn filter-hint-labels [current-keys labeled-elems]
  (def current-keys-len (length current-keys))
  (def filtered @{})

  (eachp [l e] labeled-elems
    (when (string/has-prefix? current-keys l)
      (def new-label (slice l current-keys-len))
      (put filtered new-label e)))

  filtered)


(defn ui-hint-clean-up [self &opt cancel?]
  (default cancel? true)

  (def {:context context
        :hinter hinter
        :labeled-elems labeled-elems
        :hook-fn hook-fn
        :hide-msg hide-msg}
    self)
  (def {:hook-manager hook-man
        :ui-manager ui-man
        :key-manager key-man}
    context)

  (:set-key-mode key-man :command)
  (:remove-hook hook-man :key-pressed hook-fn)
  (:post-message ui-man hide-msg 0 0)

  (when cancel?
    (when-let [cancel-fn (in hinter :cancel)]
      (try
        (cancel-fn hinter)
        ((err fib)
         (log/error ":cancel method from hinter failed: %n\n%s"
                    err
                    (get-stack-trace fib))))))

  (put self :hinter nil)
  (put self :key-list nil)
  (put self :labeled-elems nil)
  (put self :current-keys nil)
  (put self :hook-fn nil)
  (when-let [orig-settings (in self :orig-settings)]
    (put self :orig-settings nil)
    (merge-settings self orig-settings)))


(defn ui-hint-handle-hint-info [self hint-info]
  (log/debug "ui-hint-handle-hint-info: hint-info = %n" hint-info)

  # Always reset to original settings before merging the
  # override values from the hinter
  (when-let [orig-settings (in self :orig-settings)]
    (merge-settings self orig-settings))

  (cond
    (nil? hint-info)
    # The hinter finished its work
    (:clean-up self false)

    (indexed? hint-info)
    (do
      (def elem-list hint-info)
      (if (empty? elem-list)
        # The hinter finished its work
        (:clean-up self false)
        # else, carry on
        (do
          (put self :labeled-elems (generate-labels elem-list (in self :key-list)))
          (put self :current-keys @"")
          (:show-hints self (in self :labeled-elems)))))

    (or (table? hint-info)
        (struct? hint-info))
    # The hinter returned new hints and override settings
    (do
      (def override-settings hint-info)
      (def elem-list (in hint-info :elements []))
      (def highlight-rects (in hint-info :highlight-rects))
      (if (and (empty? elem-list)
               (or (nil? highlight-rects)
                   (empty? highlight-rects)))
        # The hinter finished its work
        (:clean-up self false)
        # else, rinse and repeat
        (do
          (put self :labeled-elems (generate-labels elem-list (in self :key-list)))
          (put self :current-keys @"")
          (def old-settings (merge-settings self override-settings ui-hint-settings))
          # Only save and restore the initial settings
          (unless (in self :orig-settings)
            (put self :orig-settings old-settings))
          (:show-hints self (in self :labeled-elems) highlight-rects))))

    true
    (do
      # XXX: Raise an error?
      (log/warning "hinter returned: %n" hint-info)
      (:clean-up self true))))


(defn ui-hint-process-filter-result [self filtered]
  (def {:hinter hinter} self)

  (case (length filtered)
    1
    # Reached the target
    (let [target (in filtered (first (keys filtered)))]
      (def t1 (os/clock :monotonic))
      (def hint-info (:select hinter (last target)))
      (def t2 (os/clock :monotonic))
      (log/debug "(:select hinter ...) took %n seconds" (- t2 t1))
      (:handle-hint-info self hint-info))

    0
    # The prefix does not exist
    (if-let [return-fn (in hinter :return)]
      # The hinter can return to a previous step
      (do
        (def t1 (os/clock :monotonic))
        (def hint-info (return-fn hinter))
        (def t2 (os/clock :monotonic))
        (log/debug "(:return hinter) took %n seconds" (- t2 t1))
        (:handle-hint-info self hint-info))
      # else, clean up, also cancel the hinter
      (:clean-up self true))

    # Other values mean that we still have multiple choices, wait
    # for more input.
    (:show-hints self filtered)))


(defn- normalize-rect [rect]
  (cond
    (or (table? rect)
        (struct? rect))
    [(in rect :left)
     (in rect :top)
     (in rect :right)
     (in rect :bottom)]

    (indexed? rect)
    (if (= 4 (length rect))
      rect
      (errorf "invalid rect: %n" rect))

    true
    (errorf "invalid rect: %n" rect)))


(defn ui-hint-show-hints [self labeled &opt highlight-rects]
  (default highlight-rects [])

  (def {:context context
        :colors colors
        :show-msg show-msg
        :colors-msg colors-msg
        :label-scale label-scale
        :label-anchor label-anchor
        :line-width line-width}
    self)
  (def {:ui-manager ui-man} context)

  (when colors
    (:post-message ui-man colors-msg (alloc-and-marshal colors) 0))

  (def to-show @[])
  (eachp [l e] labeled
    (array/push to-show [l (normalize-rect (first e))]))
  (def hl-info-list
    (map (fn [hlr]
           (if (has-key? hlr :rect)
             [(normalize-rect (in hlr :rect)) (in hlr :color) (in hlr :line-width)]
             # else
             (normalize-rect hlr)))
         highlight-rects))
  (:post-message ui-man
                 show-msg
                 (alloc-and-marshal [to-show
                                     label-scale
                                     label-anchor
                                     hl-info-list
                                     line-width])
                 0))


(defn ui-hint-cmd [self raw-key-list &opt hinter]
  (default hinter (uia-hinter))

  (when (in self :hook-fn)
    # Another :ui-hint command is in progress, early return
    # See :clean-up method for clearing :hook-fn
    (log/debug "aborting nested :ui-hint command")
    (break))

  (def normalized-keys (normalize-key-list raw-key-list))

  (def {:context context
        :show-msg show-msg}
    self)
  (def
    {:hook-manager hook-man
     :key-manager key-man
     :ui-manager ui-man
     :uia-manager uia-man}
    context)

  (def t1 (os/clock :monotonic))
  (def hint-info (:init hinter self))
  (def t2 (os/clock :monotonic))
  (log/debug "(:init hinter ...) took %n seconds" (- t2 t1))

  (def [override-settings elem-list]
    (cond
      (nil? hint-info)
      [{} []]

      (indexed? hint-info)
      [{} hint-info]
      
      (or (table? hint-info)
          (struct? hint-info))
      [hint-info (in hint-info :elements [])]

      true
      (errorf ":init method from hinter returned invalid value: %n" hint-info)))
  (def highlight-rects (in override-settings :highlight-rects))

  (when (and (empty? elem-list)
             (or (nil? highlight-rects)
                 (empty? highlight-rects)))
    (:show-tooltip ui-man :ui-hint "No matching UI element.")
    # Early return
    (break))

  (put self :hinter hinter)
  (put self :key-list normalized-keys)
  (put self :labeled-elems (generate-labels elem-list (in self :key-list)))
  (put self :current-keys @"")
  (put self :orig-settings (merge-settings self override-settings ui-hint-settings))

  (def hook-fn
    (:add-hook hook-man :key-pressed
       (fn [key]
         (:on-key-pressed self key))))
  (put self :hook-fn hook-fn)

  (:set-key-mode key-man :raw)

  (:show-hints self (in self :labeled-elems) highlight-rects))


(defn ui-hint-on-key-pressed [self key]
  (def {:hinter hinter
        :key-list key-list
        :current-keys current-keys
        :labeled-elems labeled-elems}
    self)
  (def key-code (in key :key))
  # TOOD: Modifiers?

  (cond
    (find |(= $ key-code) key-list)
    # A valid key is pressed
    (do
      (buffer/push-byte current-keys key-code)
      (def filtered (filter-hint-labels current-keys labeled-elems))
      (:process-filter-result self filtered))

    (or (= key-code VK_BACK)
        (= key-code VK_DELETE))
    (do
      (if (empty? current-keys)
        (when-let [return-fn (in hinter :return)]
          (def hint-info (return-fn hinter))
          (:handle-hint-info self hint-info))
        # else
        (do
          (buffer/popn current-keys 1)
          (def filtered (filter-hint-labels current-keys labeled-elems))
          (:process-filter-result self filtered))))

    (= key-code VK_RETURN)
    (if-let [confirm-fn (in hinter :confirm)]
      (let [hint-info (confirm-fn hinter)]
        (:handle-hint-info self hint-info))
      # else
      (:clean-up self true))

    (= key-code VK_ESCAPE)
    (:clean-up self true)))


(defn ui-hint-enable [self]
  (:disable self)

  (def {:context context} self)
  (def {:ui-manager ui-man
        :command-manager command-man}
    context)

  (def show-msg (:add-custom-message ui-man handle-show-hint-area))
  (when (< show-msg (int/s64 0))
    (error "failed to register show-hint-area message"))
  (def hide-msg (:add-custom-message ui-man handle-hide-hint-area))
  (when (< hide-msg (int/s64 0))
    (error "failed to register hide-hint-area message"))
  (def colors-msg (:add-custom-message ui-man handle-set-hint-colors))
  (when (< colors-msg (int/s64 0))
    (error "failed to register set-hint-colors message"))

  (put self :show-msg show-msg)
  (put self :hide-msg hide-msg)
  (put self :colors-msg colors-msg)

  (:add-command command-man :ui-hint
     (fn [key-list &opt hinter]
       (:cmd self key-list hinter))
     ```
     (:ui-hint key-list &opt hinter)

     Shows hints on the UI of the current window, and waits for user's
     input. Key-list should be a string that contains all possible
     letters which can be used to generate labels for the hints. Hinter
     should be an object implementing the :init and :select methods.

     (:init hinter ui-hint) should return a tuple or array, containing
     elements in the form of [rect elem], where rect specifies where to
     draw a hint label, and elem will the passed back to
     (:select hinter elem), when a user selects that label.

     (:select hinter elem) should process the selected element, then
     return nil, or a new list of [rect elem]. When nil is returned, the
     :ui-hint command ends. When a list is returned, new labels will be
     shown for the elements in that list.
     ```))


(defn ui-hint-disable [self]
  (def {:context context
        :show-msg show-msg
        :hide-msg hide-msg
        :colors-msg colors-msg}
    self)
  (def {:ui-manager ui-man
        :command-manager command-man}
    context)

  (:remove-command command-man :ui-hint)

  (when (in self :hook-fn)
    # A :ui-hint command is in progress, abort it
    (log/debug "ui-hint-disable when :ui-hint command is in progress")
    (:clean-up self))

  (when show-msg
    (:remove-custom-message ui-man show-msg)
    (put self :show-msg nil))
  (when hide-msg
    (:remove-custom-message ui-man hide-msg)
    (put self :hide-msg nil))
  (when colors-msg
    (:remove-custom-message ui-man colors-msg)
    (put self :colors-msg nil))

  (def cleanup-msg
    (:add-custom-message ui-man handle-cleanup-hint-area))
  (if (< cleanup-msg (int/s64 0))
    (log/warning "failed to clean up hwnd")
    # else
    (:send-message ui-man cleanup-msg 0 0)))


(def ui-hint-proto
  @{:on-key-pressed ui-hint-on-key-pressed

    :cmd ui-hint-cmd
    :handle-hint-info ui-hint-handle-hint-info
    :show-hints ui-hint-show-hints
    :process-filter-result ui-hint-process-filter-result
    :clean-up ui-hint-clean-up
    :enable ui-hint-enable
    :disable ui-hint-disable})


(defn ui-hint [context]
  (table/setproto
   @{:context context

     # Default settings
     :label-scale 1
     :label-anchor :left
     :colors @{:text 0x505050
               :background 0xf5f5f5
               :border 0x828282
               :shadow 0x828282
               :highlight 0x00a1ff
               :key 0x000000}
     :line-width 2
    }
   ui-hint-proto))
