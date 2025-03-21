(use jw32/_winuser)
(use jw32/_wingdi)
(use jw32/_libloaderapi)
(use jw32/_errhandlingapi)
(use jw32/_uiautomation)
(use jw32/_util)

(use jwno/util)

(import jwno/log)


################## vvvv Runs in UI thread vvvv ##################

(def HINT-AREA-WINDOW-CLASS-NAME "jwno-gradual-hint-area-window")
(var global-hint-state nil)


(defn draw-elem-rect [hdc rect border-color scale-x scale-y]
  (def orig-pen (SelectObject hdc (GetStockObject NULL_PEN)))
  (SetDCBrushColor hdc border-color)

  (def line-width 4)
  (def half-line-width (brshift line-width 1))

  (def [left top right bottom] rect)

  (defer
    (SelectObject hdc orig-pen)

    (do
      (Rectangle hdc
                 (- left half-line-width)
                 (- top half-line-width)
                 (+ left half-line-width)
                 (+ bottom half-line-width))
      (Rectangle hdc
                 (- left half-line-width)
                 (- top half-line-width)
                 (+ right half-line-width)
                 (+ top half-line-width))
      (Rectangle hdc
                 (- right half-line-width)
                 (- top half-line-width)
                 (+ right half-line-width)
                 (+ bottom half-line-width))
      (Rectangle hdc
                 (- left half-line-width)
                 (- bottom half-line-width)
                 (+ right half-line-width)
                 (+ bottom half-line-width)))))


(defn draw-label [hdc label rect text-color bg-color border-color shadow-color scale-x scale-y]
  (def dt-format (bor DT_SINGLELINE DT_NOCLIP))
  (def [height text-rect] (DrawText hdc label [0 0 0 0] (bor dt-format DT_CALCRECT)))
  (when (>= 0 height)
    (error "DrawText DT_CALCRECT failed"))
  (def {:right text-width
        :bottom text-height}
    text-rect)

  (def padding 2)
  (def padding-x (math/floor (* padding scale-x)))
  (def padding-y (math/floor (* padding scale-y)))

  (def shadow-offset 2)
  (def shadow-offset-x (math/floor (* shadow-offset scale-x)))
  (def shadow-offset-y (math/floor (* shadow-offset scale-y)))

  (def [left top right bottom] rect)

  (def text-x left)
  (def text-y (brshift (+ top bottom (- text-height)) 1))
  (def text-right (+ text-x text-width))
  (def text-bottom (+ text-y text-height))

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
  (DrawText hdc label [text-x text-y text-right text-bottom] dt-format))


(defn create-font [scale]
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
  hfont)


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
                  :elem-rect elem-rect
                  :hint-list hint-list}
              global-hint-state)

            (def colors (in global-hint-state :colors @{}))
            (def text-color (in colors :text 0x505050))
            (def bg-color (in colors :background 0xf5f5f5))
            (def border-color (in colors :border 0x828282))
            (def shadow-color (in colors :shadow 0x828282))

            (def [scale-x scale-y] (calc-pixel-scale client-rect))
            (def font (create-font scale-y))
            (def orig-font (SelectObject hdc font))

            (def offset-x (in client-rect :left))
            (def offset-y (in client-rect :top))

            (defer
              (do
                (SelectObject hdc orig-font)
                (DeleteObject font))
              (do
                (SelectObject hdc (GetStockObject DC_BRUSH))
                (SelectObject hdc (GetStockObject DC_PEN))

                (draw-elem-rect hdc
                                [(- (in elem-rect :left) offset-x)
                                 (- (in elem-rect :top) offset-y)
                                 (- (in elem-rect :right) offset-x)
                                 (- (in elem-rect :bottom) offset-y)]
                                border-color
                                scale-x
                                scale-y)

                (each [label rect] hint-list
                  (draw-label hdc
                              label
                              [(- (in rect 0) offset-x)
                               (- (in rect 1) offset-y)
                               (- (in rect 2) offset-x)
                               (- (in rect 3) offset-y)]
                              text-color
                              bg-color
                              border-color
                              shadow-color
                              scale-x
                              scale-y)))))))
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


(defn handle-show-hint-area [_hwnd _msg wparam _lparam _hook-handler state]
  (def [root-rect elem-rect hint-list] (unmarshal-and-free wparam))
  (def hint-state (in state :gradual-hint-state @{}))

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

  (put hint-state :area-rect root-rect)
  (put hint-state :elem-rect elem-rect)
  (put hint-state :hint-list hint-list)
  (put state :gradual-hint-state hint-state)
  (set global-hint-state hint-state)

  (ShowWindow hint-hwnd SW_SHOW)
  (InvalidateRect hint-hwnd nil true)
  (UpdateWindow hint-hwnd)

  (def [width height] (rect-size root-rect))
  (SetWindowPos hint-hwnd
                HWND_TOPMOST
                (in root-rect :left)
                (in root-rect :top)
                width
                height
                (bor SWP_NOACTIVATE))
  0)


(defn handle-hide-hint-area [_hwnd _msg _wparam _lparam _hook-handler state]
  (def hint-state (in state :gradual-hint-state @{}))
  (when-let [hint-hwnd (in hint-state :area-hwnd)]
    (ShowWindow hint-hwnd SW_HIDE))
  0)


(defn handle-cleanup-hint-area [_hwnd msg _wparam _lparam _hook-handler state]
  (def hint-state (in state :gradual-hint-state @{}))
  (set global-hint-state nil)
  (put state :gradual-hint-state nil)

  (when-let [hint-hwnd (in hint-state :area-hwnd)]
    (DestroyWindow hint-hwnd))

  (def unreg-ret
    (UnregisterClass HINT-AREA-WINDOW-CLASS-NAME (GetModuleHandle nil)))
  (when (= FALSE unreg-ret)
    (log/debug "Failed to unregister hint window class: 0x%x" (GetLastError)))

  (when-let [hbr (in hint-state :win-hbr)]
    (DeleteObject hbr))
  (when-let [hfont (in hint-state :font)]
    (DeleteObject hfont))

  (when-let [custom-msgs (in state :custom-messages)
             cleanup-fn (in custom-msgs msg)]
    (put custom-msgs msg nil))
  0)


################## ^^^^ Runs in UI thread ^^^^ ##################

(defn release [elem]
  (def name (:get_CachedName elem))
  (def control-type (:get_CachedControlType elem))
  (def refc (:Release elem))
  (unless (= refc (int/u64 0))
    (log/warning "bad ref count: %n (%n, %n)\n%s"
                 refc
                 name
                 control-type
                 (get-stack-trace (fiber/current)))))


(defn pop-stack [stack]
  (if (> (length stack) 1)
    (do
      (def [last-elem last-children _last-labels] (array/pop stack))
      (each c last-children
        (release c))
      (release last-elem)
      true)
    false))


(defn filter-valid-children [child-arr]
  (def res @[])
  (for i 0 (:get_Length child-arr)
    (with-uia [e (:GetElement child-arr i)]
      (when (and (not= FALSE (:get_CachedIsEnabled e))
                 (= FALSE (:get_CachedIsOffscreen e)))
        (:AddRef e)
        (array/push res e))))
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
          #(not= FALSE (:GetCachedPropertyValue cur-elem UIA_IsInvokePatternAvailablePropertyId))
          #(set stop true)

          (= 0 (length cur-children))
          (set stop true)

          (= 1 (length cur-children))
          (do
            (release cur-elem)
            (set cur-elem (first cur-children)))

          true
          (set stop true)))))

  [cur-elem cur-children])


(defn normalize-key-list [key-list]
  (string/ascii-upper key-list))


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


(defn handle-action-invoke [elem]
  :todo)


(defn gradual-ui-hint-cmd [self raw-key-list &opt action]
  (default action :invoke)

  (when (in self :hook-fn)
    (log/debug "aborting nested :gradual-ui-hint command")
    (break))

  (def {:context context
        :uia-cr cr}
    self)
  (def
    {:hook-manager hook-man
     :key-manager key-man
     :ui-manager ui-man
     :uia-manager uia-man}
    context)

  (with-uia [uia-win (:get-focused-window uia-man true cr)]
    (when uia-win
      (def [elem children] (strip-nested-elements uia-win uia-man cr))
      (put self :key-list (normalize-key-list raw-key-list))
      (put self
           :stack
           @[
             [elem
              children
              (generate-labels (seq [i :range [0 (length children)]] i) (in self :key-list))]
            ])
      (put self :current-keys @"")
      (put self :action action)

      (def hook-fn
        (:add-hook hook-man :key-pressed
           (fn [key]
             (:on-key-pressed self key))))
      (put self :hook-fn hook-fn)

      (:set-key-mode key-man :raw)

      (:update-hints self))))


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


(defn gradual-ui-hint-update-hints [self]
  (def {:context context
        :current-keys current-keys
        :stack stack}
    self)

  (def {:uia-manager uia-man
        :ui-manager ui-man}
    context)

  (def [elem children labels] (last stack))
  (def [root-elem & _] (first stack))

  (def current-keys-len (length current-keys))
  (def filtered-labels @{})
  (eachp [l i] labels
    (when (string/has-prefix? current-keys l)
      (def new-label (slice l current-keys-len))
      (put filtered-labels new-label i)))
  (log/debug "---- current-keys = %n" current-keys)
  (log/debug "---- filtered-labels = %n" filtered-labels)

  (cond
    (or
      # Always show the hint visuals if there's no input, even
      # when there's only a single child, to prevent the code
      # from skipping those children without siblings.
      (empty? current-keys)
      (< 1 (length filtered-labels)))
    (do
      (def root-rect (:get_CachedBoundingRectangle root-elem))
      (def elem-rect (clip-rect (:get_CachedBoundingRectangle elem) root-rect))
      (def to-show @[])
      (eachp [l i] filtered-labels
        (def child (in children i))
        (def rect
          (clip-rect (:get_CachedBoundingRectangle child) root-rect))
        (def labeled-rect
          [l
           [(in rect :left)
            (in rect :top)
            (in rect :right)
            (in rect :bottom)]])
        (log/debug "---- labeled-rect = %n" labeled-rect)
        (array/push to-show labeled-rect))
      (:post-message ui-man
                     (in self :show-msg)
                     (alloc-and-marshal [root-rect elem-rect to-show])
                     0))

    (= 1 (length filtered-labels))
    (do
      (def target-idx (in filtered-labels (first (keys filtered-labels))))
      (def child (in children target-idx))
      (def [stripped grand-children] (strip-nested-elements child uia-man (in self :uia-cr)))
      (if (empty? grand-children)
        (do
          (:clean-up self)
          # TODO
          (try
            (:SetFocus stripped)
            ((err fib)
             (log/debug "---- SetFocus failed: %n\n%s"
                        err
                        (get-stack-trace fib)))))
        # else
        (do
          (array/push stack
                      [stripped
                       grand-children
                       (generate-labels (seq [i :range [0 (length grand-children)]] i)
                                        (in self :key-list))])
          (buffer/clear current-keys)
          (:update-hints self))))

    (= 0 (length filtered-labels))
    # go back to the upper level
    (if (pop-stack stack)
      (:update-hints self)
      (if (= 1 (length stack))
        # We reached the top of the stack, cancel everything
        (:clean-up self)))))


(defn gradual-ui-hint-on-key-pressed [self key]
  (def {:key-list key-list
        :current-keys current-keys
        :stack stack}
    self)
  (def key-code (in key :key))

  (cond
    (find |(= $ key-code) key-list)
    (do
      (buffer/push-byte current-keys key-code)
      (:update-hints self))

    (or (= key-code VK_BACK)
        (= key-code VK_DELETE))
    (cond
      (>= 0 (length current-keys))
      # Go back to the upper level, but don't pop the starting element
      (when (pop-stack stack)
        (:update-hints self))

      true
      (do
        (buffer/popn current-keys 1)
        (:update-hints self)))

    (or (= key-code VK_ESCAPE)
        (= key-code VK_RETURN))
    (:clean-up self)))


(defn gradual-ui-hint-clean-up [self]
  (def {:context context
        :stack stack
        :current-keys current-keys
        :hook-fn hook-fn
        :hide-msg hide-msg}
    self)

  (def {:hook-manager hook-man
        :ui-manager ui-man
        :key-manager key-man}
    context)

  (:set-key-mode key-man :command)

  (when hook-fn
    (put self :hook-fn nil)
    (:remove-hook hook-man :key-pressed hook-fn))

  (when stack
    (put self :stack nil)
    (each [elem children _labels] stack
      (release elem)
      (each c children
        (release c))))

  (buffer/clear current-keys)

  (:post-message ui-man hide-msg 0 0))


(defn gradual-ui-hint-enable [self]
  (:disable self)

  (def {:context context} self)
  (def {:ui-manager ui-man
        :uia-manager uia-man
        :command-manager command-man}
    context)

  (put self
       :uia-cr
       (:create-cache-request uia-man
                              [UIA_NativeWindowHandlePropertyId
                               UIA_NamePropertyId
                               UIA_ControlTypePropertyId
                               UIA_BoundingRectanglePropertyId
                               UIA_IsInvokePatternAvailablePropertyId
                               UIA_IsOffscreenPropertyId
                               UIA_IsEnabledPropertyId]
                              [UIA_InvokePatternId]))

  (def show-msg (:add-custom-message ui-man handle-show-hint-area))
  (when (< show-msg (int/s64 0))
    (error "failed to register show-hint-area message"))
  (def hide-msg (:add-custom-message ui-man handle-hide-hint-area))
  (when (< hide-msg (int/s64 0))
    (error "failed to register hide-hint-area message"))

  (put self :show-msg show-msg)
  (put self :hide-msg hide-msg)

  (:add-command command-man :gradual-ui-hint
     (fn [key-list &opt action]
       (:cmd self key-list action))))


(defn gradual-ui-hint-disable [self]
  (def {:context context
        :uia-cr uia-cr
        :show-msg show-msg
        :hide-msg hide-msg}
    self)
  (def {:ui-manager ui-man
        :command-manager command-man}
    context)

  (:remove-command command-man :gradual-ui-hint)

  (when uia-cr
    (put self :uia-cr nil)
    (:Release uia-cr))

  (when (in self :hook-fn)
    (log/debug "gradual-ui-hint-disable when :gradual-ui-hint command is in progress")
    (:clean-up self))

  (when show-msg
    (:remove-custom-message ui-man show-msg)
    (put self :show-msg nil))
  (when hide-msg
    (:remove-custom-message ui-man hide-msg)
    (put self :hide-msg nil))

  (def cleanup-msg
    (:add-custom-message ui-man handle-cleanup-hint-area))
  (if (< cleanup-msg (int/s64 0))
    (log/warning "failed to clean up hwnd")
    # else
    (:send-message ui-man cleanup-msg 0 0)))


(def gradual-ui-hint-proto
  @{:on-key-pressed gradual-ui-hint-on-key-pressed

    :cmd gradual-ui-hint-cmd
    :update-hints gradual-ui-hint-update-hints
    :clean-up gradual-ui-hint-clean-up
    :enable gradual-ui-hint-enable
    :disable gradual-ui-hint-disable})


(defn gradual-ui-hint [context]
  (table/setproto
   @{:context context
     :action-handlers @{:invoke handle-action-invoke}

     # Default settings
     :colors @{:text 0x505050
               :background 0xf5f5f5
               :border 0x828282
               :shadow 0x828282
               :key 0x000000}
    }
   gradual-ui-hint-proto))
