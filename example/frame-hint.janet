#
# frame-hint.janet (WIP)
#
# An experimental way of selecting a frame, which works very much
# like the UI Hint feature: you type an on-screen label to select
# the corresponding leaf frame. Should be more ergonomic than
# directional frame selection commands, when dealing with a far-away
# frame.
#
# To try it out:
#
#   (import frame-hint)
#   (def fh (frame-hint/frame-hint jwno/context))
#   (:enable fh)
#
# Then bind the command, for example:
#
#   (:define-key my-keymap "RCtrl" [:frame-hint "abcdefgh"])
#   (:set-keymap (in jwno/context :key-manager) my-keymap)
#

(use jw32/_winuser)
(use jw32/_wingdi)
(use jw32/_libloaderapi)
(use jw32/_errhandlingapi)
(use jw32/_util)

(import jwno/log)
(use jwno/util)


################## vvvv Runs in UI thread vvvv ##################

(def FRAME-HINT-AREA-WINDOW-CLASS-NAME "jwno-frame-hint-area-window")
(var global-frame-hint-state nil)


(defn create-font [scale]
  # We simply copy a system font, and apply the scaling here
  (def [spi-ret spi-ncm] (SystemParametersInfo SPI_GETNONCLIENTMETRICS 0 nil 0))
  (when (= FALSE spi-ret)
    (errorf "SystemParametersInfo failed: 0x%x" (GetLastError)))
  (def cap-font (in spi-ncm :lfCaptionFont))
  (def hfont (CreateFont (math/floor (* 5 scale (in cap-font :lfHeight)))
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


(defn draw-label [hdc label rect text-color bg-color border-color shadow-color client-rect]
  (def [scale-x scale-y] (calc-pixel-scale rect))

  (log/debug "---- scale-x = %n, scale-y = %n" scale-x scale-y)

  (def hfont (create-font scale-y))
  (def orig-hfont (SelectObject hdc hfont))

  (defer
    (do
      (SelectObject hdc orig-hfont)
      (DeleteObject hfont))

    (do
      (def dt-format (bor DT_SINGLELINE DT_NOCLIP))
      (def [height text-rect] (DrawText hdc label [0 0 0 0] (bor dt-format DT_CALCRECT)))
      (when (>= 0 height)
        (error "DrawText DT_CALCRECT failed"))
      (def {:right text-width
            :bottom text-height}
        text-rect)

      (def padding 10)
      (def padding-x (math/floor (* padding scale-x)))
      (def padding-y (math/floor (* padding scale-y)))

      (def shadow-offset 4)
      (def shadow-offset-x (math/floor (* shadow-offset scale-x)))
      (def shadow-offset-y (math/floor (* shadow-offset scale-y)))

      (def offset-x (in client-rect :left))
      (def offset-y (in client-rect :top))
      (def left   (- (in rect 0) offset-x))
      (def top    (- (in rect 1) offset-y))
      (def right  (- (in rect 2) offset-x))
      (def bottom (- (in rect 3) offset-y))

      (def text-x left)
      # TODO
      (def text-y
        top
        #(brshift (+ top bottom (- text-height)) 1)
        )
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
      (DrawText hdc label [text-x text-y text-right text-bottom] dt-format))))


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
          (when global-frame-hint-state
            (log/debug "global-frame-hint-state = %n" global-frame-hint-state)
            (def {:area-rect client-rect
                  :hint-list hint-list}
              global-frame-hint-state)

            (def colors (in global-frame-hint-state :colors @{}))
            (def text-color (in colors :text 0x505050))
            (def bg-color (in colors :background 0xf5f5f5))
            (def border-color (in colors :border 0x828282))
            (def shadow-color (in colors :shadow 0x828282))

            (do
              (SelectObject hdc (GetStockObject DC_BRUSH))
              (SelectObject hdc (GetStockObject DC_PEN))

              (each [label rect] hint-list
                (draw-label hdc
                            label
                            rect
                            text-color
                            bg-color
                            border-color
                            shadow-color
                            client-rect))))))
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
     :lpszClassName FRAME-HINT-AREA-WINDOW-CLASS-NAME
     :hCursor (LoadCursor nil IDC_ARROW)
     :hbrBackground win-hbr))
  (when (null? (RegisterClassEx wc))
    (errorf "window class registration failed: 0x%x" (GetLastError)))

  (def new-hwnd
    (CreateWindowEx (bor WS_EX_LAYERED
                         WS_EX_TOOLWINDOW
                         WS_EX_NOACTIVATE
                         WS_EX_TOPMOST)
                    FRAME-HINT-AREA-WINDOW-CLASS-NAME
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
  (def [rect hint-list] (unmarshal-and-free wparam))
  (def hint-state (in state :frame-hint-state @{}))

  (def hint-hwnd
    (if-let [old-hwnd (in hint-state :area-hwnd)]
      old-hwnd
      #else
      (let [colors (in hint-state :colors @{})
            key-color (in colors :key 0x000000)
            win-hbr (CreateSolidBrush key-color)
            new-hwnd (create-hint-area-window key-color win-hbr)]
        (put hint-state :area-hwnd new-hwnd)
        (put hint-state :win-hbr win-hbr)
        new-hwnd)))

  (put hint-state :area-rect rect)
  (put hint-state :hint-list hint-list)
  (put state :frame-hint-state hint-state)
  (set global-frame-hint-state hint-state)

  (ShowWindow hint-hwnd SW_SHOW)
  (InvalidateRect hint-hwnd nil true)
  (UpdateWindow hint-hwnd)

  (def [width height] (rect-size rect))
  (SetWindowPos hint-hwnd
                HWND_TOPMOST
                (in rect :left)
                (in rect :top)
                width
                height
                (bor SWP_NOACTIVATE))
  0)


(defn handle-hide-hint-area [_hwnd _msg _wparam _lparam _hook-handler state]
  (def hint-state (in state :frame-hint-state @{}))
  (when-let [hint-hwnd (in hint-state :area-hwnd)]
    (ShowWindow hint-hwnd SW_HIDE))
  0)


(defn handle-cleanup-hint-area [_hwnd msg _wparam _lparam _hook-handler state]
  (def hint-state (in state :frame-hint-state @{}))
  (set global-frame-hint-state nil)
  (put state :frame-hint-state nil)

  (when-let [hint-hwnd (in hint-state :area-hwnd)]
    (DestroyWindow hint-hwnd))

  (def unreg-ret
    (UnregisterClass FRAME-HINT-AREA-WINDOW-CLASS-NAME (GetModuleHandle nil)))
  (when (= FALSE unreg-ret)
    (log/debug "Failed to unregister hint window class: 0x%x" (GetLastError)))

  (when-let [hbr (in hint-state :win-hbr)]
    (DeleteObject hbr))

  (when-let [custom-msgs (in state :custom-messages)
             cleanup-fn (in custom-msgs msg)]
    (put custom-msgs msg nil))
  0)


################## ^^^^ Runs in UI thread ^^^^ ##################

(defn get-all-leaf-frames [node &opt arr]
  (default arr @[])

  (def children (in node :children))
  (cond
    (= :window (in node :type))
    arr

    (not= :frame (in node :type)) # it's another container type
    (do
      (each c children
        (:get-all-leaf-frames c arr))
      arr)

    (empty? children)
    (do
      (array/push arr node)
      arr)

    (= :window (in (first children) :type))
    (do
      (array/push arr node)
      arr)

    true # children are other frames
    (do
      (each c children
        (get-all-leaf-frames c arr))
      arr)))


(defn get-desktop-rect [lo]
  (def rect @{:left 0 :top 0 :right 0 :bottom 0})
  (each c (in lo :children)
    (def cr (get-in c [:monitor :work-area]))
    (when (< (in cr :left) (in rect :left))
      (put rect :left (in cr :left)))
    (when (< (in cr :top) (in rect :top))
      (put rect :top (in cr :top)))
    (when (> (in cr :right) (in rect :right))
      (put rect :right (in cr :right)))
    (when (> (in cr :bottom) (in rect :bottom))
      (put rect :bottom (in cr :bottom))))
  rect)


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


(defn filter-hint-labels [current-keys labeled-frames]
  (def current-keys-len (length current-keys))
  (def filtered @{})

  (eachp [l e] labeled-frames
    (when (string/has-prefix? current-keys l)
      (def new-label (slice l current-keys-len))
      (put filtered new-label e)))

  filtered)


(defn frame-hint-cmd [self raw-key-list &opt action]
  (when (in self :hook-fn)
    (log/debug "aborting nested :frame-hint command")
    (break))

  (def {:context context
        :show-msg show-msg}
    self)
  (def
    {:window-manager window-man
     :hook-manager hook-man
     :key-manager key-man
     :ui-manager ui-man}
    context)

  (def cur-lo (get-in window-man [:root :current-child]))
  (unless cur-lo
    (break))

  (def frame-list (get-all-leaf-frames cur-lo))
  (log/debug "---- found %n leaf frames" (length frame-list))
  (def desktop-rect (get-desktop-rect cur-lo))
  (log/debug "---- desktop-rect = %n" desktop-rect)

  (when (or (>= 0 (rect-width desktop-rect))
            (>= 0 (rect-height desktop-rect)))
    (log/debug "---- null rect for frame-hint?")
    (break))

  (put self :key-list (normalize-key-list raw-key-list))
  (put self :labeled-frames (generate-labels frame-list (in self :key-list)))
  (put self :current-keys @"")
  (put self :win-rect desktop-rect)
  (put self :action action)

  (def hook-fn
    (:add-hook hook-man :key-pressed
       (fn [key]
         (:on-key-pressed self key))))
  (put self :hook-fn hook-fn)

  (:set-key-mode key-man :raw)

  (:show-hints self (in self :labeled-frames)))


(defn frame-hint-on-key-pressed [self key]
  (def {:key-list key-list
        :current-keys current-keys
        :labeled-frames labeled-frames}
    self)
  (def key-code (in key :key))

  (cond
    (find |(= $ key-code) key-list)
    # A valid key is pressed
    (do
      (buffer/push-byte current-keys key-code)
      (def filtered (filter-hint-labels current-keys labeled-frames))
      (:process-filter-result self filtered))

    (or (= key-code VK_BACK)
        (= key-code VK_DELETE))
    (do
      (buffer/popn current-keys 1)
      (def filtered (filter-hint-labels current-keys labeled-frames))
      (:process-filter-result self filtered))

    (or (= key-code VK_ESCAPE)
        (= key-code VK_RETURN))
    (:clean-up self)))


(defn frame-hint-process-filter-result [self filtered]
  (def {:context context
        :action action}
    self)
  (def {:window-manager window-man} context)

  (case (length filtered)
    1
    (let [target (in filtered (first (keys filtered)))]
      (:clean-up self)

      (cond
        (or (function? action)
            (cfunction? action))
        (action target)

        true
        (with-activation-hooks window-man
          (:set-focus window-man target))))

    0
    (:clean-up self)

    (:show-hints self filtered)))


(defn frame-hint-show-hints [self labeled]
  (def {:context context
        :win-rect win-rect
        :show-msg show-msg}
    self)
  (def {:ui-manager ui-man} context)

  (def to-show @[])
  (eachp [l fr] labeled
    (def rect (in fr :rect))
    (array/push to-show
                [l
                 [(in rect :left)
                  (in rect :top)
                  (in rect :right)
                  (in rect :bottom)]]))

  (:post-message ui-man show-msg (alloc-and-marshal [win-rect to-show]) 0))


(defn frame-hint-enable [self]
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

  (put self :show-msg show-msg)
  (put self :hide-msg hide-msg)

  (:add-command command-man :frame-hint
     (fn [key-list &opt action]
       (:cmd self key-list action))))


(defn frame-hint-disable [self]
  (def {:context context
        :show-msg show-msg
        :hide-msg hide-msg
        :colors-msg colors-msg}
    self)
  (def {:ui-manager ui-man
        :command-manager command-man}
    context)

  (:remove-command command-man :frame-hint)

  (when (in self :hook-fn)
    (log/debug "frame-hint-disable when :frame-hint command is in progress")
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
    (log/warning "failed to clean up frame hint")
    # else
    (:send-message ui-man cleanup-msg 0 0)))


(defn frame-hint-clean-up [self]
  (def {:context context
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
  (put self :hook-fn nil)
  (put self :labeled-frames nil))


(def frame-hint-proto
  @{:on-key-pressed frame-hint-on-key-pressed

    :cmd frame-hint-cmd
    :show-hints frame-hint-show-hints
    :process-filter-result frame-hint-process-filter-result
    :clean-up frame-hint-clean-up
    :enable frame-hint-enable
    :disable frame-hint-disable})


(defn frame-hint [context]
  (table/setproto
   @{:context context}
   frame-hint-proto))
