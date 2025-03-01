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

            (defer
              (do
                (SelectObject hdc orig-font)
                (DeleteObject font))
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


(defn handle-show-hint-area [_hwnd _msg wparam _lparam _hook-handler state]
  (def [rect hint-list] (unmarshal-and-free wparam))
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

  (put hint-state :area-rect rect)
  (put hint-state :hint-list hint-list)
  (put state :hint-state hint-state)
  (set global-hint-state hint-state)

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
  (def hint-state (in state :hint-state @{}))
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
  (when-let [hfont (in hint-state :font)]
    (DeleteObject hfont))

  (when-let [custom-msgs (in state :custom-messages)
             cleanup-fn (in custom-msgs msg)]
    (put custom-msgs msg nil))
  0)

################## ^^^^ Runs in UI thread ^^^^ ##################


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


(defn filter-hint-labels [current-keys labeled-elems]
  (def current-keys-len (length current-keys))
  (def filtered @{})

  (eachp [l e] labeled-elems
    (when (string/has-prefix? current-keys l)
      (def new-label (slice l current-keys-len))
      (put filtered new-label e)))

  filtered)


(defn ui-hint-clean-up [self]
  (def {:context context
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
  (eachp [l e] labeled-elems
    (def name (:get_CachedName e))
    (def control-type (:get_CachedControlType e))
    (def refc (:Release e))
    (unless (= refc (int/u64 0))
      (log/warning "bad ref count in ui-hint-clean-up: %n (%n, %n)" refc name control-type)))
  (:post-message ui-man hide-msg 0 0)

  (put self :hook-fn nil)
  (put self :labeled-elems nil))


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


(defn handle-action-invoke [_ui-hint target]
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


(defn handle-action-focus [_ui-hint target]
  (:SetFocus target))


(defn handle-action-move-cursor [_ui-hint target]
  (def rect (:get_CachedBoundingRectangle target))
  (move-mouse-cursor ;(rect-center rect)))


(defn handle-action-click [_ui-hint target]
  (move-mouse-cursor ;(get-click-point target))
  (send-input (mouse-button-input :left :down)
              (mouse-button-input :left :up)))


(defn handle-action-middle-click [_ui-hint target]
  (move-mouse-cursor ;(get-click-point target))
  (send-input (mouse-button-input :middle :down)
              (mouse-button-input :middle :up)))


(defn handle-action-right-click [_ui-hint target]
  (move-mouse-cursor ;(get-click-point target))
  (send-input (mouse-button-input :right :down)
              (mouse-button-input :right :up)))


(defn handle-action-double-click [_ui-hint target]
  (move-mouse-cursor ;(get-click-point target))
  (send-input (mouse-button-input :left :down)
              (mouse-button-input :left :up)
              (mouse-button-input :left :down)
              (mouse-button-input :left :up)))


(defn ui-hint-process-filter-result [self filtered]
  (def {:context context
        :current-keys current-keys
        :action action
        :action-handlers action-handlers}
    self)
  (def {:ui-manager ui-man}
    context)

  (case (length filtered)
    1
    # Reached the target
    (let [target (in filtered (first (keys filtered)))]
      (:AddRef target)
      # Our message loop may get suspended when invoking certain UI elements,
      # e.g. a button that opens a dialog box or a pop-up menu, so we send
      # the clean up messages before invoking anything.
      (:clean-up self)

      (cond
        (or (function? action)
            (cfunction? action))
        # Custom action
        :todo

        true
        (when-let [handler (in action-handlers action)]
          (try
            (handler self target)
            ((err fib)
             (log/error "ui-hint action %n failed for element (%n, %n): %n\n%s"
                        action
                        (:get_CachedName target)
                        (:get_CachedControlType target)
                        err
                        (get-stack-trace fib))))))

      (:Release target))

    0
    # The prefix does not exist, clean up so we don't confuse the user
    (:clean-up self)

    # Other values mean that we still have multiple choices, wait
    # for more input.
    (:show-hints self filtered)))


(defn ui-hint-show-hints [self labeled]
  (def {:context context
        :colors colors
        :win-rect win-rect
        :show-msg show-msg
        :colors-msg colors-msg}
    self)
  (def {:ui-manager ui-man} context)

  (when colors
    (:post-message ui-man colors-msg (alloc-and-marshal colors) 0))

  (def offset-x (in win-rect :left))
  (def offset-y (in win-rect :top))
  (def to-show @[])
  (eachp [l e] labeled
    (def rect (:get_CachedBoundingRectangle e))
    (array/push to-show
                [l
                 [(- (in rect :left) offset-x)
                  (- (in rect :top) offset-y)
                  (- (in rect :right) offset-x)
                  (- (in rect :bottom) offset-y)]]))
  (:post-message ui-man show-msg (alloc-and-marshal [win-rect to-show]) 0))


#
# Firefox keeps all of its offscreen tabs in the UI tree, and FindAll is
# not smart enough to skip them, even if we specify [:property UIA_IsOffscreenPropertyId false]
# in the condition. Traversing complex web pages is reeeeaaaally slow.
# This is to filter out all the offscreen tabs. It should deal with other
# browsers based on Firefox too. E.g. Zen browser.
#
(defn find-firefox-init-elements [uia-win uia-man &opt cr?]
  (log/debug "find-firefox-init-elements for window: %n" (:get_CachedName uia-win))

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
             (= FALSE (:GetCachedPropertyValue child UIA_IsEnabledPropertyId))
             :nop

             (not= FALSE (:GetCachedPropertyValue child UIA_IsOffscreenPropertyId))
             :nop

             (= UIA_GroupControlTypeId (:GetCachedPropertyValue child UIA_ControlTypePropertyId))
             # For some unknown reason, FindAll* don't work well with
             # [:property UIA_IsControlElementPropertyId false] elements,
             # so we have to walk the tree ourselves.
             (do
               (def group-members @[])
               (:enumerate-children
                  uia-man
                  child
                  (fn [group-child]  # Children in top-level group controls
                    (when (and (not= FALSE (:GetCachedPropertyValue group-child UIA_IsEnabledPropertyId))
                               (= FALSE (:GetCachedPropertyValue group-child UIA_IsOffscreenPropertyId)))
                      (:AddRef group-child)
                      (array/push group-members group-child)))
                  walker
                  cr)
               (log/debug "found %n on-screen group members" (length group-members))
               (array/concat elem-list group-members))

             true
             (do
               (:AddRef child)
               (array/push elem-list child))))
         walker
         cr)))

  elem-list)


(defn ui-hint-find-ui-elements [self uia-win cond-spec &opt cr]
  (def {:context context} self)
  (def {:uia-manager uia-man} context)

  (def init-elems
    (cond
      (and (= UIA_WindowControlTypeId (:get_CachedControlType uia-win))
           (= "Gecko" (:get_CachedFrameworkId uia-win))
           (= "MozillaWindowClass" (:get_CachedClassName uia-win)))
      (find-firefox-init-elements uia-win uia-man cr)

      true
      @[uia-win]))

  (def elem-list @[])
  # XXX: FindAll sometimes returns duplicate entries for certain UIs
  # (e.g. Copilot), so we use this to filter out those duplicates.
  (def seen-runtime-ids @{})

  (with-uia [cond (:create-condition uia-man cond-spec)]
    # XXX: e is leaking
    (each e init-elems
      (with-uia [elem-arr (if cr
                            (:FindAllBuildCache e TreeScope_Subtree cond cr)
                            (:FindAll e TreeScope_Subtree cond))]
        (for i 0 (:get_Length elem-arr)
          (with-uia [found (:GetElement elem-arr i)]
            (def runtime-id (:GetRuntimeId found))
            (def seen
              # XXX: Some elements return empty runtime IDs
              (and (not (empty? runtime-id))
                   (has-key? seen-runtime-ids runtime-id)))
            (unless seen
              (:AddRef found)
              (put seen-runtime-ids runtime-id true)
              (array/push elem-list found)))))))
  elem-list)


(defn ui-hint-cmd [self raw-key-list &opt cond-spec action]
  (default cond-spec
    [:or
     [:property UIA_IsKeyboardFocusablePropertyId true]
     [:property UIA_IsInvokePatternAvailablePropertyId true]])
  (default action :invoke)

  (when (in self :hook-fn)
    # Another :ui-hint command is in progress, early return
    # See :clean-up method for clearing :hook-fn
    (log/debug "aborting nested :ui-hint command")
    (break))

  (def {:context context
        :show-msg show-msg}
    self)
  (def
    {:hook-manager hook-man
     :key-manager key-man
     :ui-manager ui-man
     :uia-manager uia-man}
    context)

  (def uia-com (in uia-man :com))

  (var elem-list @[])
  (var win-rect nil)

  (with-uia [cr (:create-cache-request uia-man
                                       [UIA_NamePropertyId
                                        UIA_ClassNamePropertyId
                                        UIA_FrameworkIdPropertyId
                                        UIA_NativeWindowHandlePropertyId
                                        UIA_ControlTypePropertyId
                                        UIA_BoundingRectanglePropertyId
                                        UIA_IsInvokePatternAvailablePropertyId
                                        UIA_IsOffscreenPropertyId
                                        UIA_IsEnabledPropertyId]
                                       [UIA_InvokePatternId])]
    (with-uia [uia-win (:get-focused-window uia-man true cr)]
      (when uia-win
        (def win-hwnd (:get_CachedNativeWindowHandle uia-win))
        (when (or (nil? win-hwnd)
                  (null? win-hwnd))
          (log/debug "Invalid HWND for ui-hint: %n" win-hwnd)
          (break))

        (set win-rect (DwmGetWindowAttribute win-hwnd DWMWA_EXTENDED_FRAME_BOUNDS))
        (set elem-list
             (:find-ui-elements
                self
                uia-win
                # XXX: Always ignore disabled and off-screen elements
                [:and
                 [:property UIA_IsOffscreenPropertyId false]
                 [:property UIA_IsEnabledPropertyId true]
                 cond-spec]
                cr)))))

  (def elem-count (length elem-list))
  (log/debug "Found %n UI elements" elem-count)

  (cond
    (nil? win-rect)
    (do
      (:show-tooltip ui-man :ui-hint "Invalid window.")
      (break)) # early return

    (>= 0 elem-count)
    (do
      (:show-tooltip ui-man :ui-hint "No matching UI element.")
      (break)) # early return
    )

  (put self :key-list (normalize-key-list raw-key-list))
  (put self :labeled-elems (generate-labels elem-list (in self :key-list)))
  (put self :current-keys @"")
  (put self :win-rect win-rect)
  (put self :action action)

  (def hook-fn
    (:add-hook hook-man :key-pressed
       (fn [key]
         (:on-key-pressed self key))))
  (put self :hook-fn hook-fn)

  (:set-key-mode key-man :raw)

  (:show-hints self (in self :labeled-elems)))


(defn ui-hint-on-key-pressed [self key]
  (def {:context context
        :key-list key-list
        :current-keys current-keys
        :labeled-elems labeled-elems
        :win-rect win-rect
        :hook-fn hook-fn}
    self)
  (def {:ui-manager ui-man} context)
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
      (buffer/popn current-keys 1)
      (def filtered (filter-hint-labels current-keys labeled-elems))
      (:process-filter-result self filtered))

    (or (= key-code VK_ESCAPE)
        (= key-code VK_RETURN))
    (:clean-up self)))


(defn ui-hint-enable [self]
  (:disable self)

  (def {:context context} self)
  (def {:ui-manager ui-man
        :command-manager command-man}
    context)

  (def show-msg (:add-custom-message ui-man handle-show-hint-area))
  (when (< show-msg (int/s64 0))
    (error "failed to register show-frame-area message"))
  (def hide-msg (:add-custom-message ui-man handle-hide-hint-area))
  (when (< hide-msg (int/s64 0))
    (error "failed to register hide-frame-area message"))
  (def colors-msg (:add-custom-message ui-man handle-set-hint-colors))
  (when (< colors-msg (int/s64 0))
    (error "failed to register set-hint-colors message"))

  (put self :show-msg show-msg)
  (put self :hide-msg hide-msg)
  (put self :colors-msg colors-msg)

  (:add-command command-man :ui-hint
     (fn [key-list &opt cond-spec action]
       (:cmd self key-list cond-spec action))
     ```
     (:ui-hint key-list &opt cond-spec action)

     Shows hints on the UI of the current window, and waits for user's
     input. Key-list should be a string that contains all possible
     letters which can be used to generate labels for the hints. Once
     a label is entered by the user, tries to carry out the specified
     action.
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
    :find-ui-elements ui-hint-find-ui-elements
    :show-hints ui-hint-show-hints
    :process-filter-result ui-hint-process-filter-result
    :clean-up ui-hint-clean-up
    :enable ui-hint-enable
    :disable ui-hint-disable})


(defn ui-hint [context]
  (table/setproto
   @{:context context
     :action-handlers @{:invoke handle-action-invoke
                        :focus handle-action-focus
                        :move-cursor handle-action-move-cursor
                        :click handle-action-click
                        :middle-click handle-action-middle-click
                        :right-click handle-action-right-click
                        :double-click handle-action-double-click
                        :prompt :todo}

     # Default settings
     :colors @{:text 0x505050
               :background 0xf5f5f5
               :border 0x828282
               :shadow 0x828282
               :key 0x000000}
    }
   ui-hint-proto))
