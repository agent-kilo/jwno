(use jw32/_winuser)
(use jw32/_wingdi)
(use jw32/_libloaderapi)
(use jw32/_errhandlingapi)
(use jw32/_uiautomation)
(use jw32/_dwmapi)
(use jw32/_util)

(use ./util)

(import ./log)


################## vvvv Runs in UI thread vvvv ##################

(def HINT-AREA-WINDOW-CLASS-NAME "jwno-hint-area-window")
(var global-hint-state nil)


(defn to-client-coords [rect-tuple client-rect]
  (def offset-x (in client-rect :left))
  (def offset-y (in client-rect :top))
  (def [left top right bottom] rect-tuple)
  {:left (- left offset-x)
   :top (- top offset-y)
   :right (- right offset-x)
   :bottom (- bottom offset-y)})


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

            # Text settings
            (def font (in global-hint-state :font))
            (SelectObject hdc font)
            (SetBkMode hdc OPAQUE)

            (def dt-format (bor DT_SINGLELINE DT_VCENTER DT_NOCLIP))
            (def offset 2)
            (def text-color 0x00505050)
            (def bk-color 0x00f5f5f5)
            (def shadow-color 0x00828282)

            (each [label rect] hint-list
              (def [left top right bottom] rect)
              (SetTextColor hdc shadow-color)
              (SetBkColor hdc shadow-color)
              (DrawText hdc label [(+ left offset) (+ top offset) (+ right offset) (+ bottom offset)] dt-format)

              (SetTextColor hdc text-color)
              (SetBkColor hdc bk-color)
              (DrawText hdc label rect dt-format)))))
      0)

    WM_CLOSE
    (do
      (DestroyWindow hwnd)
      0)

    (DefWindowProc hwnd msg wparam lparam)))


(defn create-hint-area-window []
  (def wc
    (WNDCLASSEX
     :style CS_OWNDC
     :lpfnWndProc hint-area-wndproc
     :hInstance (GetModuleHandle nil)
     :lpszClassName HINT-AREA-WINDOW-CLASS-NAME
     :hCursor (LoadCursor nil IDC_ARROW)
     :hbrBackground (+ 1 COLOR_WINDOW)
     ))
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

  (SetLayeredWindowAttributes new-hwnd (GetSysColor (int/to-number COLOR_WINDOW)) 0 LWA_COLORKEY)
  (try
    # Raises E_INVALIDARG on Windows 10
    (DwmSetWindowAttribute new-hwnd DWMWA_WINDOW_CORNER_PREFERENCE DWMWCP_ROUND)
    ((err _fib)
     (log/debug "DwmSetWindowAttribute failed: %n" err)))

  new-hwnd)


(defn create-font []
  # We simply copy a system font here
  (def [spi-ret spi-ncm] (SystemParametersInfo SPI_GETNONCLIENTMETRICS 0 nil 0))
  (when (= FALSE spi-ret)
    (errorf "SystemParametersInfo failed: 0x%x" (GetLastError)))
  (def cap-font (in spi-ncm :lfCaptionFont))
  (def hfont (CreateFont (in cap-font :lfHeight)
                         (in cap-font :lfWidth)
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


(defn handle-show-hint-area [_hwnd _msg wparam _lparam _hook-handler state]
  (def [rect hint-list] (unmarshal-and-free wparam))
  (def hint-state (in state :hint-state @{}))

  (def hint-hwnd
    (if-let [old-hwnd (in hint-state :area-hwnd)]
      old-hwnd
      # else
      (let [new-hwnd (create-hint-area-window)]
        (put hint-state :area-hwnd new-hwnd)
        new-hwnd)))

  (when (nil? (in hint-state :font))
    (put hint-state :font (create-font)))
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

  (when-let [hfont (in hint-state :font)]
    (DeleteObject hfont))

  (when-let [custom-msgs (in state :custom-messages)
             cleanup-fn (in custom-msgs msg)]
    (put custom-msgs msg nil))
  0)

################## ^^^^ Runs in UI thread ^^^^ ##################


(defn normalize-key-list [key-list]
  (string/ascii-upper key-list))


(defn make-condition [uia-com spec]
  (match spec
    [:property prop-id prop-val]
    (:CreatePropertyCondition uia-com prop-id prop-val)

    [:and & spec-list]
    (do
      (def cond-list (map |(make-condition uia-com $) spec-list))
      (def ret (:CreateAndConditionFromArray uia-com cond-list))
      (each c cond-list
        (:Release c))
      ret)

    [:or & spec-list]
    (do
      (def cond-list (map |(make-condition uia-com $) spec-list))
      (def ret (:CreateOrConditionFromArray uia-com cond-list))
      (each c cond-list
        (:Release c))
      ret)

    [:not spec]
    (do
      (def orig-cond (make-condition uia-com spec))
      (def ret (:CreateNotCondition uia-com orig-cond))
      (:Release orig-cond)
      ret)

    _
    (errorf "unknown condition spec: %n" spec)))


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
    (:Release e))
  (:post-message ui-man hide-msg 0 0))


(defn ui-hint-process-filter-result [self filtered]
  (def {:context context
        :current-keys current-keys}
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
      (if (not= 0 (:GetCachedPropertyValue target UIA_IsInvokePatternAvailablePropertyId))
        (try
          (do
            (:SetFocus target)
            (with-uia [invoke-pat (:GetCachedPatternAs target
                                                       UIA_InvokePatternId
                                                       IUIAutomationInvokePattern)]
              (:Invoke invoke-pat)))
          ((err _fib)
           (log/debug "failed to invoke UI element: %n, name: %n, control type: %n"
                      err
                      (:get_CachedName target)
                      (:get_CachedControlType target))))

        # No invoke pattern, focus it only
        (try
          (:SetFocus target)
          ((err _fib)
           (log/debug "failed to focus UI element: %n, name: %n, control type: %n"
                      err
                      (:get_CachedName target)
                      (:get_CachedControlType target)))))
      (:Release target))

    0
    # The prefix does not exist, clean up so we don't confuse the user
    (:clean-up self)

    # Other values mean that we still have multiple choices, wait
    # for more input.
    (:show-hints self filtered)))


(defn ui-hint-show-hints [self labeled]
  (def {:context context
        :win-rect win-rect
        :show-msg show-msg}
    self)
  (def {:ui-manager ui-man} context)

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


(defn ui-hint-cmd [self raw-key-list &opt cond-spec]
  (default cond-spec
    [:or
     [:property UIA_IsKeyboardFocusablePropertyId true]
     [:property UIA_IsInvokePatternAvailablePropertyId true]])

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

  (def elem-list @[])
  (var win-rect nil)

  (with-uia [uia-win (:get-focused-window uia-man)]
    (when uia-win
      (def win-hwnd (:get_CachedNativeWindowHandle uia-win))
      (when (or (nil? win-hwnd)
                (null? win-hwnd))
        (log/debug "Invalid HWND for ui-hint: %n" win-hwnd)
        (break))
      (set win-rect (DwmGetWindowAttribute win-hwnd DWMWA_EXTENDED_FRAME_BOUNDS))

      # XXX: Always ignore disabled and off-screen elements
      (with-uia [cond (make-condition uia-com [:and
                                                 [:property UIA_IsOffscreenPropertyId false]
                                               [:property UIA_IsEnabledPropertyId true]
                                               cond-spec])]
        (with-uia [cr (:CreateCacheRequest uia-com)]
          (:AddProperty cr UIA_NamePropertyId)
          (:AddProperty cr UIA_ControlTypePropertyId)
          (:AddProperty cr UIA_BoundingRectanglePropertyId)
          (:AddProperty cr UIA_IsInvokePatternAvailablePropertyId)
          (:AddPattern cr UIA_InvokePatternId)

          (with-uia [elem-arr (:FindAllBuildCache uia-win TreeScope_Subtree cond cr)]
            (for i 0 (:get_Length elem-arr)
              (array/push elem-list (:GetElement elem-arr i))))))))

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
  (when (< show-msg (int/s64 0))
    (error "failed to register hide-frame-area message"))

  (put self :show-msg show-msg)
  (put self :hide-msg hide-msg)

  (:add-command command-man :ui-hint
     (fn [key-list &opt cond-spec] (:cmd self key-list cond-spec))
     ```
     (:ui-hint key-list &opt cond-spec)

     Shows hints on the UI of the current window, and waits for user's
     input. Key-list should be a string that contains all possible
     letters which can be used to generate labels for the hints. Once
     a label is entered by the user, Tries to activate the labeled UI
     element. When the UI element is not activatable, set focus to it
     instead.
     ```))


(defn ui-hint-disable [self]
  (def {:context context
        :show-msg show-msg
        :hide-msg hide-msg}
    self)
  (def {:ui-manager ui-man
        :command-manager command-man}
    context)

  (:remove-command command-man :ui-hint)

  (when show-msg
    (:remove-custom-message ui-man show-msg)
    (put self :show-msg nil))
  (when hide-msg
    (:remove-custom-message ui-man hide-msg)
    (put self :show-msg nil))

  (def cleanup-msg
    (:add-custom-message ui-man handle-cleanup-hint-area))
  (if (< cleanup-msg (int/s64 0))
    (log/warning "failed to clean up hwnd")
    # else
    (:send-message ui-man cleanup-msg 0 0)))


(def ui-hint-proto
  @{:on-key-pressed ui-hint-on-key-pressed

    :cmd ui-hint-cmd
    :show-hints ui-hint-show-hints
    :process-filter-result ui-hint-process-filter-result
    :clean-up ui-hint-clean-up
    :enable ui-hint-enable
    :disable ui-hint-disable})


(defn ui-hint [context]
  (table/setproto
   @{:context context}
   ui-hint-proto))
