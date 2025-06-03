(use jw32/_winuser)
(use jw32/_wingdi)
(use jw32/_libloaderapi)
(use jw32/_errhandlingapi)
(use jw32/_dwmapi)
(use jw32/_util)

(import ./log)

(use ./util)


# ================== Show a tooltip on the current frame ==================
#
# Shows a simple tooltip when a frame is activated. To use it, add these
# in your config:
#
#     (def current-frame-tooltip (indicator/current-frame-tooltip jwno/context))
#     # The text shown in the tooltip. Defaults to "Current Frame"
#     (put current-frame-tooltip :text "Current Frame")
#     # How long the tooltip will be shown, in milliseconds. Defaults to 1500
#     (put current-frame-tooltip :timeout 1500)
#     (:enable current-frame-tooltip)
#
# To stop it:
#
#     (:disable current-frame-tooltip)
#

(defn current-frame-tooltip-on-frame-activated [self fr]
  (def {:ui-manager ui-man
        :text text}
    self)
  (def rect (in fr :rect))
  (def center-x (brshift (+ (in rect :left) (in rect :right)) 1))
  (def center-y (brshift (+ (in rect :top) (in rect :bottom)) 1))
  (:show-tooltip ui-man :current-frame text center-x center-y))


(defn current-frame-tooltip-enable [self]
  (:disable self)
  (:set-tooltip-timeout (in self :ui-manager):current-frame (in self :timeout))
  (def hook-fn
    (:add-hook (in self :hook-manager) :frame-activated
       (fn [& args]
         (:on-frame-activated self ;args))))
  (put self :hook-fn hook-fn))


(defn current-frame-tooltip-disable [self]
  (def hook-fn (in self :hook-fn))
  (when hook-fn
    (put self :hook-fn nil)
    (:remove-hook (in self :hook-manager) :frame-activated hook-fn)))


(def current-frame-tooltip-proto
  @{:on-frame-activated current-frame-tooltip-on-frame-activated
    :enable current-frame-tooltip-enable
    :disable current-frame-tooltip-disable})


(defn current-frame-tooltip [context]
  (def {:hook-manager hook-man
        :ui-manager ui-man}
    context)

  (table/setproto
   @{:hook-manager hook-man
     :ui-manager ui-man

     # Default settings
     :timeout 1500 # In milliseconds
     :text "Current Frame"}

   current-frame-tooltip-proto))


# ================== Highlights the current frame ==================
#
# Fills an empty frame with a blank window when it's activated, so
# that it stands out from empty desktop areas nearby. To use it, add
# these in your config:
#
#     (def current-frame-area (indicator/current-frame-area jwno/context))
#     # The empty space reserved around the area, in virtual pixels (scales with DPI).
#     # Defaults to zero.
#     (put current-frame-area :margin 10)
#     (:enable current-frame-area)
#
# To stop it:
#
#     (:disable current-frame-area)
#

################## vvvv Runs in UI thread vvvv ##################

(def FRAME-AREA-WINDOW-CLASS-NAME "jwno-frame-area-window")

(defn frame-area-wndproc [hwnd msg wparam lparam]
  #(log/debug "################## frame-area-wndproc ##################")

  (case msg
    WM_CLOSE
    (do
      (DestroyWindow hwnd)
      0)

    (DefWindowProc hwnd msg wparam lparam)))


(defn create-frame-area-window [win-hbr alpha]
  (def wc
    (WNDCLASSEX
     :lpfnWndProc frame-area-wndproc
     :hInstance (GetModuleHandle nil)
     :lpszClassName FRAME-AREA-WINDOW-CLASS-NAME
     :hCursor (LoadCursor nil IDC_ARROW)
     # When using our own class, hbrBackground is mendatory, or the window will be invisible
     :hbrBackground win-hbr
     ))
  (when (null? (RegisterClassEx wc))
    (errorf "window class registration failed: 0x%x" (GetLastError)))

  (def new-hwnd
    (CreateWindowEx (bor WS_EX_LAYERED
                         WS_EX_TRANSPARENT
                         WS_EX_TOOLWINDOW
                         WS_EX_NOACTIVATE)
                    FRAME-AREA-WINDOW-CLASS-NAME
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

  (SetLayeredWindowAttributes new-hwnd 0 alpha LWA_ALPHA)
  (try
    # Raises E_INVALIDARG on Windows 10
    (DwmSetWindowAttribute new-hwnd DWMWA_WINDOW_CORNER_PREFERENCE DWMWCP_ROUND)
    ((err _fib)
     (log/debug "DwmSetWindowAttribute failed: %n" err)))

  new-hwnd)


(defn handle-show-frame-area [_hwnd _msg wparam _lparam _hook-handler state]
  (def [rect color alpha] (unmarshal-and-free wparam))
  (def [width height] (rect-size rect))
  (def frame-area-state (in state :frame-area-state @{}))

  (def area-hwnd
    (if-let [old-hwnd (in frame-area-state :hwnd)]
      old-hwnd
      # else
      (let [win-hbr (CreateSolidBrush color)
            new-hwnd (create-frame-area-window win-hbr alpha)]
        (put frame-area-state :hwnd new-hwnd)
        (put frame-area-state :win-hbr win-hbr)
        new-hwnd)))

  (put state :frame-area-state frame-area-state)

  (ShowWindow area-hwnd SW_SHOW)
  (UpdateWindow area-hwnd)
  (SetWindowPos area-hwnd
                HWND_BOTTOM
                (in rect :left)
                (in rect :top)
                width
                height
                (bor SWP_NOACTIVATE))
  0)


(defn handle-hide-frame-area [_hwnd _msg _wparam _lparam _hook-handler state]
  (def frame-area-state (in state :frame-area-state @{}))
  (when-let [area-hwnd (in frame-area-state :hwnd)]
    (ShowWindow area-hwnd SW_HIDE))
  0)


(defn handle-cleanup-frame-area [_hwnd msg _wparam _lparam _hook-handler state]
  (def frame-area-state (in state :frame-area-state @{}))
  (put state :frame-area-state nil)

  (when-let [area-hwnd (in frame-area-state :hwnd)]
    (DestroyWindow area-hwnd))

  (def unreg-ret (UnregisterClass FRAME-AREA-WINDOW-CLASS-NAME (GetModuleHandle nil)))
  (when (= FALSE unreg-ret)
    (log/debug "Failed to unregister frame area window class: 0x%x" (GetLastError)))

  (when-let [hbr (in frame-area-state :win-hbr)]
    (DeleteObject hbr))

  (when-let [custom-msgs (in state :custom-messages)
             cleanup-fn (in custom-msgs msg)]
    (put custom-msgs msg nil))
  0)

################## ^^^^ Runs in UI thread ^^^^ ##################


(defn calc-rect-with-margin [rect margins &opt scaled]
  (default scaled true)

  (def [scale-x scale-y]
    (if scaled
      (calc-pixel-scale rect)
      [1 1]))
  (def scaled-margins
    (if (and (= 1 scale-x) (= 1 scale-y))
      margins
      {:left (* scale-x (in margins :left))
       :top (* scale-y (in margins :top))
       :right (* scale-x (in margins :right))
       :bottom (* scale-y (in margins :bottom))}))
  (shrink-rect rect scaled-margins))


(defn current-frame-area-maybe-show-area [self frame]
  (def {:ui-manager ui-man
        :show-msg show-msg
        :hide-msg hide-msg}
    self)
  (def color (in self :color (GetSysColor (int/to-number COLOR_WINDOW))))
  (def alpha (in self :alpha 64))

  (def visible-children (filter |(:visible? $)
                                (in frame :children)))
  (if (empty? visible-children)
    (let [rect (:get-padded-rect frame)
          margin (in self :margin)
          margins {:left margin :top margin :right margin :bottom margin}
          rect-with-margin (calc-rect-with-margin rect margins)]
      (:post-message ui-man show-msg (alloc-and-marshal [rect-with-margin color alpha]) 0))
    (do
      (:post-message ui-man hide-msg 0 0))))


(defn current-frame-area-on-frame-activated [self frame]
  (def {:ui-manager ui-man
        :uia-manager uia-man
        :hide-msg hide-msg}
    self)
  (def lo (:get-layout frame))

  (cond
    (not= (in lo :name)
          (:get_CurrentName (in uia-man :root)))
    # The frame is on a virtual desktop that's different from our active one,
    # The area window should have been closed in the :virtual-desktop-changed
    # hook, no need to do anything here.
    # This would happen if the user switched to a new virtual desktop for the
    # first time, and don't have any window opened there.
    :nop

    (and (in frame :monitor)
         (<= (length (in lo :children)) 1))
    # Hide the area when there's only one monitor and one frame
    (:post-message ui-man hide-msg 0 0)

    true
    (:maybe-show-area self frame)))


(defn current-frame-area-on-virtual-desktop-changed [self _vd-name _layout]
  (def {:ui-manager ui-man
        :hide-msg hide-msg}
    self)
  # Only clean-up here
  (:post-message ui-man hide-msg 0 0))


(defn current-frame-area-on-window-created [self win _uia-win _exe-path desktop-info]
  (def {:ui-manager ui-man
        :window-manager window-man
        :hide-msg hide-msg}
    self)
  (def frame (get-in win [:tags :frame]))
  (when (or (nil? frame)
            (= frame
               (:get-current-frame-on-desktop (in window-man :root)
                                              desktop-info)))
    # A window is created in our current frame, hide the area
    (:post-message ui-man hide-msg 0 0)))


(defn current-frame-area-on-window-removed [self dead-win]
  (def parent-fr (in dead-win :parent))

  (cond
    (not (:attached? parent-fr))
    # The parent got removed from the tree, maybe it's auto-close-empty-frame, or
    # something else.
    :nop

    (and (in parent-fr :monitor)
         (<= (length (in (:get-layout parent-fr) :children)) 1))
    # There's only one monitor an one frame
    :nop

    (not= parent-fr (:get-current-frame (get-in self [:window-manager :root])))
    # The dead window belonged to a frame other than the current frame, none of
    # our business.
    :nop

    true
    (:maybe-show-area self parent-fr)))


(defn current-frame-area-on-frame-resized [self frame]
  (def {:window-manager window-man} self)
  (def cur-frame (:get-current-frame (in window-man :root)))
  (when (= frame cur-frame)
    (:maybe-show-area self frame)))


(defn current-frame-area-enable [self]
  (:disable self)

  (def {:hook-manager hook-man
        :ui-manager ui-man}
    self)

  (def show-msg (:add-custom-message ui-man handle-show-frame-area))
  (when (< show-msg (int/s64 0))
    (error "failed to register show-frame-area message"))
  (def hide-msg (:add-custom-message ui-man handle-hide-frame-area))
  (when (< hide-msg (int/s64 0))
    (:remove-custom-message ui-man show-msg)
    (error "failed to register hide-frame-area message"))

  (put self :show-msg show-msg)
  (put self :hide-msg hide-msg)

  (def hook-fns @{})
  (def hook-names [:frame-activated
                   :virtual-desktop-changed
                   :window-created
                   :window-removed
                   :frame-resized])
  (each hn hook-names
    (def method-name (keyword "on-" hn))
    (put hook-fns hn
       (:add-hook hook-man hn
          (fn [& args]
            (method-name self ;args)))))

  (put self :hook-fns hook-fns))


(defn current-frame-area-disable [self]
  (def {:hook-manager hook-man
        :ui-manager ui-man
        :hook-fns hook-fns
        :show-msg show-msg
        :hide-msg hide-msg}
    self)

  (when show-msg
    (:remove-custom-message ui-man show-msg)
    (put self :show-msg nil))
  (when hide-msg
    (:remove-custom-message ui-man hide-msg)
    (put self :hide-msg nil))

  (def cleanup-msg
    (:add-custom-message ui-man handle-cleanup-frame-area))
  (if (< cleanup-msg (int/s64 0))
    (log/warning "failed to clean up hwnd")
    # else
    (:send-message ui-man cleanup-msg 0 0))

  (when hook-fns
    (eachp [hn hf] hook-fns
      (:remove-hook hook-man hn hf)))
  (put self :hook-fns nil))


(def current-frame-area-proto
  @{:on-frame-activated current-frame-area-on-frame-activated
    :on-virtual-desktop-changed current-frame-area-on-virtual-desktop-changed
    :on-window-created current-frame-area-on-window-created
    :on-window-removed current-frame-area-on-window-removed
    :on-frame-resized current-frame-area-on-frame-resized

    :maybe-show-area current-frame-area-maybe-show-area
    :enable current-frame-area-enable
    :disable current-frame-area-disable})


(defn current-frame-area [context]
  (def {:hook-manager hook-man
        :ui-manager ui-man
        :uia-manager uia-man
        :window-manager window-man} context)

  (table/setproto
   @{:hook-manager hook-man
     :ui-manager ui-man
     :uia-manager uia-man
     :window-manager window-man

     # Default settings
     :margin 0
     :color (GetSysColor (int/to-number COLOR_WINDOW))
     :alpha 64
    }

   current-frame-area-proto))
