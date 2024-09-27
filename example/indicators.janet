(use jw32/_winuser)
(use jw32/_libloaderapi)
(use jw32/_errhandlingapi)
(use jw32/_dwmapi)
(use jw32/_util)

(import jwno/log)

(use jwno/util)


(def FRAME-AREA-WINDOW-CLASS-NAME "jwno-frame-area-window")


################## vvvv Runs in UI thread vvvv ##################

(defn frame-area-wndproc [hwnd msg wparam lparam]
  #(log/debug "################## frame-area-wndproc ##################")

  (case msg
    WM_CLOSE
    (do
      (DestroyWindow hwnd)
      0)

    (DefWindowProc hwnd msg wparam lparam)))


(defn create-frame-area-window []
  (def wc
    (WNDCLASSEX
     :lpfnWndProc frame-area-wndproc
     :hInstance (GetModuleHandle nil)
     :lpszClassName FRAME-AREA-WINDOW-CLASS-NAME
     :hCursor (LoadCursor nil IDC_ARROW)
     # When using our own class, hbrBackground is mendatory, or the window will be invisible
     :hbrBackground (+ 1 COLOR_WINDOW)
     ))
  (when (null? (RegisterClassEx wc))
    (errorf "window class registration failed: 0x%x" (GetLastError)))

  (def new-hwnd
    (CreateWindowEx (bor WS_EX_LAYERED WS_EX_TOOLWINDOW WS_EX_NOACTIVATE)
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

  (SetLayeredWindowAttributes new-hwnd 0 64 LWA_ALPHA)
  (try
    # Raises E_INVALIDARG on Windows 10
    (DwmSetWindowAttribute new-hwnd DWMWA_WINDOW_CORNER_PREFERENCE DWMWCP_ROUND)
    ((err _fib)
     (log/debug "DwmSetWindowAttribute failed: %n" err)))

  new-hwnd)


(defn handle-show-frame-area [hwnd _msg wparam _lparam _hook-handler state]
  (def rect (unmarshal-and-free wparam))
  (def [width height] (rect-size rect))

  (def area-hwnd
    (if-let [old-hwnd (in state :frame-area-hwnd)]
      old-hwnd
      # else
      (do
        (def new-hwnd (create-frame-area-window))
        (put state :frame-area-hwnd new-hwnd)
        new-hwnd)))

  (log/debug "--------- AREA-HWND = %n" area-hwnd)
  (ShowWindow area-hwnd SW_SHOW)
  (UpdateWindow hwnd)
  (SetWindowPos area-hwnd
                HWND_BOTTOM
                (in rect :left)
                (in rect :top)
                width
                height
                (bor SWP_NOACTIVATE))
  0)


(defn handle-hide-frame-area [_hwnd _msg _wparam _lparam _hook-handler state]
  (when-let [area-hwnd (in state :frame-area-hwnd)]
    (log/debug "--------- HIDING AREA-HWND = %n" area-hwnd)
    (ShowWindow area-hwnd SW_HIDE))
  0)


(defn handle-cleanup-frame-area [_hwnd msg _wparam _lparam _hook-handler state]
  (when-let [area-hwnd (in state :frame-area-hwnd)]
    (put state :frame-area-hwnd nil)
    (DestroyWindow area-hwnd))

  (def unreg-ret (UnregisterClass FRAME-AREA-WINDOW-CLASS-NAME (GetModuleHandle nil)))
  (when (= FALSE unreg-ret)
    (log/debug "Failed to unregister window class: 0x%x" (GetLastError)))

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

  (def visible-children (filter |(and (:alive? $)
                                      (= FALSE (IsIconic (in $ :hwnd))))
                                (in frame :children)))
  (if (empty? visible-children)
    (let [rect (:get-padded-rect frame)
          margin (in self :margin)
          margins {:left margin :top margin :right margin :bottom margin}
          rect-with-margin (calc-rect-with-margin rect margins)]
      (:post-message ui-man show-msg (alloc-and-marshal rect-with-margin) 0))
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
     :margin 0}
   current-frame-area-proto))
