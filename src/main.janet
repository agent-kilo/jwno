(use jw32/winuser)
(use jw32/libloaderapi)
(use jw32/combaseapi)
(use jw32/uiautomation)

(use ./key)
(use ./util)

(import ./ui)
(import ./uia)
(import ./log)


(def DEFAULT-CHAN-LIMIT 65536)


(defn main-loop [context]
  (forever
   (def event (ev/select ;(in context :event-sources)))

   (match event
     [:take chan msg]
     (match msg
       [:ui/initialized thread-id msg-hwnd]
       (do
         (put context :ui-thread thread-id)
         (put context :msg-hwnd msg-hwnd))

       :ui/exit
       (break)

       [:uia/window-opened win]
       (do
         (when (and (= (win :name) "File Explorer")
                    (= (win :class-name) "CabinetWClass"))
           (def uia-win (:ElementFromHandle (in context :uia) (win :native-window-handle)))
           (def pat (:GetCurrentPatternAs uia-win UIA_TransformPatternId IUIAutomationTransformPattern))
           (when pat
             (:Move pat 0 0)
             (:Resize pat 900 900))))

       [:key/key-event state key cmd]
       (process-key-event state key cmd context)

       _
       (log/warning "Unknown message: %n" msg))
     _
     (log/warning "Unhandled ev/select event: %n" event))))


(defn main [& args]
  (log/init :debug)
  (log/debug "in main")

  (def hInstance (GetModuleHandle nil))

  (def uia-chan (ev/thread-chan DEFAULT-CHAN-LIMIT))
  (def [uia uia-deinit-fns]
    (try
      (uia/uia-init uia-chan)
      ((err fib)
       (show-error-and-exit err 1))))

  (def ui-chan (ev/thread-chan DEFAULT-CHAN-LIMIT))

  (def keymap (define-keymap))

  (define-key keymap
    [(key (ascii "Q") @[:lwin])]
    :quit)

  (define-key keymap
    [(key (ascii "V") @[:rwin])
     (key VK_LWIN)]
    [:send-keys
     VK_LWIN])

  (define-key keymap
    [(key (ascii "V") @[:rwin])
     (key (ascii "R") @[:rwin])]
    [:send-keys
     [VK_LWIN :down]
     (ascii "R")
     [VK_LWIN :up]])

  (define-key keymap
    [(key VK_LWIN @[:lctrl])]
    [:send-keys
     [VK_LCONTROL :up]
     VK_LWIN])

  (define-key keymap
    [(key (ascii "T") @[:lctrl :lalt])
     (key VK_LCONTROL)]
    [:send-keys
     (ascii "A")
     (ascii "B")
     (ascii "C")
     (ascii "D")])

  (define-key keymap
    [(key VK_RMENU @[:lctrl])]
    [:send-keys
     [VK_LCONTROL :up]
     #[VK_RMENU :up]
     VK_LWIN])

  (define-key keymap
    [(key (ascii "T") @[:lwin])
     (key (ascii "N") @[:lwin])]
    "LWin+t LWin+n")

  #(define-key keymap
  #  # This would block all keys using the :lwin modifier. Is this acceptable?
  #  [(key VK_LWIN)]
  #  [:send-keys VK_LWIN])

  (log/debug "keymap = %n" keymap)

  (ev/spawn-thread
   (ui/ui-thread hInstance (args 0) keymap ui-chan))

  (def context
    @{:hInstance hInstance
      :uia uia
      :event-sources [uia-chan ui-chan]
      :ui-thread nil
      :msg-hwnd nil})

  (main-loop context)

  (uia/uia-deinit uia uia-deinit-fns)

  (log/deinit))
