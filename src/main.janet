(use jw32/winuser)
(use jw32/libloaderapi)
(use jw32/consoleapi)
(use jw32/combaseapi)
(use jw32/uiautomation)

(use ./util)

(import ./ui)
(import ./uia)
(import ./log)


(def DEFAULT-CHAN-LIMIT 65536)


(defn main [& args]
  (log/init :debug)
  (log/debug "in main")
  #(FreeConsole)

  (def hInstance (GetModuleHandle nil))

  (def uia-chan (ev/thread-chan DEFAULT-CHAN-LIMIT))
  (def [uia uia-deinit-fns]
    (try
      (uia/uia-init uia-chan)
      ((err fib)
       (show-error-and-exit err 1))))

  (def ui-chan (ev/thread-chan DEFAULT-CHAN-LIMIT))
  (ev/spawn-thread
   (ui/ui-thread hInstance (args 0) ui-chan))

  (when (not= (ev/take ui-chan) :ok)
    (show-error-and-exit "UI thread initialization failed" 1))
  (ev/give ui-chan :ok)

  (forever
    (def event (ev/select uia-chan ui-chan))
    (log/debug "event = %p" event)

    (match event
      [:take chan msg]
      (match msg
        :ui/exit
        (break)

        [:uia/window-opened win]
        (do
          (when (and (= (win :name) "File Explorer")
                     (= (win :class-name) "CabinetWClass"))
            (def uia-win (:ElementFromHandle uia (win :native-window-handle)))
            (def pat (:GetCurrentPatternAs uia-win UIA_TransformPatternId IUIAutomationTransformPattern))
            (when pat
              (:Move pat 0 0)
              (:Resize pat 900 900))))
        
        _
        (log/warning "Unknown message: %p" msg))
      _
      (log/warning "Unhandled ev/select event: %p" event)))

  (uia/uia-deinit uia uia-deinit-fns)

  (log/deinit))
