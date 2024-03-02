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

  (var ui-thread nil)

  (forever
    (def event (ev/select uia-chan ui-chan))
    #(log/debug "event = %p" event)

    (match event
      [:take chan msg]
      (match msg
        [:ui/initialized thread-id]
        (set ui-thread thread-id)

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
        (log/warning "Unknown message: %n" msg))
      _
      (log/warning "Unhandled ev/select event: %n" event)))

  (uia/uia-deinit uia uia-deinit-fns)

  (log/deinit))
