(use jw32/_combaseapi)
(use jw32/_shobjidl_core)

(import ./log)


(defn vdm-call-method [self method args &opt chan]
  (default chan (ev/chan))
  (ev/give (in self :in) [:call-method method args chan])
  (ev/take chan))


(defn vdm-destroy [self]
  (ev/chan-close (in self :in))
  (def sup-msg (ev/take (in self :sup)))
  (log/debug "vdm-worker supervisor message: %n" sup-msg))


(def virtual-desktop-manager-proto
  @{:call-method vdm-call-method
    :destroy     vdm-destroy})


(defn vdm-worker-try-method [method com args &opt retry-count max-retries init-wait backoff]
  (default retry-count 0)
  (default max-retries 4)
  (default init-wait   0.1) # in seconds
  (default backoff     2)

  (def ret (protect (method com ;args)))
  (match ret
    [true result]
    ret

    [false err]
    # All errors are assumed to be recoverable, and we attempt retries on them.
    (if (>= retry-count max-retries)
      (do
        (log/debug "Virtual desktop method call failed after %n retries" retry-count)
        ret)
      (do
        (def wait-time (* init-wait (math/pow backoff retry-count)))
        (log/debug "Virtual desktop method call failed (%n), retrying in %n seconds"
                   err wait-time)
        (ev/sleep wait-time)
        (vdm-worker-try-method method com args (+ 1 retry-count) max-retries init-wait backoff)))))


(defn vdm-worker [[com in-chan]]
  (log/debug "vdm-worker spawned")

  (var msg nil)
  (while (set msg (ev/take in-chan))
    (match msg
      [:call-method method args out-chan]
      (ev/give out-chan (vdm-worker-try-method method com args))

      _
      (log/warning "Unknown message for vdm-worker: %n" msg)))

  (:Release com)

  (log/debug "vdm-worker stopping"))


(defn virtual-desktop-manager []
  (def com
    (CoCreateInstance CLSID_VirtualDesktopManager
                      nil
                      CLSCTX_INPROC_SERVER
                      IVirtualDesktopManager))
  (def worker-in-chan  (ev/chan))
  (def worker-sup      (ev/chan))
  (def worker (ev/go vdm-worker [com worker-in-chan] worker-sup))
  (table/setproto
   @{:worker worker
     :sup    worker-sup
     :in     worker-in-chan}
   virtual-desktop-manager-proto))
