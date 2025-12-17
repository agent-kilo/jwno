(use jw32/_combaseapi)
(use jw32/_shobjidl_core)
(use jw32/_winnt)
(use jw32/_winreg)

(import ./log)


(defn vdm-call-method [self method args &opt chan]
  (default chan (ev/chan))
  (ev/give (in self :in) [:call-method method args chan])
  (ev/take chan))


(defn vdm-destroy [self]
  (ev/chan-close (in self :in))
  (def sup-msg (ev/take (in self :sup)))
  (log/debug "vdm-worker supervisor message: %n" sup-msg))


(defn- format-guid [guid-buf]
  (string/format "{%02X%02X%02X%02X-%02X%02X-%02X%02X-%02X%02X-%02X%02X%02X%02X%02X%02X}"
                 (in guid-buf 3)
                 (in guid-buf 2)
                 (in guid-buf 1)
                 (in guid-buf 0)

                 (in guid-buf 5)
                 (in guid-buf 4)

                 (in guid-buf 7)
                 (in guid-buf 6)

                 (in guid-buf 8)
                 (in guid-buf 9)

                 (in guid-buf 10)
                 (in guid-buf 11)
                 (in guid-buf 12)
                 (in guid-buf 13)
                 (in guid-buf 14)
                 (in guid-buf 15)))


#
# Mostly adapted from PowerToys' VirtualDesktopHelper.cs
#
(defn vdm-get-all-desktops [self &opt refresh]
  (default refresh false)

  (var cache (in self :vd-cache))
  (when (and (not refresh)
             (not (nil? cache)))
    (log/debug "Reusing vd-cache")
    # Early return
    (break cache))

  (log/debug "Refreshing vd-cache")

  (def [stat hk-cu] (RegOpenCurrentUser KEY_READ))
  (unless (= 0 stat)
    (errorf "RegOpenCurrentUser failed: %n" stat))

  (with [_ hk-cu RegCloseKey]
    (def reg-vd-path ``SOFTWARE\Microsoft\Windows\CurrentVersion\Explorer\VirtualDesktops``)
    (def reg-vd-value-name "VirtualDesktopIDs")
    (def [stat hk-vd] (RegOpenKeyEx hk-cu reg-vd-path 0 KEY_READ))
    (unless (= 0 stat)
      (errorf "RegOpenKeyEx failed for %n: %n" reg-vd-path stat))

    (with [_ hk-vd RegCloseKey]
      (def [stat _type id-data]
        (RegGetValue hk-vd nil reg-vd-value-name RRF_RT_REG_BINARY))
      (log/debug "VD IDs read from registry (%n bytes): %n"
                 (if (nil? id-data) 0 (length id-data))
                 id-data)
      (unless (or (= 0 stat)  # ERROR_SUCCESS
                  (= 2 stat)) # ERROR_FILE_NOT_FOUND, means the value storing VD IDs does not exist.
                              # A non-existent value in turn means the user never created another VD,
                              # we should consider the VD feature "disabled", instead of panicking.
        (errorf "RegGetValue failed for %n: %n" reg-vd-value-name stat))

      (set cache @[])

      (def guid-size 16)
      (def vd-count
        (if-not (= 0 stat)
          0  # We didn't get anything from the registry
          (/ (length id-data) guid-size)))
      (for i 0 vd-count
        (def s (* i guid-size))
        (def e (+ s guid-size))
        (def guid (buffer/slice id-data s e))
        (def guid-str (format-guid guid))
        (def vd-subkey (string/format ``Desktops\%s`` guid-str))
        (def [stat _type name-data]
          (RegGetValue hk-vd vd-subkey "Name" RRF_RT_REG_SZ))
        (log/debug "RegGetValue stat = %n" stat)
        (def name
          (case stat
            # Success
            0  (slice name-data 0 -2)  # Exclude trailing zero
            # 2 = ERROR_FILE_NOT_FOUND, the "Name" value is absent
            2  nil
            # Default branch
            (errorf "RegGetValue failed for name value in %n: %n"
                    vd-subkey stat)))
        (log/debug "Found desktop: %n, %n" guid-str name)
        (array/push cache [(+ 1 i) guid-str name]))

      (put self :vd-cache cache)
      cache)))


(defn vdm-get-desktop-name [self guid &opt refresh]
  (default refresh false)

  (def dlist (:get-all-desktops self refresh))

  (if (empty? dlist)
    # Virtual desktops are disabled
    (do
      (log/debug "No active virtual desktop, using placeholder for desktop name")
      :default)

    # else, VDs are enabled
    (do
      (def filter-fn (fn [[_i g _n]] (= g guid)))
      (when-let [dinfo (find filter-fn dlist)
                 [idx _guid name] dinfo]
        (if name
          name
          # else, the desktop has no user-defined name
          [:default idx]))
      # nil for all unknown desktops
      )))


(def default-desktop-name-peg
  (peg/compile ~(thru (sequence :s (replace (capture :d+) ,scan-number) -1))))

# XXX: This method assumes that, the default desktop names always end
# with a whitespace character and a number (see the peg pattern above)
(defn vdm-get-desktop-guid-from-name [self name &opt refresh]
  (default refresh false)

  (def dlist (:get-all-desktops self refresh))

  (when (empty? dlist)
    # Early return
    (log/debug "No active virtual desktop, using placeholder for desktop GUID")
    (break :default))

  (def filter-fn
    (fn [[i _g n]]
      (if (nil? n)
        (= name [:default i])
        (= name n))))
  (def found (filter filter-fn dlist))
  (when (< 1 (length found))
    (log/warning "There are more than one virtual desktop with the name %n, falling back to the first one"))

  (if-let [dinfo (first found)
           [_idx guid _found-name] dinfo]
    guid

    # else
    (match name
      [:default idx]
      (do
        (log/warning "There is no virtual desktop with the index %n (%n)"
                     idx name)
        nil)

      # name should be a string otherwise, and it should contain the
      # desktop index at the end
      (if-let [matched (peg/match default-desktop-name-peg name)
               [matched-idx] matched]
        (do
          (def filter-fn (fn [[i _g _n]] (= i matched-idx)))
          (def indexed (filter filter-fn dlist))
          (if-let [dinfo (first indexed)
                   [_idx guid found-name] dinfo]
            (cond
              (nil? found-name)   guid
              (= name found-name) guid
              true (do
                     (log/warning "Desktop has a different user-defined name (%n / %n)"
                                  name found-name)
                     nil))
            # else
            (do
              (log/warning "There is no virtual desktop with the index %n (%n)"
                           matched-idx name)
              nil)))
        # else, the system-provided name doesn't match the peg pattern,
        # something's seriously wrong
        (do
          (log/warning "Cannot extract index from default desktop name: %n" name)
          nil)))))


(def virtual-desktop-manager-proto
  @{:call-method vdm-call-method
    :destroy     vdm-destroy
    :get-all-desktops vdm-get-all-desktops
    :get-desktop-name vdm-get-desktop-name
    :get-desktop-guid-from-name vdm-get-desktop-guid-from-name})


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
    (case [method err]
      [:GetWindowDesktopId -2147319765] # 0x8002802B, TYPE_E_ELEMENTNOTFOUND
      # This special combo means the window is not managed by virtual desktops (e.g. the start menu window)
      # no retry needed in this case
      (do
        (log/debug "GetWindowDesktopId returned %n for window %n" err args)
        ret)

      # All other errors are assumed to be recoverable, and we attempt retries on them.
      (if (>= retry-count max-retries)
        (do
          (log/debug "Virtual desktop method %n failed after %n retries (%n)" method retry-count err)
          ret)
        (do
          (def wait-time (* init-wait (math/pow backoff retry-count)))
          (log/debug "Virtual desktop method %n failed (%n), retrying in %n seconds"
                     method err wait-time)
          (ev/sleep wait-time)
          (vdm-worker-try-method method com args (+ 1 retry-count) max-retries init-wait backoff))))))


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


#
# The VD manager is mostly used to query VD info. It handles some... special cases.
#
# Special Case #1:
#   A user may have NEVER created another virtual desktop. In this case the
#   `VirtualDesktopIDs` value would not exist in the registry, and it would be
#   impossible (and meaningless) for us to match the actual GUID and desktop
#   name, so the VD manager will behave as if virtual desktops are all "disabled":
#     * :get-all-desktops always returns an empty list;
#     * :get-desktop-name always returns :default, instead of the actual name;
#     * :get-desktop-guid-from-name always returns :default, instead of the actual GUID.
#
# Special Case #2:
#   Windows does not write the default desktop names to the registry, if the user
#   have NEVER changed them. Yet the default desktop names are LOCALIZED strings,
#   e.g. they look like "Desktop 1", "Desktop 2" in an English locale, but turn
#   into "デスクトプ 1", "デスクトプ 2" in a Japanese locale. To avoid hard-coding
#   these localized strings in the code, the VD manager handles names like this,
#   for desktops with default names:
#     * The corresponding name components returned by :get-all-desktops will be nil;
#     * :get-desktop-name will return tuples like [:default 1],[:default 2], etc.,
#       instead of string names.
#     * :get-desktop-guid-from-name can accept these tuple names.
#
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
     :in     worker-in-chan
     :vd-cache nil  # Cached VD GUIDs and names, initialized in vdm-get-all-desktops
     }
   virtual-desktop-manager-proto))
