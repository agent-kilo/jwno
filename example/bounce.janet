#
# bounce.janet
#
# This is my 2025 Spring Lisp Game Jam submission. It's more like a silly "toy"
# rather than a "game" though. You'll need Jwno (https://github.com/agent-kilo/jwno)
# to run it.
#
# This script can work in two ways:
#
# 1. Standalone:
#    Just drag-n-drop this file onto jwno.exe, then press `Win + Enter` when you're
#    bored. Press `Win + P` to pause it, and `Win + Q` to quit.
#
# 2. Imported as a module:
#    If you're already using Jwno, you can save this file alongside your own config,
#    then do these:
#
#        (import bounce)
#        # To start bouncing:
#        (bounce/start jwno/context)
#        # To poke at it:
#        (bounce/poke)
#        # To pause it:
#        (bounce/toggle-pause)
#        # And, to stop it:
#        (bounce/stop)
#


(use jw32/_winuser)
(use jw32/_dwmapi)
(use jw32/_uiautomation)
(use jw32/_util)

(import jwno/util)
(import jwno/log)


(def exec-mode
  (if (dyn 'jwno/context)
    :standalone
    :imported))


(def G
  "Gravity"
  [0 1000])
(def ATTENUATION
  "How bouncy are the screen edges"
  (- 1 0.1))
(def FRICTION
  "How much of the velocity should remain after each bounce"
  (- 1 0.1))
(def INIT-VX-RANGE
  "Range of initial horizontal velocity"
  [-400 400])
(def INIT-VY-RANGE
  "Range of initial vertical velocity"
  [-600 -300])
(def INTERVAL
  "Time to wait before next update"
  0.008)

(def STOP-TIMEOUT
  "How long should we wait for threads/fibers when cleaning up"
  1) # In seconds


(def state
  @{:sup      nil
    :bouncers nil
    :paused   nil
    :context  nil})


(defn rand-range [range]
  (def [mini maxi] range)
  (+ mini (* (- maxi mini) (math/random))))


(defn check-collision [rect bound-rect vx vy]
  (def x-col
    (cond
      (and (> 0 vx)
           (<= (in rect :left) (in bound-rect :left)))
      :left

      (and (< 0 vx)
           (>= (in rect :right) (in bound-rect :right)))
      :right))
  (def y-col
    (cond
      (and (> 0 vy)
           (<= (in rect :top) (in bound-rect :top)))
      :top

      (and (< 0 vy)
           (>= (in rect :bottom) (in bound-rect :bottom)))
      :bottom))
  [x-col y-col])


(defn calc-movement [rect dx dy]
  [(math/round (+ (in rect :left) dx))
   (math/round (+ (in rect :top)  dy))])


(defn check-started []
  (has-key? state :sup))


(defn try-to-give [hwnd chan msg]
  (try
    (ev/give chan msg)
    ((err)
     (log/debug "bounce: failed to write to channel for hwnd %n: %n"
                hwnd err))))


(defn broadcast-impl [bouncers msg]
  (eachp [hwnd chan] bouncers
    (try-to-give hwnd chan msg)))


(defn bouncer [args]
  (def [obj chan] args)
  (def [hwnd v bound] obj)
  (var [vx vy] v)
  (var t (os/clock :monotonic))
  (var paused false)

  (math/seedrandom (util/pointer-to-number hwnd))

  (defer
    (ev/chan-close chan)

    # body
    (while (and (not= FALSE (IsWindow hwnd))
                (not= FALSE (IsWindowVisible hwnd)))
      (def msg
        (try
          (ev/with-deadline INTERVAL (ev/take chan))
          ((err fib)
           (if (= err "deadline expired")
             nil
             (propagate err fib)))))
      (match msg
        nil
        :nop

        :stop
        # Out of while loop
        (break)

        [:set-v [nvx nvy]]
        (do
          (set vx nvx)
          (set vy nvy))

        :rand-v
        (do
          (set vx (rand-range INIT-VX-RANGE))
          (set vy (rand-range INIT-VY-RANGE)))

        :pause
        (do
          (set paused true)
          (log/debug "bounce: thread for hwnd %n paused" hwnd))

        :unpause
        (do
          (set paused false)
          (set t (os/clock :monotonic))
          (log/debug "bounce: thread for hwnd %n unpaused" hwnd))

        _
        (log/debug "bounce: unknown message: %n" msg))

      (unless paused
        (def [gwr-ret rect] (GetWindowRect hwnd))
        (when (= FALSE gwr-ret)
          (errorf "failed to get bounding rectangle for window %n" hwnd))
        (def [x-col y-col] (check-collision rect bound vx vy))
        (when x-col
          (set vx (- (* vx ATTENUATION)))
          (set vy (* vy FRICTION)))
        (when y-col
          (set vy (- (* vy ATTENUATION)))
          (set vx (* vx FRICTION)))

        (def now (os/clock :monotonic))
        (def dt (- now t))
        (set t now)

        (+= vx (* dt (in G 0)))
        (+= vy (* dt (in G 1)))

        (def new-coords (calc-movement rect (* vx dt) (* vy dt)))
        (SetWindowPos hwnd
                      nil
                      ;new-coords
                      0 0
                      (bor SWP_NOZORDER
                           SWP_NOACTIVATE
                           SWP_NOSIZE
                           SWP_ASYNCWINDOWPOS
                           SWP_NOREDRAW))))))


(defn spawn-bouncer [obj sup &opt paused]
  (default paused false)

  (def chan (ev/thread-chan))
  (ev/thread bouncer [obj chan] :nt sup)
  (when paused
    (ev/give chan :pause))
  chan)


(defn wait-for-bouncers [bouncers sup timeout]
  (var stop? false)
  (while (and (not stop?)
              (not (empty? bouncers)))
    (try
      (ev/with-deadline timeout
        (def [sig payload args] (ev/take sup))
        (when args
          (def [obj chan] args)
          (def [hwnd _ _] obj)
          (put bouncers hwnd nil)
          (unless (= :ok sig)
            (log/debug "bounce: bouncer for hwnd %n exited abnormally, sig = %n, payload = %n"
                       hwnd sig payload))))
      ((err fib)
       (if (= err "deadline expired")
         (set stop? true)
         (propagate err fib))))))


(defn hwnd-to-bouncy-obj [hwnd v]
  (def hmon (MonitorFromWindow hwnd MONITOR_DEFAULTTONULL))
  (when (= NULL hmon)
    (break nil))

  (def mi (MONITORINFOEX))
  (when (= FALSE (GetMonitorInfo hmon mi))
    (break nil))

  [hwnd v (in mi :rcWork)])


(defn check-for-new-windows []
  (unless (check-started)
    (break))

  (def {:sup sup
        :bouncers bouncers
        :paused  paused
        :context context}
    state)
  (def {:window-manager window-man} context)

  (def obj-list @[])

  (EnumChildWindows
   nil
   (fn [hwnd]
     (unless (has-key? bouncers hwnd)
       (def filter-result
         (try
           (:filter-hwnd window-man hwnd)
           ((err)
            [false [:error err]])))
       (match filter-result
         [false reason]
         (log/debug "not handling hwnd %n: %n" hwnd reason)

         true
         (when-let [rand-v [(rand-range INIT-VX-RANGE) (rand-range INIT-VY-RANGE)]
                    obj    (hwnd-to-bouncy-obj hwnd rand-v)]
           (array/push obj-list obj))))
     1 # !!! IMPORTANT
     ))

  (each obj obj-list
    (put bouncers (first obj) (spawn-bouncer obj sup paused))))


(defn on-vd-changed [_name _lo]
  (unless (check-started)
    (break))
  (ev/spawn
   (check-for-new-windows)))


(defn on-filter-window [hwnd &]
  (def ret (not= exec-mode :standalone))

  (unless (check-started)
    # Early return
    (break ret))

  (def {:sup sup
        :bouncers bouncers
        :paused paused}
    state)

  (when (has-key? bouncers hwnd)
    # Early return
    (break ret))

  (when-let [rand-v [(rand-range INIT-VX-RANGE) (rand-range INIT-VY-RANGE)]
             obj    (hwnd-to-bouncy-obj hwnd rand-v)]
    (put bouncers hwnd (spawn-bouncer obj sup paused)))

  ret)


# Function forward declaration
(var on-shutting-down nil)


(defn start [context]
  (when (check-started)
    (break state))

  (def {:hook-manager hook-man} context)

  (put state :sup (ev/thread-chan 1024))
  (put state :bouncers @{})
  (put state :paused false)
  (put state :context context)

  (check-for-new-windows)

  (:remove-hook hook-man :virtual-desktop-changed on-vd-changed)
  (:add-hook hook-man :virtual-desktop-changed on-vd-changed)

  (:remove-hook hook-man :filter-window on-filter-window)
  (:add-hook hook-man :filter-window on-filter-window)

  (:remove-hook hook-man :shutting-down on-shutting-down)
  (:add-hook hook-man :shutting-down on-shutting-down)

  state)


(defn stop []
  (unless (check-started)
    (break))

  (def {:hook-manager hook-man} (in state :context))

  (:remove-hook hook-man :virtual-desktop-changed on-vd-changed)
  (:remove-hook hook-man :filter-window on-filter-window)
  (:remove-hook hook-man :shutting-down on-shutting-down)

  (def sup (in state :sup))
  (def bouncers (in state :bouncers))

  (table/clear state)

  (broadcast-impl bouncers :stop)
  (wait-for-bouncers bouncers sup STOP-TIMEOUT)

  bouncers)


(varfn on-shutting-down []
  (stop))


(defn broadcast [msg]
  (unless (check-started)
    (break))
  (broadcast-impl (in state :bouncers) msg))


(defn toggle-pause []
  (unless (check-started)
    (break))
  (put state :paused (not (in state :paused)))
  (if (in state :paused)
    (broadcast-impl (in state :bouncers) :pause)
    # else
    (broadcast-impl (in state :bouncers) :unpause)))


(defn poke []
  (broadcast :rand-v))


(compwhen (= exec-mode :standalone)
  (def {:command-manager command-man
        :key-manager     key-man
        :hook-manager    hook-man}
    jwno/context)

  (:add-command command-man :bounce-poke poke)
  (:add-command command-man :bounce-toggle-pause toggle-pause)

  (def keymap (:new-keymap key-man))

  (:define-key keymap "Win + Enter" :bounce-poke)
  (:define-key keymap "Win + P"     :bounce-toggle-pause)
  (:define-key keymap "Win + Q"     :quit)

  (:set-keymap key-man keymap)

  (start jwno/context))
