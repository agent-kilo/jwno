(use jw32/_winuser)
(use jw32/_winbase)
(use jw32/_processthreadsapi)
(use jw32/_handleapi)
(use jw32/_util)

(import ./const)


################## Misc. Tools ##################

(defn show-error-and-exit [msg exit-code &opt stack-trace]
  (MessageBox nil
              (if stack-trace
                (string/format "Error: %s\n%s" msg stack-trace)
                (string/format "Error: %s" msg))
              "Error"
              (bor MB_ICONEXCLAMATION MB_OK))
  (os/exit exit-code))


(defn get-exe-dir []
  (def argv0 (in (dyn :args) 0))
  (def rev-argv0 (string/reverse argv0))
  (def back-slash-found (string/find "\\" rev-argv0))
  (def slash-found (string/find "/" rev-argv0))

  (def last-seg-len
    (cond
      (nil? back-slash-found)
      (if slash-found
        slash-found
        nil)

      (nil? slash-found)
      back-slash-found

      true
      (min back-slash-found slash-found)))

  (if last-seg-len
    (string/slice argv0 0 (- (length argv0) last-seg-len))
    # XXX: No path separator found, assume it's started
    # from current working directory.
    (string (os/cwd) "\\")))


(defn get-stack-trace [fib]
  (def err-buf @"")
  (with-dyns [:err err-buf
              :err-color false]
    (debug/stacktrace fib))
  err-buf)


(defn global-ns [sym]
  (symbol (string const/GLOBAL-NAMESPACE-PREFIX sym)))


(defn get-pid-path [pid]
  (with [proc
         (OpenProcess (bor PROCESS_QUERY_INFORMATION
                           PROCESS_VM_READ)
                      true
                      pid)
         CloseHandle]
    (QueryFullProcessImageName proc 0)))


(defn get-hwnd-path [hwnd]
  (def [_tid pid] (GetWindowThreadProcessId hwnd))
  (when (= (int/u64 0) pid)
    (break nil))

  (def path (get-pid-path pid))

  (var uwp-pid nil)
  (when (and (not (nil? path))
             (string/has-suffix? "ApplicationFrameHost.exe" path))
    # Executables for UWP apps live somewhere else
    (EnumChildWindows hwnd
                      (fn [child-hwnd]
                        (def [_tid child-pid] (GetWindowThreadProcessId child-hwnd))
                        (when (and (not= (int/u64 0) child-pid)
                                   (not= pid child-pid))
                          (set uwp-pid child-pid)
                          (break FALSE))
                        TRUE)))
  (if uwp-pid
    (get-pid-path uwp-pid)
    path))


(defn merge-settings [obj settings &opt mergers]
  (def default-merger (fn [orig-v new-v] new-v))

  (def orig-settings @[])

  (def kv-pairs
    (cond
      (indexed? settings)
      settings

      (or (table? settings)
          (struct? settings))
      (pairs settings)))

  (each [k v] kv-pairs
    (def orig-v (in obj k))

    (cond
      (nil? mergers)
      (do
        (array/push orig-settings [k orig-v])
        (put obj k (default-merger orig-v v)))

      (has-key? mergers k)
      (do
        (def m (in mergers k))
        (cond
          (or (function? m)
              (cfunction? m))
          (do
            (array/push orig-settings [k orig-v])
            (put obj k (m orig-v v)))

          (truthy? m)
          (do
            (array/push orig-settings [k orig-v])
            (put obj k (default-merger orig-v v)))))))

  orig-settings)


################## Pointers ##################

(def pointer-peg
  (peg/compile
   ~{:left-delim "<"
     :right-delim ">"
     :hex-digit (choice (range "09") (range "af") (range "AF"))
     :hex-addr (capture (sequence "0x" (some :hex-digit)))
     :main (sequence :left-delim :s* "pointer" :s+ :hex-addr :s* :right-delim)}))


(defn pointer-to-number [pointer]
  (def pointer-str (string pointer))
  (def matched (peg/match pointer-peg pointer-str))
  (parse ;matched))


################## Calculations ##################

(defmacro rect-center [rect]
  ~[(math/round (/ (+ (in ,rect :left) (in ,rect :right)) 2))
    (math/round (/ (+ (in ,rect :top) (in ,rect :bottom)) 2))])

(defmacro rect-width [rect]
  ~(- (in ,rect :right) (in ,rect :left)))

(defmacro rect-height [rect]
  ~(- (in ,rect :bottom) (in ,rect :top)))

(defmacro rect-size [rect]
  ~[(- (in ,rect :right) (in ,rect :left))
    (- (in ,rect :bottom) (in ,rect :top))])

(defmacro rect-same-size? [r1 r2]
  ~(= (rect-size ,r1) (rect-size ,r2)))

(defn shrink-rect [rect amounts]
  (def top (math/ceil (+ (in rect :top)
                         (in amounts :top 0))))
  (def bottom (math/floor (- (in rect :bottom)
                             (in amounts :bottom 0))))
  (def left (math/ceil (+ (in rect :left)
                          (in amounts :left 0))))
  (def right (math/floor (- (in rect :right)
                            (in amounts :right 0))))
  {:top top
   :left left
   :bottom (if (< bottom top) top bottom)
   :right (if (< right left) left right)})

(defn expand-rect [rect amounts]
  (def top (math/floor (- (in rect :top)
                          (in amounts :top 0))))
  (def bottom (math/ceil (+ (in rect :bottom)
                            (in amounts :bottom 0))))
  (def left (math/floor (- (in rect :left)
                           (in amounts :left 0))))
  (def right (math/ceil (+ (in rect :right)
                           (in amounts :right 0))))
  {:top top
   :left left
   :bottom (if (< bottom top) top bottom)
   :right (if (< right left) left right)})

(defn union-rect [& rect-list]
  (var ret-left   math/int-max)
  (var ret-top    math/int-max)
  (var ret-right  math/int-min)
  (var ret-bottom math/int-min)

  (defn check-rect [r]
    (def {:left   left
          :top    top
          :right  right
          :bottom bottom}
      r)
    (when (< left ret-left)
      (set ret-left left))
    (when (< top ret-top)
      (set ret-top top))
    (when (> right ret-right)
      (set ret-right right))
    (when (> bottom ret-bottom)
      (set ret-bottom bottom)))

  (each r rect-list
    (check-rect r))

  (when (and (<= ret-left ret-right)
             (<= ret-top ret-bottom))
    {:left ret-left
     :top  ret-top
     :right  ret-right
     :bottom ret-bottom}))

(defn intersect-rect [& rect-list]
  (var ret-left   math/int-min)
  (var ret-top    math/int-min)
  (var ret-right  math/int-max)
  (var ret-bottom math/int-max)

  (defn check-rect [r]
    (def {:left   left
          :top    top
          :right  right
          :bottom bottom}
      r)
    (when (> left ret-left)
      (set ret-left left))
    (when (> top ret-top)
      (set ret-top top))
    (when (< right ret-right)
      (set ret-right right))
    (when (< bottom ret-bottom)
      (set ret-bottom bottom)))

  (each r rect-list
    (check-rect r))

  (when (and (not= ret-left math/int-min)
             (not= ret-right math/int-max)
             (<= ret-left ret-right)
             (not= ret-top math/int-min)
             (not= ret-bottom math/int-max)
             (<= ret-top ret-bottom))
    {:left ret-left
     :top  ret-top
     :right  ret-right
     :bottom ret-bottom}))

(defn combine-rect-border-space [& args]
  (def last-arg (last args))
  (def filter-fn
    (if (function? last-arg)
      last-arg
      identity))
  (def operands
    (if (function? last-arg)
      (slice args 0 -2)
      args))

  (def top (sum (map |(in $ :top) operands)))
  (def left (sum (map |(in $ :left) operands)))
  (def bottom (sum (map |(in $ :bottom) operands)))
  (def right (sum (map |(in $ :right) operands)))

  {:top (filter-fn top)
   :left (filter-fn left)
   :bottom (filter-fn bottom)
   :right (filter-fn right)})


(defn calc-pixel-scale [rect]
  (def hmon (MonitorFromRect rect MONITOR_DEFAULTTONULL))
  (def [dpi-x dpi-y]
    (if (null? hmon)
      # XXX: Some windows spawn themselves outside of any monitor, use
      # the default DPI in this case.
      [(int/u64 const/USER-DEFAULT-SCREEN-DPI) (int/u64 const/USER-DEFAULT-SCREEN-DPI)]
      # GetDpiForWindow will always return 96 for windows that are not
      # DPI-aware, which is incorrect for DWM border size calculation.
      # Have to use GetDpiForMonitor here instead.
      (GetDpiForMonitor hmon MDT_DEFAULT)))
  # int/u64 doesn't support floating point arithmetic, thus int/to-number
  [(/ (int/to-number dpi-x) const/USER-DEFAULT-SCREEN-DPI)
   (/ (int/to-number dpi-y) const/USER-DEFAULT-SCREEN-DPI)])


(defmacro- ckmin [a b]
  (def a-sym (gensym))
  (def b-sym (gensym))
  ~(let [,a-sym ,a
         ,b-sym ,b]
     (if (< ,b-sym ,a-sym)
       (do
         (set ,a ,b-sym)
         true)
       # else
       false)))

#
# Copied from https://en.wikipedia.org/wiki/Hungarian_algorithm
#
(defn hungarian-assignment [C]
  (def J (length C))
  (def W (length (in C 0)))
  (assert (<= J W))

  (def job (array/new-filled (+ 1 W) -1))
  (def ys (array/new-filled J 0))
  (def yt (array/new-filled (+ 1 W) 0))
  (def total-costs @[])
  (def inf math/int-max)

  (for j_cur 0 J
    (var w_cur W)
    (set (job w_cur) j_cur)
    (def min_to (array/new-filled (+ 1 W) inf))
    (def prv (array/new-filled (+ 1 W) -1))
    (def in_Z (array/new-filled (+ 1 W) false))
    (while (not= -1 (in job w_cur))
      (set (in_Z w_cur) true)
      (def j (in job w_cur))
      (var delta inf)
      (var w_next 0)
      (for w 0 W
        (unless (in in_Z w)
          (when (ckmin (min_to w)
                       (- (get-in C [j w])
                          (in ys j)
                          (in yt w)))
            (set (prv w) w_cur))
          (when (ckmin delta (in min_to w))
            (set w_next w))))
      (for w 0 (+ 1 W)
        (if (in in_Z w)
          (do
            (+= (ys (in job w)) delta)
            (-= (yt w) delta))
          # else
          (-= (min_to w) delta)))
      (set w_cur w_next))
    (var w 0)
    (while (not= w_cur W)
      (set w (in prv w_cur))
      (set (job w_cur) (in job w))
      (set w_cur w))
    (array/push total-costs (- (in yt W))))

  [(last total-costs) (slice job 0 W)])


################## Hook Helpers ##################

(defmacro with-activation-hooks [wm & body]
  ~(:with-activation-hooks ,wm (fn [] ,;body)))


################## UIAutomation Helpers ##################

(def- DEBUG-REF-COUNT false)
(var- with-uia-dtor-fn nil)
(if DEBUG-REF-COUNT
  (set with-uia-dtor-fn
       ~(fn [x]
          (unless (nil? x)
            (def refc (:Release x))
            (log/debug "++++ After releasing %n, ref count = %n, stack:\n%s"
                       x
                       refc
                       (get-stack-trace (fiber/current))))))
  (set with-uia-dtor-fn
       ~(fn [x]
          (unless (nil? x)
            (:Release x)))))

(defmacro with-uia [[binding ctor dtor] & body]
  ~(do
     (def ,binding ,ctor)
     ,(apply defer [(or dtor with-uia-dtor-fn) binding] body)))


################## REPL Helpers ##################

(defmacro export-to-repl [repl-server & syms]
  (def quoted-syms-meta @[])
  (loop [i :range [0 (length syms)]]
    (def sym (in syms i))
    (array/push quoted-syms-meta ~(quote ,sym))
    (array/push quoted-syms-meta ~(in (curenv) (quote ,sym))))
  ~(:export ,repl-server ,;quoted-syms-meta))

(defmacro unset-from-repl [repl-server & syms]
  (def quoted-syms
    (map (fn [s] ~(quote ,s)) syms))
  ~(:unset ,repl-server ,;quoted-syms))
