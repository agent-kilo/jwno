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


################## Calculations ##################

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
