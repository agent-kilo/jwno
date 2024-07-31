(use jw32/_winuser)


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


################## Calculations ##################

(defmacro rect-width [rect]
  ~(- (in ,rect :right) (in ,rect :left)))

(defmacro rect-height [rect]
  ~(- (in ,rect :bottom) (in ,rect :top)))

(defmacro rect-size [rect]
  ~[(- (in ,rect :right) (in ,rect :left))
    (- (in ,rect :bottom) (in ,rect :top))])

(defn shrink-rect [rect amounts]
  (def top (+ (in rect :top) (in amounts :top 0)))
  (def bottom (- (in rect :bottom) (in amounts :bottom 0)))
  (def left (+ (in rect :left) (in amounts :left 0)))
  (def right (- (in rect :right) (in amounts :right 0)))
  {:top top
   :left left
   :bottom (if (< bottom top) top bottom)
   :right (if (< right left) left right)})

(defn expand-rect [rect amounts]
  (def top (- (in rect :top) (in amounts :top 0)))
  (def bottom (+ (in rect :bottom) (in amounts :bottom 0)))
  (def left (- (in rect :left) (in amounts :left 0)))
  (def right (+ (in rect :right) (in amounts :right 0)))
  {:top top
   :left left
   :bottom (if (< bottom top) top bottom)
   :right (if (< right left) left right)})


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
