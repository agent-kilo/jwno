(use jw32/_winuser)


(defn show-error-and-exit [msg exit-code]
  (MessageBox nil
              (string/format "Error: %s" msg)
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
