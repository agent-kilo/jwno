(import ./util)


(def fossil-info-peg
  (peg/compile
   ~{:main (some :info-line)
     :info-line (replace
                 (sequence :info-name ":" :s* :info-value "\n")
                 ,(fn [& info-pair] info-pair))
     :info-name (capture (to ":"))
     :info-value (capture (to "\n"))}))


(defn get-fossil-version []
  (def fossil-info
    (try
      (util/spawn-and-wait "fossil" "info")
      ((_err _fib)
       # Fossil is not installed
       nil)))
  (def info-table @{})
  (when fossil-info
    (each [k v] (peg/match fossil-info-peg fossil-info)
      (put info-table k v)))
  (when-let [hash-and-time (in info-table "checkout")]
    (def matched
      (peg/match ~(sequence (capture (some (range "af" "AF" "09"))) :s* (thru -1))
                  hash-and-time))
    (when matched
      (def ver (in matched 0))
      (def state
        (if (nil? (util/spawn-and-wait "fossil" "changes"))
          :clean
          :dirty))
      [ver state])))


(defn get-git-version []
  (def ver
    (try
      (string/trim
       (util/spawn-and-wait "git" "rev-parse" "HEAD"))
      ((_err _fib)
       nil)))
  (if ver
    (let [state (if (nil? (util/spawn-and-wait "git" "diff"))
                  :clean
                  :dirty)]
      [ver state])
    nil))


(def fossil-origin-name-peg
  (peg/compile
   ~{:main (sequence (to :fossil-origin-name) :fossil-origin-name :fossil-checkout :s* -1)
     :fossil-origin-name (sequence "FossilOrigin-Name:" :s*)
     :fossil-checkout (capture (some (range "af" "AF" "09")))}))


(defn get-git-fossil-origin-version []
  (def git-version (get-git-version))
  (when (nil? git-version)
    (break nil))

  (def [git-ver state] git-version)
  (def git-msg
    (try
      (util/spawn-and-wait "git" "show" "-s" "--format=format:%B" git-ver)
      ((_err _fib)
       nil)))
  (when (nil? git-msg)
    (break nil))

  (def matched (peg/match fossil-origin-name-peg git-msg))
  (when (nil? matched)
    (break nil))

  (def ver (in matched 0))
  [ver state])


(defn get-vcs-version []
  (def fossil-version (get-fossil-version))
  (when fossil-version
    (break [:fossil fossil-version]))

  (def fossil-origin-version (get-git-fossil-origin-version))
  (when fossil-origin-version
    (break [:fossil-origin fossil-origin-version]))

  (def git-version (get-git-version))
  (when git-version
    (break [:git git-version]))

  nil)


(defn format-vcs-version-string [vcs-version abbr-hash-len]
  (defn format-version [prefix version state]
    (def ver
      (string/slice version 0 abbr-hash-len))
    (case state
      :clean
      (string/format "%s-%s" prefix ver)

      :dirty
      (string/format "%s-%s-dirty" prefix ver)))

  (match vcs-version
    [:fossil [fossil-version state]]
    (format-version "fossil" fossil-version state)

    [:fossil-origin [fossil-origin-version state]]
    (format-version "fossil" fossil-origin-version state)

    [:git [git-version state]]
    (format-version "git" git-version state)

    nil
    # There's no version control
    nil))
