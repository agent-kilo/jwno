#
# Creates doc/content/ref/built-in-hooks/*.mdz
# Run in the root of the source tree.
#

(def out-path "doc\\content\\ref\\built-in-hooks\\")
(def src-dir "src")


(defn walk-dir [dir-name access-fn]
  (def file-list (os/dir dir-name))
  (each f file-list
    (def file-name (string dir-name "\\" f))
    (def stat (os/stat file-name))
    (case (in stat :mode)
      :file
      (access-fn file-name)

      :directory
      (walk-dir file-name access-fn))))


(defn find-hook-names-impl [expr hook-names]
  (when (= :tuple (type expr))
    (cond
      # (:call-hook ...)
      (and (= :call-hook (first expr))
           (= :keyword (type (in expr 2))))
      (do
        (def args (slice expr 3))
        (put hook-names (in expr 2) [:normal args]))

      # (:call-filter-hook ...)
      (and (= :call-filter-hook (first expr))
           (= :keyword (type (in expr 3))))
      (do
        (def args (slice expr 4))
        (put hook-names (in expr 3) [[:filter (in expr 2)] args]))

      true
      (each e expr
        (find-hook-names-impl e hook-names)))))


(defn find-hook-names [file-name hook-names]
  (printf "-- Searching in %n for hooks" file-name)
  (def content (slurp file-name))
  (def parsed (parse-all content))
  (each p parsed
    (find-hook-names-impl p hook-names)))


(def hooks @{})
(walk-dir src-dir (fn [f] (find-hook-names f hooks)))
#(printf "-- %n" hooks)

(def hook-list (sort (keys hooks)))
(printf "-- Found %n hooks" (length hook-list))

(def built-in-hooks-toc @[])

(eachp [idx hook-name] hook-list
  (def out-file (string out-path hook-name ".mdz"))
  (def default-content "TODO\n")     
  (var gen-state-str nil)
  (def content
    (if (os/stat out-file)
      (do
        (set gen-state-str ":partial")
        (def orig-text (slurp out-file))
        (def content-sep "### Description\n\n")
        (def sep-idx (string/find content-sep orig-text))
        (string/slice orig-text (+ sep-idx (length content-sep)))
        )
      (do
        (set gen-state-str "true")
        default-content)))

  (def hook-info (in hooks hook-name))
  (def type-str
    (match (first hook-info)
      :normal
      "@p{@link[../../using-hooks/normal-hooks.html]{Normal}}"

      [:filter filter-type]
      (string/format "@p{@link[../../using-hooks/filter-hooks.html]{Filter} \\(%s)}" filter-type)

      _
      (errorf "unknown hook type: %n" (first hook-info))))
  (def sig-str
    (let [args (last hook-info)]
      (if (empty? args)
        "(hook-fn)"
        (string "(hook-fn " (string/join args " ") ")"))))

  (def page-template
````
{:title "%n"
 :template "main.html"
 :back-to ["Built-In Hooks" "index.html"]
 :order %d
 :generated %s}
---

### Type

%s

### Function Signature

@codeblock[janet]```
%s
```

### Description

%s
````)

  (def text
    (string/format page-template
                   hook-name
                   (+ 1 idx)
                   gen-state-str
                   type-str
                   sig-str
                   content))
  (spit out-file text)
  (printf "-- Created %n" out-file)

  (def li-template "@li{@link[%s.html]{%n}}")
  (array/push built-in-hooks-toc
              (string/format li-template hook-name hook-name)))


(def built-in-hooks-page-template
````
{:title "Built-In Hooks"
 :template "main.html"
 :back-to ["Reference Index" "../index.html"]
 :order 4
 :generated true}
---

These are the hooks available in Jwno by default.


### Table of Contents

@ul{

%s

}


### See Also

@ul{
  @li{
    @link[../../using-hooks/index.html]{Using Hooks}
  }
}

````)


(def out-file (string out-path "index.mdz"))
(spit out-file (string/format built-in-hooks-page-template
                              (string/join built-in-hooks-toc "\n")))
(printf "-- Generated %n" out-file)
