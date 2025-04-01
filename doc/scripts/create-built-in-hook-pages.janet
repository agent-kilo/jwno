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
      (put hook-names (in expr 2) true)

      (and (= :call-filter-hook (first expr))
           (= :keyword (type (in expr 3))))
      (put hook-names (in expr 3) true)

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

(def hook-list (sort (keys hooks)))
(printf "-- Found %n hooks" (length hook-list))

(def built-in-hooks-toc @[])

(eachp [idx hook-name] hook-list
  (def out-file (string out-path hook-name ".mdz"))
  (def description
    (if (os/stat out-file)
      (do
        (def orig-content (slurp out-file))
        (def desc-header "### Description\n\n")
        (def desc-idx (string/find desc-header orig-content))
        (string/slice orig-content (+ desc-idx (length desc-header))))
      "TODO\n"))

  (def page-template
````
{:title "%n"
 :template "main.html"
 :back-to ["Built-In Hooks" "index.html"]
 :order %d}
---

### Description

%s
````)

  (def content (string/format page-template hook-name (+ 1 idx) description))
  (spit out-file content)
  (printf "-- Created %n" out-file)

  (def li-template "@li{@link[%s.html]{%n}}")
  (array/push built-in-hooks-toc
              (string/format li-template hook-name hook-name))
  )


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
