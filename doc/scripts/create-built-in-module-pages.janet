#
# Creates doc/content/ref/built-in-modules/*.mdz
# Run in the root of the source tree.
#


(import /src/mod)

(def out-path "doc\\content\\ref\\built-in-modules\\")

(def module-man (mod/module-manager))

(def jwno-mod-list
  (array/concat @["jwno/user-config"]
                (filter |(string/has-prefix? "jwno/" $)
                        (keys (table/proto-flatten (in module-man :cache))))))
(def mod-list (sort jwno-mod-list))

(def built-in-modules-toc @[])

(eachp [idx mod-name] mod-list
  (def page-file-name (string/replace-all "/" "--" mod-name))
  (def out-file (string out-path page-file-name ".mdz"))

  (def default-content
````
### Synopsis

TODO

### Description

TODO

````)

  (def page-template
````
{:title "%s"
 :template "main.html"
 :back-to ["Built-In Modules" "index.html"]
 :order %d
 :generated %s}
---

%s
````)

  (var gen-state-str nil)
  (def content
    (if (os/stat out-file)
      (do
        (set gen-state-str ":partial")
        (def orig-text (slurp out-file))
        (def content-sep "---\n\n")
        (def sep-idx (string/find content-sep orig-text))
        (string/slice orig-text (+ sep-idx (length content-sep))))
      # else
      (do
        (set gen-state-str "true")
        default-content)))

  (def text
    (string/format page-template mod-name idx gen-state-str content))
  (spit out-file text)
  (printf "-- Generated %n" out-file)

  (def li-template "@li{@link[%s.html]{%s}}")
  (array/push built-in-modules-toc
              (string/format li-template page-file-name mod-name)))


(def built-in-modules-page-template
````
{:title "Built-In Modules"
 :template "main.html"
 :back-to ["Reference Index" "../index.html"]
 :order 5
 :generated true}
---

These are the modules available in Jwno by default.


### Table of Contents

@ul{

%s

}


### See Also

@ul{
  @li{
    @link[../../using-modules.html]{Using Modules}
  }
}

````)

(def out-file (string out-path "index.mdz"))
(spit out-file (string/format built-in-modules-page-template
                              (string/join built-in-modules-toc "\n")))
(printf "-- Generated %n" out-file)
