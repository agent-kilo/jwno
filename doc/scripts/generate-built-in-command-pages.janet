#
# Generates doc/content/ref/built-in-commands/*.mdz
# Run in the root of the source tree.
#


(import /src/cmd)

(def out-path "doc\\content\\ref\\built-in-commands\\")

(def command-man (cmd/command-manager :dummy-hook-man))
(cmd/add-default-commands command-man @{})


(def cmd-list (sort (keys (in command-man :commands))))
#(printf "Built-In Commands: %n" cmd-list)

(def built-in-commands-toc @[])

(eachp [idx cmd-name] cmd-list
  (def doc (get-in command-man [:commands cmd-name :doc]))
  (def syno-sep "\n\n")
  (def syno-end (string/find syno-sep doc))
  (def synopsis (string/slice doc 0 syno-end))
  (def description (string/slice doc (+ syno-end (length syno-sep))))

  (def page-template
````
{:title "%n"
 :template "main.html"
 :back-to ["Built-In Commands" "index.html"]
 :order %d
 :generated true}
---

### Synopsis

@codeblock[janet]```
%s
```

### Description

%s

````)

  (def content
    (string/format page-template
                   cmd-name
                   (+ 1 idx)
                   synopsis
                   description))

  (def out-file (string out-path cmd-name ".mdz"))
  (spit out-file content)
  (printf "-- Generated %n" out-file)

  (def li-template "@li{@link[%s.html]{%n}}")
  (array/push built-in-commands-toc
              (string/format li-template cmd-name cmd-name)))


(def built-in-commands-page-template
````
{:title "Built-In Commands"
 :template "main.html"
 :back-to ["Reference Index" "../index.html"]
 :order 3
 :generated true}
---

These are the commands available in Jwno by default.


### Table of Contents

@ul{

%s

}


### See Also

@ul{
  @li{
    @link[../../using-commands/index.html]{Using Commands}
  }
}

````)


(def out-file (string out-path "index.mdz"))
(spit out-file (string/format built-in-commands-page-template
                              (string/join built-in-commands-toc "\n")))
(printf "-- Generated %n" out-file)
