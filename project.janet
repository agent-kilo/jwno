(declare-project
 :name "Jwno"
 :description "A window manager built with Janet."
 :dependencies [{:url "https://github.com/janet-lang/spork.git"
                 :tag "d644da0fd05612a2d5a3c97277bf7b9bb96dcf6b"}])


(declare-executable
 :name "jwno"
 :entry "src/main.janet"
 :deps (->> (os/dir "src")
            (filter |(string/has-suffix? ".janet" $))
            (map |(string "src/" $))))
