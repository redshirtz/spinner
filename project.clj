(defproject redshirtz/spinner "0.1.0-SNAPSHOT"
  :description "Word spinning library"
  :global-vars {*warn-on-reflection* true}
  :dependencies [ [org.clojure/clojure "1.8.0"]
                  [clojure-csv "2.0.1"] ]
  :pom-plugins [[com.theoryinpractise/clojure-maven-plugin "1.3.20"
                 {:extensions "true"
                  :configuration ([:sourceDirectories [:sourceDirectory "src"]]
                                  [:temporaryOutputDirectory "true"])
                  :executions [:execution
                               [:id "compile-clojure"]
                               [:phase "compile"]
                               [:goals [:goal "compile"]]]}]])
