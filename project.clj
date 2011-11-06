(defproject wfeditor "1.0.0-SNAPSHOT"
  :description "Workflow Welder: a user interface for managing the execution of complex scientific computation on various compute infrastructures"
  :dependencies [[org.clojure/clojure "1.2.1"]
                 ;; clojure-contrib 1.2.1 doesn't exist, 1.2.0 is last on 1.2x branch
                 [org.clojure/clojure-contrib "1.2.0"]]
  :disable-deps-clean true
  :aot [wfeditor.core]
  :jvm-opts ["-XstartOnFirstThread"])