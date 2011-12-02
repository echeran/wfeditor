(defproject wfeditor "1.0.0-SNAPSHOT"
  :description "Workflow Welder: a user interface for managing the execution of complex scientific computation on various compute infrastructures"
  :dependencies [[org.clojure/clojure "1.3.0"]
                 ;; clojure-contrib 1.2.1 doesn't exist, 1.2.0 is last on 1.2x branch
                 [org.clojure/clojure-contrib "1.2.0"]
                 [popen "0.1.0"]]
  :disable-deps-clean true
  :aot [wfeditor.main]
  ;:jvm-opts ["-XstartOnFirstThread"]
  :main wfeditor.main
  ;; need to put in an uberjar-exclusion to prevent "lein uberjar"
  ;; from putting in a .sf file in the standalone .jar that causes a
  ;; security exception when run.  This is as according to the SO info
  ;; at http://stackoverflow.com/questions/7892244/leiningen-has-problems-building-a-working-uberjar
  :uberjar-exclusions [#"ECLIPSEF.SF"]
  )
