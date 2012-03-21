;; based on the StackOverflow question about multi-platform build via
;; leiningen:
;; http://stackoverflow.com/questions/4688336/what-is-an-elegant-way-to-set-up-a-leiningen-project-that-requires-different-dep
(let [properties (select-keys (into {} (System/getProperties))
                              ["os.arch" "os.name"])
      platform (apply format "%s (%s)" (vals properties))
      jvm-opts (if (= "Mac OS X" (get properties "os.name"))
                ["-XstartOnFirstThread"]
                []) 
      ;; swt (case platform
      ;;       "Windows XP (x86)" '[org.eclipse/swt-win32-win32-x86 "3.5.2"]
      ;;       "Linux (x86)"      '[org.eclipse/swt-gtk-linux-x86 "3.5.2"])
      ]
  
  (defproject wfeditor "1.0.0-SNAPSHOT"
    :description "Workflow Welder: a user interface for managing the execution of complex scientific computation on various compute infrastructures"
    :dependencies [[org.clojure/clojure "1.3.0"]
                   ;; clojure-contrib 1.2.1 doesn't exist, 1.2.0 is last on 1.2x branch
                   [org.clojure/clojure-contrib "1.2.0"]

                   ;; the following are actual dependencies, but they
                   ;; cause conflicts in the versions of certain
                   ;; libraries, so I am manually managing the
                   ;; versions of the libraries via the git repo and
                   ;; commenting out these deps so that leiningen
                   ;; doesn't keep overwriting them for each lein deps
                   ;; or lein uberjar operation
                   
                   ;; [popen "0.2.0"]
                   ;; [clj-ssh "0.3.0"]
                   ;; [noir "1.2.1"]
                   ]
    :disable-deps-clean true
    :aot [wfeditor.main]
    :jvm-opts ~jvm-opts
    :main wfeditor.main
    ;; need to put in an uberjar-exclusion to prevent "lein uberjar"
    ;; from putting in a .sf file in the standalone .jar that causes a
    ;; security exception when run.  This is as according to the SO info
    ;; at http://stackoverflow.com/questions/7892244/leiningen-has-problems-building-a-working-uberjar
    :uberjar-exclusions [#"ECLIPSEF.SF"
                         #"wfeditor.*\.clj$"
                         #"project.clj"
                         #"code_jam.*.clj$"
                         #"sample.*xml$"]
  ))