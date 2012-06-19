(ns wfeditor.io.util.const)
;; a class whose purpose is to store constants
;; hopefully, this will help prevent cyclical namespace
;; requirements/dependencies


;;
;; relay.server and relay.client constants
;;

(def DEFAULT-HOST "elangocheran.com")

(def DEFAULT-PORT 8080)

(def DEFAULT-LOCAL-HOST "localhost")

(def DEFAULT-LOCAL-PORT 7777)

(def DEFAULT-SERVER-HOST-REL-TO-REMOTE "localhost")

;;
;; execution constants
;;

;; let this dir path be relative to the user's home dir
(def DEFAULT-HOME-OUTPUT-DIR "/sge/qsub/")

;;
;; thread constants
;;

;; time in msec
(def DEFAULT-REPEATED-BG-THREAD-SLEEP-TIME (* 30 1000))

;;
;; status constants
;;

(def CONFIG-FILE-DIR-NAME ".wfeditor")

(def TASK-RUN-FILE-NAME "job_statuses")