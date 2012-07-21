(ns wfeditor.io.util.const
  (:require [wfeditor.util.const :as global-const]))
;; a class whose purpose is to store constants
;; hopefully, this will help prevent cyclical namespace
;; requirements/dependencies

;;
;; declarations (refs, etc.)
;;

(declare relay-type)

;;
;; relay.server and relay.client constants
;;

(def DEFAULT-HOST "elangocheran.com")

(def DEFAULT-PORT 8080)

(def DEFAULT-LOCAL-HOST "localhost")

(def DEFAULT-LOCAL-PORT 7777)

(def DEFAULT-SERVER-HOST-REL-TO-REMOTE "localhost")

;;
;; thread constants
;;

;; time in msec
(def DEFAULT-REPEATED-BG-THREAD-SLEEP-TIME (* 30 1000))

;;
;; props dir - constants
;;

(def PROPS-DIR-NAME (str "." global-const/PROGRAM-NAME-COMPACT-LC))

(def CONFIG-DIR-NAME "config")

(def DATA-DIR-NAME "data")

(def TASK-RUN-FILE-NAME "job_statuses")


;;
;; execution constants
;;
;;
;; dynamically-set set-once refs ('constant refs'?; initializations happen from elsewhere)
;;

;; this ref is a keyword in the set #{:client :server} to indicate
;; whether the program is being run in client- or server- mode.
;; initialized to :client for at least the sake of REPL's
;; this ref is set in wfeditor.main code
(def relay-type (ref :client))
