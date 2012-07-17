(ns wfeditor.io.util.const
  (:require [wfeditor.util.const :as global-const]
            [fs.core :as fs]))
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
;; props dir - functions
;;

(defn- props-dir
  "return the full path of the directory in which all the properties info of the program (running as this user) is stored as a Java File object. this directory should also be specific to whether the program is being run as a client or a server (i.e., server mode should have a separate configuration from client mode). ('properties' includes job output files, configuration, etc., and by 'properties info directory' I basically mean the dot-directory in the home directory)"
  []
  (let [home-dir (fs/home)
        relay-type-name (name @relay-type)
        props-dir (fs/file home-dir PROPS-DIR-NAME relay-type-name)]
    props-dir))

(defn config-dir
  "return the full path of the directory in which the configuration info of the program is stored. similar to props-dir, and should be within the props-dir"
  []
  (fs/file (props-dir) CONFIG-DIR-NAME))

(defn data-dir
  "return the full path of the directory in which the data of the program are stored. similar to props-dir, and should be within the props-dir"
  []
  (fs/file (props-dir) DATA-DIR-NAME))

(defn task-run-file
  "a File object representing the file where the jobs' tasks' statuses are stored"
  []
  (fs/file (config-dir) TASK-RUN-FILE-NAME))

;;
;; execution constants
;;

;; let this dir path be relative to the user's home dir
(def DEFAULT-HOME-OUTPUT-DIR "/sge/qsub/")

;;
;; dynamically-set set-once refs ('constant refs'?; initializations happen from elsewhere)
;;

;; this ref is a keyword in the set #{:client :server} to indicate
;; whether the program is being run in client- or server- mode.
;; initialized to :client for at least the sake of REPL's
;; this ref is set in wfeditor.main code
(def relay-type (ref :client))
