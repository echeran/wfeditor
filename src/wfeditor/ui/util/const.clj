(ns wfeditor.ui.util.const
  (:require [wfeditor.util.const :as global-const]))

;;
;; names and titles of program, app, etc.
;;

(def WINDOW-APP-NAME global-const/PROGRAM-NAME)

(def WINDOW-TITLEBAR global-const/PROGRAM-NAME)

;;
;; left nav pane - Edit WF tab - Job editor table
;;

(def NIL-VAL-STR-REP "")

(def JOB-FIELD-FULL-NAMES {:id "Job ID" :name "Job Name" :desc "Program Description" :prog-name "Program Name" :prog-ver "Program Version" :prog-exec-loc "Executable Location" :prog-exec-ver "Executable Version" :prog-args "Arguments" :prog-opts "Options" :std-out-file "Std. Out. File" :std-err-file "Std. Err. File" :task-statuses "Job/Task Status(es)" :array "Array Task IDs"})

(def PREDEFINED-WF-FIELD-FULL-NAMES {:name "WF Name" :url "URL" :version "WF Version" :desc "Description" :citation "Publication" :institution "Institution" :author "Author" :contact "Contact" :website "Website"})