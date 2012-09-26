(ns wfeditor.util.const)
;; a class to store globally-relevant constants
;; constants here may or may not be called directly in executing code,
;; but where possible, it should be used by model, io, or gui const
;; namespaces and used by respective 'domains' of MVC.  (e.g.,
;; official branding in the GUI might be different from the file
;; system names used by I/O ops)

;;
;; program name(s)
;;

(def PROGRAM-NAME "Workflow Commander")

(def PROGRAM-NAME-COMPACT "WFCommander")

(def PROGRAM-NAME-ABBR "WFC")


(def PROGRAM-NAME-LC (.toLowerCase PROGRAM-NAME))

(def PROGRAM-NAME-COMPACT-LC (.toLowerCase PROGRAM-NAME-COMPACT))

(def PROGRAM-NAME-ABBR-LC (.toLowerCase PROGRAM-NAME-ABBR))