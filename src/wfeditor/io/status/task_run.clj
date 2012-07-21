(ns wfeditor.io.status.task-run
  (:require [wfeditor.io.util.dir :as dir-util]
            [clojure.contrib.map-utils :as map-utils]
            [wfeditor.model.workflow :as wflow]
            [clojure.string :as string]
            [clj-commons-exec :as commons-exec]
            [cheshire.core :as cheshire]
            [fs.core :as fs]))

;;
;; refs (declarations here, initial bindings below)
;;

;; note: an add-watch statement was attached to this ref in wfeditor.ui.gui.editor-left
;;
;; nested structure containing the status of all jobs run
;; in server mode, stores locally-run jobs
;; in client mode, stores all jobs known across domains
(declare global-job-statuses)

;;
;; functions
;;

;;
;; util functions
;;

(defn update-map
  "a utility function that updates the contents of the base map by adding/replacing them with the values of the newer map, and doing this recursively through the map structure according to clojure.contrib.map-utils/deep-merge-with"
  [base-map newer-map]
  (letfn [(merge-fn [& vals] (last (concat vals)))]
    (map-utils/deep-merge-with merge-fn base-map newer-map))) 

;;
;; global status functions
;;

(defn global-statuses
  "a convenience function to deref the global-job-statuses map ref. for as many values are provided, the sub-map given from nested get calls (as given by get-in) will be returned"
  ([& levels]
     (get-in @global-job-statuses levels)))

(defn update-global-statuses-with-new-statuses
  "given a map of new global statuses, do a deep merge of the new map into our existing global-job-statuses"
  [new-status-map]
  (dosync
   (alter global-job-statuses update-map new-status-map)))

(defn add-wfinst-to-global-statuses
  "take the status information from the input wfinst"
  [wfinst]
  (let [exec-domain (:exec-domain wfinst)
        wf (:workflow wfinst)
        jobs (wflow/wf-jobs wf)
        newer-info-map {exec-domain (into {} (for [job jobs]
                                           [(:id job) (:task-statuses job)]))}]
    (update-global-statuses-with-new-statuses newer-info-map)))

;;
;; global status functions - server process-specific
;;

(defn done-job-state
  "for jobs that have run and stopped running -- either because of success, error, or being killed -- return the state as a keyword accordingly"
  [jid]
  (let [qacct-done-state-cmd-parts ["qacct" "-j" (str jid)]
        done-state-prom (commons-exec/sh qacct-done-state-cmd-parts {:handle-quoting? true})
        done-state-result @done-state-prom]
    (cond
     (or (not= 0 (:exit done-state-result)) (not (:out done-state-result))) :killed
     :else :error)))

(defn update-global-statuses
  "update the information of job execution statuses. works only for SGE, and needs work to be generalizable. providing a nil username means job statuses for all users are updated. nil exec-domain defaults to SGE"
  ([]
     (update-global-statuses nil nil))
  ([exec-domain]
     (update-global-statuses exec-domain nil))
  ([exec-domain username]
     ;; TODO: get rid of magic value default exec-domain being "SGE"
     ;; TODO: figure out a way to prevent user A from connecting to
     ;; user B's user-land instance of server and triggering saving status of
     ;; all users' jobs.  i assume that this could be desirable.  will
     ;; probably require a white-listing of superusers. if no
     ;; consensus among userbase exists, then i personally think it
     ;; should be implemented so.
     (let [exec-domain (or exec-domain "SGE")
           qstat-status-map-fn (fn [qstat-out-str]
                                 (if qstat-out-str
                                   (with-open [rdr (java.io.BufferedReader. (java.io.StringReader. qstat-out-str))]
                                     (reduce (fn [m [jid user status]] (assoc-in m [user jid] status)) {}
                                             (map (juxt #(Integer/parseInt (nth % 0)) #(nth % 3) #(nth % 4))
                                                  (map #(remove (fn [s] (or (nil? s) (= "" s)) ) %)
                                                       (map #(string/split % #"\s") (drop 2 (line-seq rdr)))))))
                                   {}))
           qstat-recently-done-cmd-parts []
           ;; using sudo regardless hopefully prevents user A from
           ;; connecting as user B (see TODO above)
           qstat-recently-done-cmd-parts (into qstat-recently-done-cmd-parts (if (and username (not= username (. System getProperty "user.name"))) ["sudo" "-u" username "-i"] ["sudo" "-u" (. System getProperty "user.name") "-i"]))
           qstat-recently-done-cmd-parts (into qstat-recently-done-cmd-parts ["qstat" "-s" "z" "-u" (or username "\"*\"")])
           recently-done-prom (commons-exec/sh qstat-recently-done-cmd-parts {:handle-quoting? true})
           ;; TODO: add a timeout to the exec/sh call opts map
           recently-done-result @recently-done-prom
           recently-done-map (qstat-status-map-fn (:out recently-done-result))
           qstat-not-done-cmd-parts []
           qstat-not-done-cmd-parts (into qstat-not-done-cmd-parts (if (and username (not= username (. System getProperty "user.name"))) ["sudo" "-u" username "-i"] ["sudo" "-u" (. System getProperty "user.name") "-i"]))
           qstat-not-done-cmd-parts (into qstat-not-done-cmd-parts ["qstat" "-u" (or username "'*'")])
           not-done-prom (commons-exec/sh qstat-not-done-cmd-parts {:handle-quoting? true})
           ;; TODO: add a timeout to the exec/sh call opts map
           not-done-result @not-done-prom
           not-done-map (qstat-status-map-fn (:out not-done-result))
           new-status-map {}
           new-status-map (reduce update-map new-status-map (for [[user user-map] not-done-map
                                                                  [jid sge-status-str] user-map]
                                                              (let [task-id 0
                                                                    status (condp = sge-status-str
                                                                             "r" :running
                                                                             "qw" :waiting
                                                                             "hqw" :waiting
                                                                             "Eqw" :error
                                                                             :uncertain)]
                                                                {user {jid {task-id status}}})))
           new-status-map (reduce update-map new-status-map (for [[user user-map] recently-done-map
                                                                  [jid sge-status-str] user-map]
                                                              (let [task-id 0
                                                                    status :done]
                                                                {user {jid {task-id status}}})))
           global-status-update-map {exec-domain new-status-map}]
       (update-global-statuses-with-new-statuses global-status-update-map))))

;;
;; task status file read/write operation functions
;;

;; btw: here is a page describing persisting info in Clojure, and why
;; he thinks Clojure's data structures + eval is better than XML /
;; JSON:
;; http://amalloy.hubpages.com/hub/Dont-use-XML-JSON-for-Clojure-only-persistence-messaging
;; I still disagree at the moment

(defn- parsed-statuses-map-flattened-entries
  "this function takes the nested map of statuses given as input (or global-job-statuses if no argument supplied), assumes each 'leaf' value is at the same depth, and returns a seq of vectors, where each vector is like the concatenation of the 'coordinates' (as used by get-in and assoc-in) and the 'leaf' value.  the input nested map is assumed to be given by the Cheshire (JSON) parser.  the function also operates on values according to the structure of the global-job-statuses map. this function was initially created for further parsing (transforming) the output of Cheshire's parsing of JSON" 
  ([parsed-statuses-map]
     (for [[exec-dom ed-map] parsed-statuses-map
           [username user-map] ed-map
           [job-id-str job-map] user-map
           [task-id-str status-str] job-map]
       [exec-dom username job-id-str task-id-str status-str])))

(defn- transform-statuses-map-flattened-entry
  "the input of this function is an entry, as given by statuses-map-flattened-entries, which is of type vector.the entry's values are assumed to come from parsed-statuses-map-flattened-entries, which takes Cheshire (JSON) parser output as its input -- therefore, values as given by Cheshire's parser are transformed by this function into the right type"
  [entry]
  (let [[exec-dom username job-id-str task-id-str status-str] entry]
    [exec-dom username (Integer/parseInt job-id-str) (Integer/parseInt task-id-str) (keyword status-str)]))

(defn json-to-statuses-map
  "given a JSON string representing the nested map representing job statuses, return a map of job statuses constructed just as global-job-statuses is constructed"
  [statuses-map-json-str]
  (let [parsed-map (cheshire/parse-string statuses-map-json-str)
        parsed-map-entries (parsed-statuses-map-flattened-entries parsed-map)
        transformed-parsed-map-entries (for [entry parsed-map-entries]
                                         (transform-statuses-map-flattened-entry entry))
        transformed-map (loop [new-map {}
                               entries transformed-parsed-map-entries]
                          (if (empty? entries)
                            new-map
                            (let [entry (first entries)
                                  assoc-in-addr (butlast entry)
                                  val (last entry)]
                              (recur (assoc-in new-map assoc-in-addr val) (rest entries)))))]
    transformed-map))

(defn statuses-map-to-json
  "return the JSON representation of the job statuses map. if no arguments, use global-job-statuses as input"
  ([]
     (statuses-map-to-json @global-job-statuses))
  ([statuses-map]
     (cheshire/generate-string statuses-map)))

(defn file-to-statuses
  "read contents of file, which is JSON-encoded version of global job statuses, and return type structure-specific formatted version. if no file provided, then file returned by task-run-file used"
  ([]
     (file-to-statuses (dir-util/task-run-file)))
  ([file]
     (let [json-str (slurp file)
           statuses-map (json-to-statuses-map json-str)]
       statuses-map)))

(defn statuses-to-file
  "save global-job-statuses map in JSON format to the provided file. if no file provided, then file returned by task-run-file used"
  ([]
     (statuses-to-file (dir-util/task-run-file)))
  ([file]
     (spit file (statuses-map-to-json))))

;;
;; pre-execution initialization functions
;;

(defn initialize-task-status-file-ops
  "do initialization work so that dirs and files exist where statuses should be stored, and any existing status info is loaded. dirs and files will exist in user that is running the process"
  []
  (let [dir-structure-leaves [(dir-util/config-dir) (dir-util/data-dir)]]
    (dorun (map fs/mkdirs dir-structure-leaves)))
  (let [task-run-file (dir-util/task-run-file)]
    (if (fs/exists? task-run-file)
      (let [restored-job-statuses-map (file-to-statuses task-run-file)]
        (dosync
         (ref-set global-job-statuses restored-job-statuses-map)))
      (fs/touch task-run-file))))


;;
;; ref initializations
;;

(def global-job-statuses (ref {}))
