(ns wfeditor.io.execution
  (:require [clojure.contrib.graph :as contrib-graph]
            [clojure.contrib.map-utils :as map-utils]
            [wfeditor.model.workflow :as wflow]
            [clojure.string :as string]
            [popen :as popen]
            [clj-commons-exec :as commons-exec]
            [wfeditor.io.relay.client :as wfeclient])
  (:import wfeditor.model.workflow.Job))

;;
;; globals
;;

;; the separator between 2 option/value pairs, 2 arguments, or a
;; program and its options/argumetns
(def sep " ")
;; the separator between an option and its value;
;; this might vary from one program to the next,
;; in which case it should be refactored as not a global
(def opt-val-sep " ")
;; the separator between 2 jobs' command
(def job-comm-sep " | ")
;; the beginning of the 'complex command' when the flow splits
(def branch-split-begin "tee 1>/dev/null ")
;; the part that goes before each job that is a branch
(def branch-split-job-prefix ">(")
;; the part that goes after each job that is a branch
(def branch-split-job-suffix ")")
;; what separates jobs that branch
(def branch-split-sep " ")
;; the end of the 'complex command' when the flow splits
(def branch-split-end "")

(def merge-split-begin "(")
(def merge-split-job-prefix "")
(def merge-split-job-suffix "")
(def merge-split-sep ";")
(def merge-split-end ")")


;;
;; refs (declarations here, initial bindings below)
;;

;; nested structure containing the status of all jobs run
;; in server mode, stores locally-run jobs
;; in client mode, stores all jobs known across domains
(declare global-job-statuses)
;; TODO: save this to file periodically, and if the server restarts or
;; crahess, pick up the latest value from file

;; counter for unique id's to assign internally to jobs (akin to SQL autoincrement)
(declare internal-job-id-counter)
;; TODO: save this to file periodically, and if the server restarts,
;; pick up the latest value of the counter from file

;; a map to translate grid engine job id's to internal job id's
(declare job-id-translate-map)

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
    (map-utils/deep-merge-with base-map newer-map)))

;;
;; functions for piped shell commands
;;

(defn opts-list
  "convert an options map into a list of strings of the flags and/or option/value pairs.
the options map should be specified as keys being the option flags as strings (e.g., \"-d\") and the values being vectors of that option flag's values
the vals vector is nil if the option is a flag (e.g. \"--verbose\"). the vals vector will often times be a vector of one element, but a few, oddly-written programs will have a single option repeated multiple times (e.g. --input=file1.txt --input=file2.txt ...)"
  [opts-map]
  (flatten
   (for [[opt vals] opts-map]
     (if (or (not vals) (empty? vals))
       (str opt)
       (if (string? vals)
         (string/join opt-val-sep [opt vals])
         (for [val vals]
           (string/join opt-val-sep [opt val])))))))

(defn opts-str
  "generate a string for a program's command-line options and/or flags, given the options list, as given by opts-list. "
  [opts]
  (string/join sep (opts-list opts)))

(defn job-command
  "generate a string (or some other output?) representing the command(s) to run the graph."
  [job]
  (let [exec (:prog-exec-loc job)
        args (:prog-args job)
        opts (:prog-opts job)
        args-str (string/join sep args)
        opts-str (opts-str opts)]
    (string/join sep [exec args-str opts-str])))

(defn wf-command-linked-list
  "return the command(s) necessary to run all of the jobs in the workflow according to the dependencies specified.  this assumes that there is only one path in the dependency graph"
  [wf]
  (let [dep-graph (wflow/dep-graph wf)
        dep-levels (contrib-graph/dependency-list dep-graph)
        wf-comm (string/join job-comm-sep (map (comp job-command first) dep-levels))]
    wf-comm))

(defn wf-command-one-branch
  "return the command(s) necessary to run all of the jobs in the workflow according to the dependencies specified.  this assumes that there is only one job that has multiple streams depending on it (i.e., multiple 'branches')."
  ([wf]
     (let [dep-levels (contrib-graph/dependency-list (wflow/dep-graph wf))
           first-job (first (first dep-levels))
           flow-graph (wflow/flow-graph wf)
           jobs (:nodes flow-graph)
           flow-out-degrees (for [job jobs] (count ((:neighbors flow-graph) job)))]
       (when (>= 1 (count (filter #(< 1 %) flow-out-degrees)))
         (if-let [branch-job (first (filter #(< 1 (count ((:neighbors flow-graph) %))) jobs))]
           (wf-command-one-branch wf branch-job)
           (letfn [(leaf-job [job] (let [children ((:neighbors flow-graph) job)] (if (seq children) (leaf-job (first children)) job)))]
             (wf-command-one-branch wf (leaf-job first-job)))))))
  ([wf curr-job]
     (let [[final-job cumulative-cmds] (wf-command-one-branch wf curr-job {})]
       ;; this is where we return the string value representing the
       ;; piped shell command on behalf of the entire function
       (cumulative-cmds final-job)))
  ([wf curr-job cumulative-cmds]
     ;; this part of the function is meant for implementation purposes only
     (let [flow-graph (wflow/flow-graph wf)
           dep-graph (wflow/dep-graph wf)
           ;; a map that will end up mapping all ancestor jobs to the
           ;; string of the command representing its ancestor sub-tree (self-inclusive)
           cumulative-cmds (if (get cumulative-cmds curr-job)
                             cumulative-cmds
                             (let [upstream-jobs ((:neighbors dep-graph) curr-job)
                                   new-upstream-jobs (remove (set (keys cumulative-cmds)) upstream-jobs)
                                   ;; where newly-seen ancestor job(s)
                                   ;; are added to the map
                                   cumulative-cmds (loop [cumul-cmds cumulative-cmds
                                                          up-jobs new-upstream-jobs]
                                                     (if (empty? up-jobs)
                                                       cumul-cmds
                                                       (let [job (first up-jobs)
                                                             [j ccmds vjobs] (wf-command-one-branch wf job cumul-cmds)]
                                                         (recur ccmds (rest up-jobs)))))
                                   ;; this is added especially because
                                   ;; of the hack below to allow the
                                   ;; proper accumulation of the
                                   ;; downstream branches of the job
                                   ;; with out-degree >= 2
                                   upstream-cmds (remove nil? (for [job upstream-jobs] (cumulative-cmds job)))
                                   ;; the string representation of the
                                   ;; ancestor sub-tree (not
                                   ;; self-inclusive) is formed
                                   upstream-cmd (condp = (count upstream-cmds)
                                                  0 ""
                                                  1 (str (first upstream-cmds) job-comm-sep)
                                                  (str merge-split-begin
                                                       (string/join merge-split-sep (map #(str merge-split-job-prefix % merge-split-job-suffix) upstream-cmds))
                                                       merge-split-end
                                                       job-comm-sep))
                                   ;; now the current job's
                                   ;; command is created and the
                                   ;; ancestor sub-tree's command
                                   ;; string is combined in order to
                                   ;; include the current job in the map
                                   curr-cumul-cmd (str upstream-cmd
                                                       (job-command curr-job))
                                   cumulative-cmds (assoc cumulative-cmds curr-job curr-cumul-cmd)
                                   ]
                               cumulative-cmds))
           ]
       (if (>= 1 (count ((:neighbors flow-graph) curr-job)))
         ;; if the out-degree of the job in the flow-graph is >=1, then
         ;; we're either returning from a recursive call to get the
         ;; ancestor subtree (out-degree == 1), or we're at the leaf
         ;; node (out-degree == 0), which is the base case, so return
         [curr-job cumulative-cmds]
         ;; if the out-degree of the job is > 1, then this job is a
         ;; branch, and this is the only one permissible in a piped
         ;; shell command graph.  so we determine what the branches
         ;; are, add it to this job's cumulative command and return.
         ;; permissibility is a function of what is representable as
         ;; piped shell command.  the branches are also called in the
         ;; code as 'downstream subtrees', and they can even look like
         ;; full trees so long as their out-degree is not > 1 (since
         ;; there can only be one such node in the graph with
         ;; out-degree > 1)
         (letfn [(leaf-job [job] (let [children ((:neighbors flow-graph) job)] (if (seq children) (leaf-job (first children)) job)))]
           (let [branch-jobs ((:neighbors flow-graph) curr-job)
                 branch-leaf-jobs (map leaf-job branch-jobs)
                 branch-cmd-fn (fn [branch-leaf-job] (let [[job cumul-cmds] (wf-command-one-branch wf branch-leaf-job {curr-job nil})] (cumul-cmds branch-leaf-job)))
                 branch-cmds (map branch-cmd-fn branch-leaf-jobs)
                 full-downstream-cmd (str branch-split-begin
                                          (string/join branch-split-sep (map #(str branch-split-job-prefix % branch-split-job-suffix) branch-cmds))
                                          branch-split-end)
                 full-curr-cmd (str (cumulative-cmds curr-job) job-comm-sep full-downstream-cmd)
                 new-cumulative-cmds (assoc cumulative-cmds curr-job full-curr-cmd)]
             [curr-job new-cumulative-cmds]
             ;; at this point, we're finished with the
             ;; 'implementation' part of the function, and we assume
             ;; that we return to the part of the function with
             ;; different arity which we assume any external caller
             ;; ultimately came through
             ))))))

(defn wf-command-no-branch
  "return the command(s) necessary to run all of the jobs in the workflow according to the dependencies specified.  this assumes that there no job has multiple streams depending on it (i.e., multiple 'branches')."
  ([wf]
     (let [dep-levels (contrib-graph/dependency-list (wflow/dep-graph wf))
           first-job (first (first dep-levels))]
       (wf-command-no-branch wf first-job nil)))
  ([wf root-job parent-job]
     (let [flow-graph (wflow/flow-graph wf)
           dep-graph (wflow/dep-graph wf)
           upstream-jobs (remove #(= % parent-job) ((:neighbors dep-graph) root-job))
           downstream-jobs ((:neighbors flow-graph) root-job)]
       (condp = (count downstream-jobs)
         0 (job-command root-job)
         1 (str (job-command root-job) job-comm-sep (wf-command-no-branch wf (first downstream-jobs) root-job))
         (str (job-command root-job) job-comm-sep branch-split-begin (string/join branch-split-sep (map (fn [job] (str branch-split-job-prefix (wf-command-no-branch wf job root-job) branch-split-job-suffix)) downstream-jobs)) branch-split-end)))))

(defn print-still-running
  "run a function for the provided number of times in a row (reps), where the function prints whether the provided popen process (proc) is still running"
  [proc reps]
  (let [is-running-fn (fn [] (println "wf command(s) still running?=" (popen/running? proc)) (Thread/sleep 500))]
    (dotimes [_ reps]
      (.start
       (Thread. is-running-fn)))))

(def wf-command wf-command-one-branch)

(defn print-wf-command
  [wf]
  (let [wf-comm (wf-command wf)]
    (println "wf-command=" wf-comm)))

(defn run-workflow
  "run a workflow of jobs"
  [wf]
  (let [wf-comm (wf-command wf)]
    (let [proc (popen/popen ["/bin/bash" "-c" wf-comm])
          output (slurp (popen/stdout proc))]
      (println "output of wf command(s)=" output))))

;;
;; functions for running jobs on a grid engine
;;

(defn- assoc-internal-job-id
  "create an entry in the global map to translate the grid engine id to the internal id, and return global map"
  [grid-engine-id internal-id]
  (dosync
   (alter job-id-translate-map assoc grid-engine-id internal-id)))

(defn- next-internal-id
  "increment and return the next internal job id from the counter"
  []
  (swap! internal-job-id-counter inc))

(defn- enqueue-job-sge
  "enqueue a job using SGE and return the job with the new id"
  ([wf job]
     (enqueue-job-sge nil wf job))
  ([username wf job]
     (let [job-name (:name job)
           deps (wflow/depends-upon wf job)
           hold_jid_parts (when (seq deps)
                            ["-hold_jid" (string/join "," (map :id deps))])
           job-cmd-str (job-command job)
           internal-job-id (next-internal-id)
           qsub-cmd-parts []
           qsub-cmd-parts (into qsub-cmd-parts (when username ["sudo" "-u" username "-i"]))
           std-out-file (or (:std-out-file job) (str "/home/echeran/sge/qsub/" internal-job-id ".out"))
           std-err-file (or (:std-err-file job) (str "/home/echeran/sge/qsub/" internal-job-id ".err"))
           qsub-cmd-parts (into qsub-cmd-parts ["qsub" "-o" std-out-file "-e" std-err-file])
           qsub-cmd-parts (into qsub-cmd-parts hold_jid_parts)
           commons-exec-sh-opts-map {:in job-cmd-str :flush-input? true}
           commons-exec-sh-all-args (conj qsub-cmd-parts commons-exec-sh-opts-map)
           result-map-prom (apply commons-exec/sh commons-exec-sh-all-args)
           qsub-output (:out @result-map-prom)
           qsub-job-id (Integer/parseInt (nth (string/split qsub-output #"\s+") 2))]
       (do
         (assoc-internal-job-id qsub-job-id internal-job-id))
       (assoc job
         :id qsub-job-id
         :std-out-file std-out-file
         :std-err-file std-err-file))))

(defn enqueue-wfinst-sge
  "enqueue the WFInstance using SGE and return the workflow with the new job id's"
  [wfinst]
  (let [username (:username wfinst)
        wf (:workflow wfinst)
        dep-order-job-seq (wflow/wf-job-seq wf)
        new-wf (loop [current-wf wf
                      jobs dep-order-job-seq]
                 (if (empty? jobs)
                   current-wf
                   (let [job (first jobs)
                         job-with-id (enqueue-job-sge username current-wf job)
                         new-wf (wflow/replace-job current-wf job job-with-id)]
                     (recur new-wf (rest jobs)))))
        new-wfinst (assoc wfinst :workflow new-wf)]
    new-wfinst))

(defn cmds-in-dep-order
  "return a sequence of job commands in order of which is depended on by which following jobs"
  [wf]
  (let [dep-order-job-seq (wflow/wf-job-seq wf)
        jobs (wflow/wf-jobs wf)]
    (for [job dep-order-job-seq]
      (let [job-name (:name job)
            deps (wflow/depends-upon wf job)
            deps-str (if (seq deps)
                       (str "-hold_jid " (string/join "," (map :id deps)))
                       "")
            job-id (:id job)
            cmd (string/join " " [(str "[" job-id  "]") "qsub" job-name deps-str])]
        cmd))))

(defn print-deps-in-order
  "print the jobs in order of which is depended on by following jobs"
  [wf]
  (let [cmd-seq (cmds-in-dep-order wf)]
    (doseq [cmd cmd-seq]
      (println cmd))))



;;
;; global status functions
;;

(defn global-statuses
  "a convenience function to deref the global-job-statuses map ref"
  []
  @global-job-statuses)

(defn add-wfinst-to-global-statuses
  "take the status information from the input wfinst"
  [wfinst]
  (let [exec-domain (:exec-domain wfinst)
        wf (:workflow wfinst)
        jobs (wflow/wf-jobs wf)
        newer-info-map {exec-domain (into {} (for [job jobs]
                                           [(:id job) (:task-statuses job)]))}]
    (dosync
     (alter global-job-statuses update-map newer-info-map))))

;;
;; client-process execution functions
;;

(defmulti update-wfinst (comp :exec-domain first vector))
(defmethod update-wfinst "SGE" [wfinst & conn-args] (apply wfeclient/response-wfinst wfinst conn-args))
(defmethod update-wfinst "rem-piped-shell" [wfinst & conn-args] (apply wfeclient/response-wfinst wfinst conn-args))

(defn update-wfinst-and-set-everywhere
  "a convenience function that updates the WFInstance object and records the new info (workflow, its jobs' task statuses) where necessary.  use this instead of update-wfinst if possible"
  [wfinst & conn-args]
  (let [updated-wfinst (apply update-wfinst wfinst conn-args)]
    (add-wfinst-to-global-statuses updated-wfinst)
    (wflow/set-workflow (:workflow updated-wfinst))))

;;
;; ref initializations
;;

(def global-job-statuses (ref {}))

(def internal-job-id-counter (atom 0))

(def job-id-translate-map (ref {}))