(ns wfeditor.io.execution
  (:require [clojure.contrib.graph :as contrib-graph]
        [wfeditor.model.workflow :as wflow]
        [clojure.string :as string]
        [popen :as popen])
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
;; functions
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
    (string/join sep [exec opts-str args-str])))

(defn wf-command
  "return the command(s) necessary to run all of the jobs in the workflow according to the dependencies specified.  this assumes that there is only one path in the dependency graph
TODO: extend this to handle a dependency graph with branches"
  [wf]
  (let [dep-graph (wflow/dep-graph wf)
        dep-levels (contrib-graph/dependency-list dep-graph)
        wf-comm (string/join job-comm-sep (map (comp job-command first) dep-levels))]
    wf-comm))


(defn wf-complex-command-2
  ([wf]
     (let [dep-levels (contrib-graph/dependency-list (wflow/dep-graph wf))
           first-job (first (first dep-levels))]
       (wf-complex-command-2 wf first-job)))
  ([wf root-job]
     (let [flow-graph (wflow/flow-graph wf)
           downstream-jobs ((:neighbors flow-graph) root-job)]
       (condp = (count downstream-jobs)
         0 (job-command root-job)
         1 (str (job-command root-job) job-comm-sep (wf-complex-command-2 wf (first downstream-jobs)))
         (str (job-command root-job) job-comm-sep branch-split-begin (string/join branch-split-sep (map (fn [job] (str branch-split-job-prefix (wf-complex-command-2 wf job) branch-split-job-suffix)) downstream-jobs)) branch-split-end)))))

(defn branch-command
  [wf job]
  (let [dep-graph (wflow/dep-graph wf)
        flow-graph (wflow/flow-graph wf)
        downstream-jobs ((:neighbors flow-graph) job)
        upstream-jobs ((:neighbors dep-graph) job)
        dep-jobs ()]))

(declare wf-complex-command)

(defn wf-complex-subtree-command-impl
  "return the command for either the subtree upstream or downstream of a job"
  [wf job job-set visited-nodes up-down-stream-flag cmd-begin job-prefix job-suffix job-sep cmd-end]
  (when job-set
    (let [unseen-job-set (remove visited-nodes job-set)]
      (condp =  (count unseen-job-set)
        0 nil
        1 (let [unseen-job (first unseen-job-set)]
            (if (= :down up-down-stream-flag)
              (wf-complex-command wf job (conj visited-nodes job unseen-job) (str (job-command job)) )))
        (str cmd-begin
             (string/join job-sep
                          (map #((str job-prefix % job-suffix)) unseen-job-set))
             cmd-end)))))

(defmulti wf-complex-subtree-command (fn [_ _ _ up-down-stream-flag] up-down-stream-flag))
(defmethod wf-complex-subtree-command :up [wf job visited-nodes up-down-stream-flag] (wf-complex-subtree-command-impl wf job ((:neighbors (wflow/dep-graph wf)) job) visited-nodes up-down-stream-flag merge-split-begin merge-split-job-prefix merge-split-job-suffix merge-split-sep merge-split-end))
(defmethod wf-complex-subtree-command :down [wf job visited-nodes up-down-stream-flag] (wf-complex-subtree-command-impl wf job ((:neighbors (wflow/flow-graph wf)) job) visited-nodes up-down-stream-flag branch-split-begin branch-split-job-prefix branch-split-job-suffix branch-split-sep branch-split-end))


(defn wf-complex-command
  "return the command necessary to run all jobs in a more complex graph (has branching and mergine)
for now, assumes that the entire graph is connected"
  ([wf]
     (let [dep-levels (contrib-graph/dependency-list (wflow/dep-graph wf))
           first-job (first (first dep-levels))]
       (wf-complex-command wf first-job #{} "")))
  ([wf job visited-nodes accum-cmd]
     (when job 
       (let [visited-nodes (conj visited-nodes job)
             upstream-cmd (wf-complex-subtree-command wf job visited-nodes :up)
             downstream-cmd (wf-complex-subtree-command wf job visited-nodes :down)
             job-cmd (wf-command job)]
         (string/join job-comm-sep (remove nil? [upstream-cmd job-cmd downstream-cmd]))))))

(defn print-still-running
  "run a function for the provided number of times in a row (reps), where the function prints whether the provided popen process (proc) is still running"
  [proc reps]
  (let [is-running-fn (fn [] (println "wf command(s) still running?=" (popen/running? proc)) (Thread/sleep 500))]
    (dotimes [_ reps]
      (.start
       (Thread. is-running-fn)))))

(defn print-wf-command
  [wf]
  (let [wf-comm (wf-command wf)]
    (println "wf-command=" wf-comm)))

(defn run-workflow
  "run a workflow of jobs.  supplied as type Graph
Note: currently assumes only one path through the dependency graph
TODO: figure out how to enable multiple brances in the depenedency graph"
  [wf]
  (let [wf-comm (wf-command wf)]
    (let [proc (popen/popen ["/bin/sh" "-c" wf-comm])
          output (slurp (popen/stdout proc))]
      (println "output of wf command(s)=" output))))