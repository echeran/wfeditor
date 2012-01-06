(ns wfeditor.model.execution
  (:use [clojure.contrib.graph :as contrib-graph]
        wfeditor.model.workflow
        popen)
  (:require [clojure.string :as string])
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
       (for [val vals]
         (string/join opt-val-sep [opt val]))))))

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
  (let [dep-graph (contrib-graph/reverse-graph wf)
        dep-levels (contrib-graph/dependency-list dep-graph)
        wf-comm (string/join job-comm-sep (map (comp job-command first) dep-levels))]
    wf-comm))

(defn print-still-running
  "run a function for the provided number of times in a row (reps), where the function prints whether the provided popen process (proc) is still running"
  [proc reps]
  (let [is-running-fn (fn [] (println "wf command(s) still running?=" (running? proc)) (Thread/sleep 500))]
    (dotimes [_ reps]
      (.start
       (Thread. is-running-fn)))))

(defn run-workflow
  "run a workflow of jobs.  supplied as type Graph
Note: currently assumes only one path through the dependency graph
TODO: figure out how to enable multiple brances in the depenedency graph"
  [wf]
  (let [wf-comm (wf-command wf)]
    (println "wf-command=" wf-comm)
    (let [proc (popen ["/bin/sh" "-c" wf-comm])
          output (slurp (stdout proc))]
      (println "output of wf command(s)=" output))))