(ns wfeditor.model.workflow
  (:use [clojure.contrib.graph :as contrib-graph]))


;;
;; refs (declarations here, initial bindings below)
;;

(declare g)

;;
;; records
;;

;; refactored the defrecord types to this class so that there wouldn't
;; be this catch 22 cyclic dependency for the compiler that happens (like in C++).
;; Unlike C++, I'm not sure if a forward declarataion would work.

;; unique identifier for Job = id
(defrecord Job [id name])

;; unique identifier(s) for Dependency = [src dest]
(defrecord Dependency [src dest label])

;; replacement for the defstruct declaration of graphs in
;; clojure.contrib.graph
(defrecord Graph [nodes neighbors])


;;
;; functions
;;

(defn get-job-by-id
  "returns first node containing the provided id"
  ([id]
     (get-job-by-id (:nodes @g) id))
  ([jobs id]
     ;; TODO make this much simpler by taking advantage of value equality
     ;; in Clojure datatypes
      (when (seq jobs)
        (first (filter (fn [job] (= id (:id job))) jobs)))))

(defn new-job-fn
  "return a new job record / object"
  [[id name]]
  (Job. id name))

(defn new-dep-fn
  "return a new dependency record / object"
  ([[src dest label]]
     (Dependency. src dest label)))

(defn graph
  "return the current state of the workflow graph, i.e. job dependency graph, i.e. Job objs and Dependency objs"
  []
  @g)

(defn dependent-upon
  "return the job objects that are dependent upon the provided job based on the current state of the graph"
  [job]
  (get (:neighbors @g) job))

(defn- initial-jobs
  "return a sequence of the initial jobs (sans dependent-upon info)"
  []
  (let [init-jobs (map new-job-fn [[:0 "Hamburg"] [:1 "Frankfurt"] [:2 "Berlin"] [:3 "Munich"] [:4 "Eppelheim"] [:5 "Ahrensboek"]])]
    init-jobs))

(defn job-dep-map
  "return a map that (due to Clojure rules for maps) serves as a function returning which jobs are dependent upon the input job / key.  the input is an adjacency list implemented as a map of ids to lists of ids"
  ([id-dep-map]
     (job-dep-map (:nodes @g) id-dep-map))
  ([jobs id-dep-map]
     (into {}
           (for [[key vals] id-dep-map]
             [(get-job-by-id jobs key) (for [v vals] (get-job-by-id jobs v))]))))

(defn- init-clj-graph
  "create the initial value of the graph struct object (as used by clojure.contrib.graph) for the graph. Note: the second value of the struct is a function that returns dependent jobs given a job as input.  I'm following the example of the provided test code and using a map since in Clojure, maps are functions of their keys"
  []
  (let [jobs (into #{} (initial-jobs))
        id-dep-map {:0 [:1 :4], :1 [:3], :2 [:1]}
        dep-map (job-dep-map jobs id-dep-map)]
    (Graph. jobs dep-map)))

;;
;; refs - binding initial values
;;


(def g (ref (init-clj-graph)))
