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
;; for a listing of required and optional arguments, see new-job-fn
(defrecord Job [id name desc prog-name prog-ver prog-exec-loc prog-exec-ver prog-args prog-opts std-out-file std-err-file deps])

;; unique identifier(s) for Dependency = [src dest]
;; Dependency is defined as: dest _depends on_ src
;; Dependency more represents the directed flow of information as far
;; as direction (src, dest) is concerned, but the name "Dependency"
;; conveys the follow-up work necessary better than the name "Flow" would
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
      (when (seq jobs)
        (some #(when (= id (:id %)) %) jobs))))

(defn get-job-by-field
  "returns first node containing the provided field and value, where field is given a keyword"
  ([field val]
     (get-job-by-id (:nodes @g) field val))
  ([jobs field val]
      (when (seq jobs)
        (some #(when (= val (field %)) %) jobs))))

(defn new-job-fn
  "return a new job record / object.
when supplying arguments to the function, the following are required
name, prog-exec-loc, prog-args prog-opts
the following are optional:
id desc prog-name prog-ver prog-exec-ver std-out-file std-err-file deps"
  [name prog-exec-loc prog-args prog-opts & {:keys [id desc prog-name prog-ver prog-exec-ver std-out-file std-err-file deps] :or {id nil desc "" prog-name "" prog-ver "" prog-exec-ver "" std-out-file nil std-err-file nil deps []}}]
  (Job. id name desc prog-name prog-ver prog-exec-loc prog-exec-ver prog-args prog-opts std-out-file std-err-file deps))

(defn new-dep-fn
  "return a new dependency record / object"
  ([[src dest label]]
     (Dependency. src dest label)))

(defn graph
  "return the current state of the workflow graph, i.e. job dependency graph, i.e. Job objs and Dependency objs"
  []
  @g)

(defn depends-upon
  "return the obects that this job depends upon based on the state of the graph"
  [job]
  ;; TODO: make the dependency relation go in the opposite direction
  ;; for all other relevant functions in this namespace
  (get (:neighbors @g) job))

(defn dependent-upon
  "return the job objects that are dependent upon the provided job based on the current state of the graph"
  [job]
  (get (:neighbors @g) job))

(defn set-dependent-upon
  "set the map indicating which jobs are dependent on which jobs"
  [job-dep-map]
  (dosync (alter g assoc :neighbors job-dep-map)))

(defn- initial-jobs
  "return a sequence of the initial jobs (sans dependent-upon info)"
  []
  (let [init-jobs (map #(eval (cons 'new-job-fn %))
                       [["dir-contents" "ls" ["~echeran"] {"-l" nil}]
                        ["filter-size" "awk" ["'{if (NF > 4) {print $5;}}'"] {}]
                        ["build-sum-commands" "awk" ["'{print \"a = a + \" $1} END {print \"a\";}'"] {}]
                        ["compute-sum" "bc" [] {}]] )]
    init-jobs))

(defn job-dep-map
  "return a map that (due to Clojure rules for maps) serves as a function returning which jobs are dependent upon the input job / key.  the input is an adjacency list implemented as a map of names to lists of names"
  ([id-dep-map]
     (job-dep-map (:nodes @g) id-dep-map))
  ([jobs id-dep-map]
     (into {}
           (for [[key vals] id-dep-map]
             [(get-job-by-field jobs :name key) (for [v vals] (get-job-by-field jobs :name v))]))))

(defn- init-clj-graph
  "create the initial value of the graph struct object (as used by clojure.contrib.graph) for the graph. Note: the second value of the struct is a function that returns dependent jobs given a job as input.  I'm following the example of the provided test code and using a map since in Clojure, maps are functions of their keys"
  []
  (let [jobs (into #{} (initial-jobs))
        id-dep-map {"dir-contents" ["filter-size"] "filter-size" ["build-sum-commands"] "build-sum-commands" ["compute-sum"]}
        dep-map (job-dep-map jobs id-dep-map)]
    (Graph. jobs dep-map)))

(defn wf-job-seq
  "return a sequence of jobs in the workflow graph in a topological order, where if job B depends on job A, then B will follow A in the returned sequence"
  [wf]
  (let [dep-graph (contrib-graph/reverse-graph wf)
        dep-levels (contrib-graph/dependency-list dep-graph)]
    (for [level dep-levels job level] job)))



;;
;; refs - binding initial values
;;


(def g (ref (init-clj-graph)))
