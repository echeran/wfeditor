(ns wfeditor.model.workflow
  (:use [clojure.contrib.graph :as contrib-graph]))


;;
;; refs (declarations here, initial bindings below)
;;

(declare wf)

;;
;; records
;;

;; refactored the defrecord types to this class so that there wouldn't
;; be this catch 22 cyclic dependency for the compiler that happens (like in C++).

;; proposed unique identifier for Job = id
;; for a listing of required and optional arguments, see new-job-fn
;; the deps of a job is implicitly stored in the global Graph object g
(defrecord Job [id name desc prog-name prog-ver prog-exec-loc prog-exec-ver prog-args prog-opts std-out-file std-err-file])

;; replacement for the defstruct declaration of graphs in
;; clojure.contrib.graph
;; the deps field of the Job type pulls info from the Graph obj upon request
(defrecord Graph [nodes neighbors])

;; a type encapsulating everything of interest to a workflow.  this is
;; currently the graph (encapsulates the jobs and job dependencies),
;; and all of the meta-info
(defrecord Workflow [graph wf-name wf-ver wf-format-ver parent-ver parent-file parent-hash])

;;
;; functions
;;

(defn workflow
  "return the current state of the workflow object"
  []
  @wf)

(defn dep-graph
  "return the current state of the workflow graph, i.e. job dependency graph, i.e. Job objs as nodes and dependencies represented as the function"
  []
  (:graph @wf))

(defn flow-graph
  "return the reverse of the workflow graph, i.e., representing the 'flow' of data between jobs instead of dependencies between jobs.
note: this is the reverse of the dep-graph"
  []
  (contrib-graph/reverse-graph (dep-graph)))

(defn wf-jobs
  "return the set of jobs in the workflow"
  []
  (:nodes (dep-graph)))

(defn get-job-by-id
  "returns first node containing the provided id"
  ([id]
     (get-job-by-id (wf-jobs) id))
  ([jobs id]
      (when (seq jobs)
        (some #(when (= id (:id %)) %) jobs))))

(defn get-job-by-field
  "returns first node containing the provided field and value, where field is given a keyword"
  ([field val]
     (get-job-by-id (wf-jobs) field val))
  ([jobs field val]
      (when (seq jobs)
        (some #(when (= val (field %)) %) jobs))))

(defn new-job-fn
  "return a new job record / object.
when supplying arguments to the function, the following are required
name, prog-exec-loc, prog-args prog-opts
the following are optional:
id desc prog-name prog-ver prog-exec-ver std-out-file std-err-file deps"
  [name prog-exec-loc prog-args prog-opts & {:keys [id desc prog-name prog-ver prog-exec-ver std-out-file std-err-file] :or {id nil desc nil prog-name nil prog-ver nil prog-exec-ver nil std-out-file nil std-err-file nil}}]
  (Job. id name desc prog-name prog-ver prog-exec-loc prog-exec-ver prog-args prog-opts std-out-file std-err-file))

(defn new-graph-fn
  "return a new graph type"
  [& {:keys [nodes neighbors] :or {nodes #{} neighbors {}}}]
  (Graph. nodes neighbors))

(defn new-workflow-fn
  "return a new workflow type"
  [& {:keys [graph wf-name wf-ver wf-format-ver parent-ver parent-file parent-hash] :or {graph (new-graph-fn) wf-name nil wf-ver nil wf-format-ver nil parent-ver nil parent-file nil parent-hash nil}}]
  (Workflow. graph wf-name wf-ver wf-format-ver parent-ver parent-file parent-hash))

(defn depends-upon
  "return the obects that this job depends upon based on the state of the graph"
  [job]
  (get (:neighbors (dep-graph)) job))

(defn dependent-upon
  "return the job objects that are dependent upon the provided job based on the current state of the graph"
  [job]
  (get (:neighbors (flow-graph)) job))

(defn set-depends-upon
  "set the map indicating which jobs depend on which jobs"
  [job-dep-map]
  (letfn [(update-fn [wf new-neighbors] (assoc (:graph wf) :neighbors new-neighbors))]
    (dosync
     (alter wf update-fn job-dep-map))))

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
  ([name-dep-map]
     (job-dep-map (wf-jobs) name-dep-map))
  ([jobs name-dep-map]
     (into {}
           (for [[key vals] name-dep-map]
             [(get-job-by-field jobs :name key) (for [v vals] (get-job-by-field jobs :name v))]))))

(defn- init-clj-graph
  "create the initial value of the graph struct object (as used by clojure.contrib.graph) for the graph. Note: the second value of the struct is a function that returns dependent jobs given a job as input.  I'm following the example of the provided test code and using a map since in Clojure, maps are functions of their keys"
  []
  (let [jobs (into #{} (initial-jobs))
        id-dep-map {"compute-sum" ["build-sum-commands"] "build-sum-commands" ["filter-size"] "filter-size" ["dir-contents"]}
        dep-map (job-dep-map jobs id-dep-map)]
    (Graph. jobs dep-map)))

(defn- init-clj-wf
  "create the initial value of the workflow object"
  []
  (let [init-graph (init-clj-graph)]
    (new-workflow-fn :graph init-graph)))

(defn wf-job-seq
  "return a sequence of jobs in the workflow graph in a topological order, where if job B depends on job A, then B will follow A in the returned sequence"
  [wf]
  (let [dep-graph (contrib-graph/reverse-graph wf)
        dep-levels (contrib-graph/dependency-list dep-graph)]
    (for [level dep-levels job level] job)))



;;
;; refs - binding initial values
;;


(def wf (ref (init-clj-wf)))
