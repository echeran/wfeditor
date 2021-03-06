(ns wfeditor.model.workflow
  (:use [clojure.contrib.graph :as contrib-graph]))


;;
;; refs (declarations here, initial bindings below)
;;

;; note: an add-watch statement was attached to this ref in wfeditor.ui.gui.zest.canvas
(declare wf)


;;
;; records
;;

;; for a listing of required and optional arguments, see new-job-fn
;; the deps of a job is implicitly stored in the neighbors map of the
;; Graph object that contains this Job (along with other Jobs)
;; if an option flag takes multiple values, the flag will appear in
;; the XML once per value. if a flag takes no values, then in the
;; prog-opts map, its value can either be an empty vector or nil, it
;; seems at the moment
;; int: id
;; vector: prog-args
;; map: prog-opts (string->vector of strings), task-statuses
;; (int->keyword), array (keys include: start, end, step, index-var; the keys and vals are not explicity named in
;; the XML format as with the other fields of Job)
(defrecord Job [name prog-exec-loc prog-args prog-opts id task-statuses prog-name desc std-out-file std-err-file array prog-ver prog-exec-ver sched-opts])

;; replacement for the defstruct declaration of graphs in
;; clojure.contrib.graph
;; the deps of a Job (ex: deps element in XML representation) pulls
;; info from the Graph's neighbors map upon request
;; set: nodes
;; map: neighbors (Job->vector of Jobs)
(defrecord Graph [nodes neighbors])

;; a type encapsulating everything of interest to a workflow.  this is
;; currently the graph (encapsulates the jobs and job dependencies),
;; and all of the meta-info
(defrecord Workflow [graph wf-name wf-ver wf-format-ver parent-ver parent-file parent-hash exec-domain sched-opts])

;; a type representing the execution of a workflow on a server (e.g., on a cluster
;; using a Grid Engine).
;; keyword: exec-domain
(defrecord WFInstance [username exec-domain workflow])


;;
;; record type utility functions
;;

(defn new-job-fn
  "return a new job record / object.
when supplying arguments to the function, the following are required
name, prog-exec-loc, prog-args prog-opts
the following are optional:
id desc prog-name prog-ver prog-exec-ver std-out-file std-err-file deps"
  [name prog-exec-loc prog-args prog-opts & {:keys [id desc prog-name prog-ver prog-exec-ver std-out-file std-err-file task-statuses array sched-opts] :or {id nil desc nil prog-name nil prog-ver nil prog-exec-ver nil std-out-file nil std-err-file nil task-statuses nil array nil sched-opts nil}}]
  (Job. name prog-exec-loc prog-args prog-opts id task-statuses prog-name desc std-out-file std-err-file array prog-ver prog-exec-ver sched-opts))

(defn new-graph-fn
  "return a new graph type"
  [& {:keys [nodes neighbors] :or {nodes #{} neighbors {}}}]
  (Graph. nodes neighbors))

(defn new-workflow-fn
  "return a new workflow type"
  [& {:keys [graph wf-name wf-ver wf-format-ver parent-ver parent-file parent-hash exec-domain sched-opts] :or {graph (new-graph-fn) wf-name nil wf-ver nil wf-format-ver nil parent-ver nil parent-file nil parent-hash nil exec-domain nil sched-opts nil}}]
  (Workflow. graph wf-name wf-ver wf-format-ver parent-ver parent-file parent-hash exec-domain sched-opts))

(defn new-wfinstance-fn
  "return a new workflow instance type"
  [username exec-domain workflow]
  (WFInstance. username exec-domain workflow))


;;
;; job utility functions
;;

(defn nil-job-fn
  "creates a job with nil'ed required fields"
  []
  (new-job-fn nil nil nil nil :array {:start nil :end nil :step nil :index-var nil}))

;;
;; workflow utility functions
;;

(defn workflow
  "return the current state of the workflow object"
  []
  @wf)

(defn set-workflow
  "set the current state of the workflow.  also, update the canvas graph accordingly"
  [new-wf]
  (dosync
   (ref-set wf new-wf)))

(defn dep-graph
  "return the current state of the workflow graph, i.e. job dependency graph, i.e. Job objs as nodes and dependencies represented as the function"
  ([]
     (dep-graph @wf))
  ([wf]
     (:graph wf)))

(defn flow-graph
  "return the reverse of the workflow graph, i.e., representing the 'flow' of data between jobs instead of dependencies between jobs.
note: this is the reverse of the dep-graph"
  ([]
     (flow-graph @wf))
  ([wf]
     (contrib-graph/reverse-graph (dep-graph wf))))

(defn wf-jobs
  "return the set of jobs in the workflow"
  ([]
     (wf-jobs @wf))
  ([wf]
     (:nodes (dep-graph wf))))

(defn dep-levels
  "return the dependency list of a graph as given by clojure-contrib's graph, i.e., a vector of sets of nodes that are independent of all nodes that follow in the vector"
  ([]
     (dep-levels (dep-graph @wf)))
  ([graph]
     (contrib-graph/dependency-list graph)))

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

(defn replace-job
  "return the input workflow where an existing job is replaced by a new job. updates deps accordingly"
  [wf job new-job]
  (let [dep-graph (dep-graph wf)
        job-set (:nodes dep-graph)
        new-job-set (conj (disj job-set job) new-job)
        neighbors-map (:neighbors dep-graph)
        new-neighbors-map (into {} (for [[j deps] neighbors-map]
                                     (let [new-deps (replace {job new-job} deps)
                                           j2 (or ({job new-job} j) j)]
                                       [j2 new-deps])))
        new-dep-graph (-> dep-graph
                          (assoc :nodes new-job-set)
                          (assoc :neighbors new-neighbors-map))
        new-wf (assoc wf :graph new-dep-graph)]
    new-wf))

(defn add-job
  "return a new wf by adding a new job to the old wf"
  [wf job]
  (let [dep-graph (dep-graph wf)
        job-set (:nodes dep-graph)
        new-job-set (conj job-set job)
        new-dep-graph (assoc dep-graph :nodes new-job-set)
        new-wf (assoc wf :graph new-dep-graph)]
    new-wf))

(defn delete-job
  "return a new wf by deleting a job from the old wf"
  [wf job]
  (let [dep-graph (dep-graph wf)
        job-set (:nodes dep-graph)]
    (if (job-set job)
      (let [new-job-set (disj job-set job)
            neighbors-map (:neighbors dep-graph)
            new-neighbors-map (into {} (for [[j deps] neighbors-map]
                                         (when (not= j job)
                                           (let [new-deps (remove #{job} deps)]
                                             [j new-deps]))))
            new-dep-graph (-> dep-graph
                              (assoc :nodes new-job-set)
                              (assoc :neighbors new-neighbors-map))
            new-wf (assoc wf :graph new-dep-graph)]
        new-wf)
      wf)))

(defn add-dep
  "return a new wf by adding a dep to the old wf
source and dest are named according to the default graph = dependency graph (not flow graph), so source depends on dest"
  [wf source-job dest-job]
  (let [dep-graph (dep-graph wf)
        job-set (:nodes dep-graph)]
    (if (and (get job-set source-job)
             (get job-set dest-job))
      (let [neighbors-map (:neighbors dep-graph)
            new-neighbors-map (into {} (for [[j deps] neighbors-map]
                                         (if (= j source-job)
                                           (let [new-deps (if (some #{dest-job} deps)
                                                            deps
                                                            (conj deps dest-job))]
                                             [j new-deps])
                                           [j deps])))
            new-dep-graph (assoc dep-graph :neighbors new-neighbors-map)
            new-wf (assoc wf :graph new-dep-graph)]
        new-wf) 
      wf)))

(defn delete-dep
  "return a new wf by deleting a dep from the old wf.
source and dest are named according to the default graph = dependency graph (not flow graph), so source depends on dest"
  [wf source-job dest-job]
  (let [dep-graph (dep-graph wf)
        job-set (:nodes dep-graph)]
    (if (and (get job-set source-job)
             (get job-set dest-job))
      (let [neighbors-map (:neighbors dep-graph)
            new-neighbors-map (into {} (for [[j deps] neighbors-map]
                                         (if (= j source-job)
                                           (let [new-deps (remove #{dest-job} deps)]
                                             [j new-deps])
                                           [j deps])))
            new-dep-graph (assoc dep-graph :neighbors new-neighbors-map)
            new-wf (assoc wf :graph new-dep-graph)]
        new-wf)
      wf)))

(defn depends-upon
  "return the obects that this job depends upon based on the state of the graph"
  ([job]
     (get (:neighbors (dep-graph)) job))
  ([wf job]
     (get (:neighbors (dep-graph wf)) job))
  )

(defn dependent-upon
  "return the job objects that are dependent upon the provided job based on the current state of the graph"
  ([job]
     (get (:neighbors (flow-graph)) job))
  ([wf job]
     (get (:neighbors (flow-graph wf)) job)))

(defn set-depends-upon
  "set the map indicating which jobs depend on which jobs"
  [job-dep-map]
  (letfn [(update-fn [wf new-neighbors] (assoc (:graph wf) :neighbors new-neighbors))]
    (dosync
     (alter wf update-fn job-dep-map))))

(defn wf-job-seq
  "return a sequence of jobs in the workflow graph in a topological order, where if job B depends on job A, then B will follow A in the returned sequence"
  [wf]
  (let [dep-levels (dep-levels (dep-graph wf))]
    (for [level dep-levels job level] job)))

(defn wf-with-internal-ids
  "return the workflow with artificially, uniquely assigned ids for all jobs"
  [wf]
  (let [dep-order-job-seq (wf-job-seq wf)
        local-id (atom 0)
        new-job-map (into {} (for [j dep-order-job-seq] [j (assoc j :id (swap! local-id inc))]))
        new-jobs (into #{} (vals new-job-map))
        dep-graph (dep-graph wf)
        dep-graph-neighbors (:neighbors dep-graph)
        new-dep-graph-neighbors (into {} (for [[job deps] dep-graph-neighbors] [(new-job-map job) (map new-job-map deps)]))
        new-dep-graph { :nodes new-jobs :neighbors new-dep-graph-neighbors}]
    (assoc wf :graph new-dep-graph)))

(defn wf-has-cycle
  "returns a boolean indicating whether a workflow has a cycle or not"
  [wf]
  (let [graph (:graph wf)]
    (boolean (seq (contrib-graph/self-recursive-sets graph)))))

;;
;; debugging functions
;;

(defn- debug-job-id-name
  "given a Job, return a vector of the name and id of the job"
  [job]
  [(:name job) (:id job)])

(defn- debug-dep-map-id-names
  "given the neighbors map in a Graph object, return a printable map showing the name and id for each job"
  [dep-map]
  (into {} (for [j (keys dep-map)]
             [(debug-job-id-name j) (map debug-job-id-name (dep-map j))])))

(defn- debug-dep-graph-id-names
  "return a Graph object containing a printable representation of a WF's dep-graph that shows job-name and id for each job"
  [wf]
  (let [dep-graph (dep-graph wf)
        jobs (:nodes dep-graph)
        deps (:neighbors dep-graph)
        pr-jobs (map debug-job-id-name jobs)
        pr-deps (debug-dep-map-id-names deps)]
    (Graph. pr-jobs pr-deps)))

(defn debug-wfinst-status
  "given a WFInstance, return a map associating a job id to the task-statuses array"
  [wfinst]
  (let [wf (:workflow wfinst)
        job-seq (wf-job-seq wf)]
    (into {} (for [j job-seq]
               [(:id j) (:task-statuses j)]))))

;;
;; initialization functions
;;

(defn job-dep-map
  "return a map that (due to Clojure rules for maps) serves as a function returning which jobs are dependent upon the input job / key.  the input is an adjacency list implemented as a map of names to lists of names"
  ([name-dep-map]
     (job-dep-map (wf-jobs) name-dep-map))
  ([jobs name-dep-map]
     (into {}
           (for [[key vals] name-dep-map]
             [(get-job-by-field jobs :name key) (for [v vals] (get-job-by-field jobs :name v))]))))

(defn- init-clj-wf
  "create the initial value of the workflow object"
  [] 
  (new-workflow-fn))


;;
;; refs - binding initial values
;;


(def wf (ref (init-clj-wf)))

