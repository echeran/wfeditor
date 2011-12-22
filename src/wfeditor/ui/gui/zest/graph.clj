(ns wfeditor.ui.gui.zest.graph
  (:use clojure.contrib.graph))


;;
;; records
;;

;; refactored the defrecord types to this class so that there wouldn't
;; be this catch 22 cyclic dependency for the compiler that happens (like in C++).
;; Unlike C++, I'm not sure if a forward declarataion would work.

;; unique identifier for MyNode = id
(defrecord MyNode [id name])

;; unique identifier(s) for MyConnection = [src dest]
(defrecord MyConnection [src dest label])

;;
;; refs
;;

(def nodes (ref nil))

(def connections (ref nil))

;;
;; functions
;;

(defn get-node-by-id
  "returns first node containing the provided id"
  ([id]
     (get-node-by-id @nodes id))
  ([nodes id]
      (when (seq nodes)
        (first (filter (fn [node] (= id (:id node))) nodes)))))

(defn new-mynode-fn
  "return a new node record / object"
  [[id name]]
  (MyNode. id name))

(defn new-connection-fn
  "return a new connection record / object"
  ([[src dest label]]
     (MyConnection. src dest label)))

(defn connections-map
  "return a map indicating which nodes are connected to which other nodes"
  ([]
     (connections-map @connections))
  ([connections]
     (let [to-from-pair-maps (for [c connections] {(:src c) [(:dest c)]})
           merged-maps (reduce (partial merge-with #(into %1 %2)) to-from-pair-maps)]
       merged-maps)))

(defn graph
  "return the current state of the graph as a vector of @nodes and @connections, in that order"
  []
  [@nodes @connections])

(defn connected-to
  "return the node objects that are connected to the provided node ojb  based on the current state of the graph"
  [node]
  (let [node-id (:id node)
        [nodes conns] (graph)
        conn-map (connections-map conns)
        conn-node-ids (get conn-map node-id)
        conn-node-ids-seq (seq conn-node-ids)
        conn-nodes (for [node-id conn-node-ids-seq] (get-node-by-id nodes node-id))]
    conn-nodes))

(defn- initial-nodes
  "return a sequence of the initial nodes (sans connected-to info)"
  []
  (let [init-nodes (map new-mynode-fn [[0 "Hamburg"] [1 "Frankfurt"] [2 "Berlin"] [3 "Munich"] [4 "Eppelheim"] [5 "Ahrensboek"]])]
    init-nodes))

(defn- initial-connections
  "return a sequence of the initial connections"
  []
  (let [init-cnxns (map new-connection-fn [[0 1 "0"] [0 4 "1"] [2 1 "2"] [1 3 "3"]])]
    init-cnxns))

(defn set-init-graph
  "set the initial refs containing the nodes and connections"
  []
  (let [init-nodes (initial-nodes)
        init-cnxns (initial-connections)]
    (dosync
     (ref-set nodes init-nodes)
     (ref-set connections init-cnxns))))

(defn- node-adj
  "return a map that (due to Clojure rules for maps) serves as a function returning which nodes are adjacent to the input node.  the input is an adjacency map of ids to lists of ids"
  [node-adj-map]
  (for [[key vals] node-adj-map]
    {(get-node-by-id key) [(for [v vals] (get-node-by-id v))]}))

;;
;; initializing routines (setting state)
;;

(set-init-graph)

(defn- init-clj-graph
  "create the initial value of the graph struct object (as used by clojure.contrib.graph) for the graph"
  []
  (let [nodes (into #{} (initial-nodes))
        id-adj-map {0 [1 4], 1 3, 2 1}
        adj-map (node-adj id-adj-map)]
    (struct directed-graph nodes adj-map)))

(def g (ref (init-clj-graph)))