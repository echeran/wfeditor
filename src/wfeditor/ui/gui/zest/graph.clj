(ns wfeditor.ui.gui.zest.graph
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

;; unique identifier for MyNode = id
(defrecord MyNode [id name])

;; unique identifier(s) for MyConnection = [src dest]
(defrecord MyConnection [src dest label])

;; replacement for the defstruct declaration of graphs in
;; clojure.contrib.graph
(defrecord Graph [nodes neighbors])


;;
;; functions
;;

(defn get-node-by-id
  "returns first node containing the provided id"
  ([id]
     (get-node-by-id (:nodes @g) id))
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

(defn graph
  "return the current state of the graph as a vector of @nodes and @connections, in that order"
  []
  @g)

(defn connected-to
  "return the node objects that are connected to the provided node job based on the current state of the graph"
  [node]
  (get (:neighbors @g) node))

(defn- initial-nodes
  "return a sequence of the initial nodes (sans connected-to info)"
  []
  (let [init-nodes (map new-mynode-fn [[:0 "Hamburg"] [:1 "Frankfurt"] [:2 "Berlin"] [:3 "Munich"] [:4 "Eppelheim"] [:5 "Ahrensboek"]])]
    init-nodes))

(defn node-adj-map
  "return a map that (due to Clojure rules for maps) serves as a function returning which nodes are adjacent to the input node.  the input is an adjacency list implemented as a map of ids to lists of ids"
  ([id-adj-map]
     (node-adj-map (:nodes @g) id-adj-map))
  ([nodes id-adj-map]
     (into {}
           (for [[key vals] id-adj-map]
             [(get-node-by-id nodes key) (for [v vals] (get-node-by-id nodes v))]))))

(defn- init-clj-graph
  "create the initial value of the graph struct object (as used by clojure.contrib.graph) for the graph. Note: the second value of the struct is a function that returns adjacent nodes given a node as input.  I'm following the example of the provided test code and using a map since in Clojure, maps are functions of their keys"
  []
  (let [nodes (into #{} (initial-nodes))
        id-adj-map {:0 [:1 :4], :1 [:3], :2 [:1]}
        adj-map (node-adj-map nodes id-adj-map)]
    (Graph. nodes adj-map)))

;;
;; refs - binding initial values
;;


(def g (ref (init-clj-graph)))
