(ns wfeditor.ui.gui.zest.graph)


;;
;; records
;;

;; refactored the defrecord types to this class so that there wouldn't
;; be this catch 22 cyclic dependency for the compiler that happens (like in C++).
;; Unlike C++, I'm not sure if a forward declarataion would work.

(defrecord MyNode [id name connected-to])

(defrecord MyConnection [id label source destination])

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
  [nodes id]
  (when (seq nodes)
      (first (filter (fn [node] (= id (:id node))) nodes))))

;; (defn nodes-connected-to
;;   "Returns a list of MyNodes where the connected-to info is set for the nodes.
;; This function should be re-done or eliminated, since it is just a port of OOP/imperative code, and just for the purposes of testing the Zest+JFace/SWT graphics in the program"
;;   [orig-nodes orig-connections]
;;   (loop [nodes orig-nodes
;;          cnxns orig-connections]
;;     (if (empty? cnxns)
;;       nodes
;;       (let [cnxn (first cnxns)
;;             src-id (:id (:source cnxn))
;;             dest-id (:id (:destination cnxn))
;;             src-node (get-node-by-id nodes src-id)
;;             dest-node (get-node-by-id nodes dest-id)
;;             connected-nodes (:connected-to src-node)
;;             replace-node (assoc src-node :connected-to (conj connected-nodes dest-node))]
;;         (recur (replace {src-node replace-node} nodes) (rest cnxns))))))

(defn new-mynode-fn [[id name]]
  (MyNode. id name ()))

;; (defn graph-initial-input
;;   "get the initial input for the Zest graphviewer"
;;   []
;;   (let [init-nodes (map new-mynode-fn [["1" "Hamburg"] ["2" "Frankfurt"] ["3" "Berlin"] ["4" "Munich"] ["5" "Eppelheim"] ["6" "Ahrensboek"]])
;;         new-connection-fn (fn [[id label src-id dest-id]]
;;                             (let [src-node (get-node-by-id init-nodes src-id)
;;                                   dest-node (get-node-by-id init-nodes dest-id)]
;;                               (MyConnection. id label src-node dest-node)))
;;         connections (map new-connection-fn [["1" "1" "1" "2"] ["2" "2" "1" "5"] ["3" "3" "3" "2"] ["4" "3" "2" "4"]])
;;         nodes (nodes-connected-to init-nodes connections)]
;;     nodes))

(defn new-connection-fn
  "return a sequence of the initial connections"
  ([[id label src dest]]
     (MyConnection. id label src dest)))




(defn- initial-nodes
  "return a sequence of the initial nodes (sans connected-to info)"
  []
  (let [init-nodes (map new-mynode-fn [[0 "Hamburg"] [1 "Frankfurt"] [2 "Berlin"] [3 "Munich"] [4 "Eppelheim"] [5 "Ahrensboek"]])]
    init-nodes))

(defn- initial-connections
  "return a sequence of the initial connections"
  []
  (let [init-cnxns (map new-connection-fn [[0 "0" 0 1] [1 "1" 0 4] [2 "2" 2 1] [3 "3" 1 3]])]
    init-cnxns))

(defn set-init-graph
  "set the initial refs containing the nodes and connections"
  []
  (let [init-nodes (initial-nodes)
        init-cnxns (initial-connections)]
    (dosync
     (ref-set nodes init-nodes)
     (ref-set connections init-cnxns))))

(defn connections-map
  "return a map indicating which nodes are connected to which other nodes"
  [connections]
  (let [cnxns connections
        to-from-pair-maps (for [c cnxns] {(:source c) [(:destination c)]})
        merged-maps (reduce (partial merge-with #(into %1 %2)) to-from-pair-maps)]
    merged-maps))

(defn connected-to
  "return the MyNode's that are connected to the provided MyNode based on the current state of @connections"
  [node]
  (let [cnxns-map (connections-map @connections)
        nodes @nodes
        node-id (:id node)
        conn-node-ids (get cnxns-map node-id)
        conn-node-ids-seq (seq conn-node-ids)
        conn-nodes (for [node-id conn-node-ids-seq] (get-node-by-id nodes node-id))]
    conn-nodes))

(defn graph
  "return the current state of the graph as a vector of @nodes and @connections, in that order"
  []
  [@nodes @connections])

;;
;; initializing routines (setting state)
;;

(set-init-graph)