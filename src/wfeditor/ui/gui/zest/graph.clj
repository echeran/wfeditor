(ns wfeditor.ui.gui.zest.graph
  ; (:require wfeditor.ui.gui.zest.types)
  )


;;
;; records
;;

;; refactored the defrecord types to this class so that there wouldn't
;; be this catch 22 cyclic dependency for the compiler that happens (like in C++).
;; Unlike C++, I'm not sure if a forward declarataion would work.

(defrecord MyNode [id name connected-to])

(defrecord MyConnection [id label source destination])


;;
;; functions
;;


(defn get-node-by-id
  "returns first node containing the provided id"
  [nodes id]
  (first (filter (fn [node] (= id (:id node))) nodes)))

(defn nodes-connected-to
  "Returns a list of MyNodes where the connected-to info is set for the nodes.
This function should be re-done or eliminated, since it is just a port of OOP/imperative code, and just for the purposes of testing the Zest+JFace/SWT graphics in the program"
  [orig-nodes orig-connections]
  (loop [nodes orig-nodes
         cnxns orig-connections]
    (if (empty? cnxns)
      nodes
      (let [cnxn (first cnxns)
            src-id (:id (:source cnxn))
            dest-id (:id (:destination cnxn))
            src-node (get-node-by-id nodes src-id)
            dest-node (get-node-by-id nodes dest-id)
            ;; id (:id src-node)
            connected-nodes (:connected-to src-node)
            replace-node (assoc src-node :connected-to (conj connected-nodes dest-node))]
        (recur (replace {src-node replace-node} nodes) (rest cnxns))))))

(defn graph-initial-input
  "get the initial input for the Zest graphviewer"
  []
  (let [new-mynode-fn (fn [[id name]] (MyNode. id name ()))
        init-nodes (map new-mynode-fn [["1" "Hamburg"] ["2" "Frankfurt"] ["3" "Berlin"] ["4" "Munich"] ["5" "Eppelheim"] ["6" "Ahrensboek"]])
        new-connection-fn (fn [[id label src-id dest-id]]
                            (let [src-node (get-node-by-id init-nodes src-id)
                                  dest-node (get-node-by-id init-nodes dest-id)]
                              (MyConnection. id label src-node dest-node)))
        connections (map new-connection-fn [["1" "1" "1" "2"] ["2" "2" "1" "5"] ["3" "3" "3" "2"] ["4" "3" "2" "4"]])
        nodes (nodes-connected-to init-nodes connections)]
    nodes))
