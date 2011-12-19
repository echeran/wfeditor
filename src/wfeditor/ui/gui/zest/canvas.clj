(ns wfeditor.ui.gui.zest.canvas
  (:require
   wfeditor.ui.gui.zest.types
   [wfeditor.ui.gui.zest.providers :as zproviders])
  (:import
   [wfeditor.ui.gui.zest.types MyNode MyConnection]
   org.eclipse.zest.core.viewers.GraphViewer
   org.eclipse.swt.SWT
   org.eclipse.zest.layouts.algorithms.TreeLayoutAlgorithm
   org.eclipse.zest.layouts.LayoutStyles
   org.eclipse.swt.layout.GridData))

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

(defn graph-viewer-layout
  "create and return the layout algorithm used for the graph viewer"
  []
  (TreeLayoutAlgorithm. LayoutStyles/NO_LAYOUT_NODE_RESIZING))

(defn graph-viewer-create
  "create (but don't return?) the Zest GraphViewer object creating the whole Zest canvas"
  [parent]
  (let [viewer (GraphViewer. parent SWT/BORDER)
        content-provider (zproviders/node-content-provider-proxy)
        label-provider (zproviders/label-provider-proxy)
        init-input (graph-initial-input)
        layout (graph-viewer-layout)
        parent-grid-data (GridData. (GridData/FILL_BOTH))]
    (.setLayoutData (.getControl viewer) parent-grid-data)
    (doto viewer
      (.setContentProvider content-provider)
      (.setLabelProvider label-provider)
      (.setInput init-input)
      (.setLayoutAlgorithm layout true)
      (.applyLayout))
    ))
