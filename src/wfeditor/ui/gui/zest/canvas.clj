(ns wfeditor.ui.gui.zest.canvas
  (:require
   [wfeditor.ui.gui.zest.providers :as zproviders]
   [wfeditor.model.workflow :as wflow])
  (:import
   org.eclipse.zest.core.viewers.GraphViewer
   org.eclipse.swt.SWT
   org.eclipse.zest.layouts.LayoutAlgorithm
   org.eclipse.zest.layouts.algorithms.DirectedGraphLayoutAlgorithm
   org.eclipse.zest.layouts.algorithms.TreeLayoutAlgorithm
   org.eclipse.zest.layouts.algorithms.HorizontalShift
   org.eclipse.zest.layouts.algorithms.CompositeLayoutAlgorithm
   org.eclipse.zest.layouts.LayoutStyles
   org.eclipse.swt.layout.GridData))

;; a ref to hold the GraphViewer object that maintains the state of
;; the Zest GEF canvas
(def gv (ref nil))

(add-watch wflow/wf :re-bind (fn [key r old new]
                         (let [jarr-input (into-array (wflow/wf-jobs new))
                               return-viewer-with-new-input-fn (fn [viewer input]
                                          (.setInput viewer input)
                                          ;; since using in alter
                                          ;; statement, have to return viewer
                                          viewer)]
                           (dosync
                            (alter gv return-viewer-with-new-input-fn jarr-input)))))

;;
;; functions
;;

(defn graph-viewer-layout
  "create and return the layout algorithm used for the graph viewer"
  []
  (let [style LayoutStyles/NO_LAYOUT_NODE_RESIZING
        dag-layout ^LayoutAlgorithm (DirectedGraphLayoutAlgorithm. style)
        hshift-layout ^LayoutAlgorithm (HorizontalShift. style)
        tree-layout ^LayoutAlgorithm (TreeLayoutAlgorithm. style)]
    (CompositeLayoutAlgorithm. style (into-array LayoutAlgorithm [tree-layout ]))
    ))

(defn set-graph-jobs
  "give a collection of workflow Job's to be set as the new canvas graph content. subsequently, refresh the graph"
  [jobs]
  (let [jarr-input (into-array jobs)
        return-viewer-with-new-input-fn (fn [viewer input]
                                          (.setInput viewer input)
                                          ;; since using in alter
                                          ;; statement, have to return viewer
                                          viewer)]
    (dosync
     (alter gv return-viewer-with-new-input-fn jarr-input))))

(defn graph-viewer-create
  "create (but don't return?) the Zest GraphViewer object creating the whole Zest canvas"
  [parent]
  (let [viewer (GraphViewer. parent SWT/BORDER)
        content-provider (zproviders/node-content-provider-proxy)
        label-provider (zproviders/label-provider-proxy)
        init-input (:nodes (wflow/dep-graph))
        ;; have to convert the Clojure seq into a Java array to make
        ;; the Java classes of GEF/Zest happy
        jarr-init-input (into-array init-input)        
        layout (graph-viewer-layout)
        parent-grid-data (GridData. (GridData/FILL_BOTH))]
    (.setLayoutData (.getControl viewer) parent-grid-data)
    (doto viewer
      (.setContentProvider content-provider)
      (.setLabelProvider label-provider)
      (.setInput jarr-init-input)
      (.setLayoutAlgorithm layout true)
      (.applyLayout))
    (dosync
     (ref-set gv viewer))))
