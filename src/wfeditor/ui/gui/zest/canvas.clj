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


;;
;; ref declarations (initial bindings below)
;;

(declare gv)

(declare all-wf-swt-colors)

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

(defn set-graph-from-wf
  "give a collection of workflow Job's to be set as the new canvas graph content. subsequently, refresh the graph"
  [wf]
  (let [return-viewer-with-new-input-fn (fn [viewer input]
                                          (.setInput viewer input)
                                          ;; since using in alter
                                          ;; statement, have to return viewer
                                          viewer)]
    (dosync
     (zproviders/dispose-job-swt-colors)
     (alter gv return-viewer-with-new-input-fn wf))))

(defn graph-viewer-create
  "create (but don't return?) the Zest GraphViewer object creating the whole Zest canvas"
  [parent]
  (let [viewer (GraphViewer. parent SWT/BORDER)
        content-provider (zproviders/node-content-provider-proxy)
        label-provider (zproviders/label-provider-proxy)
        init-input (wflow/workflow)
        ;; have to convert the Clojure seq into a Java array to make
        ;; the Java classes of GEF/Zest happy
        layout (graph-viewer-layout)
        parent-grid-data (GridData. (GridData/FILL_BOTH))]
    (.setLayoutData (.getControl viewer) parent-grid-data)
    (doto viewer
      (.setContentProvider content-provider)
      (.setLabelProvider label-provider)
      (.setInput init-input)
      (.setLayoutAlgorithm layout true)
      (.applyLayout))
    (dosync
     (ref-set gv viewer))))

;;
;; SWT util functions
;;

(defn dispose-all-wf-swt-colors
  "dispose the SWT Color objects created across all WF's and the current WF"
  []
  (dosync
   (doseq [wf-map (vals @all-wf-swt-colors)
           job-map (vals wf-map)
           color (vals job-map)]
     (.dispose color))
   (ref-set all-wf-swt-colors {})
   (zproviders/dispose-job-swt-colors)))

;;
;; ref initial bindings
;;

;; a ref to hold the GraphViewer object that maintains the state of
;; the Zest GEF canvas
(def gv (ref nil))

;; repainting of the canvas happens automatically whenever the
;; workflow object changes value, using the add-watch mechanism. we
;; have to trust that following this add-watch, the graphviewer gets
;; instantiated before the workflow changes
(add-watch wflow/wf :re-bind (fn [key r old new]
                               (set-graph-from-wf new)))

;; the ref's value is a multi-level map:
;; key 1: a Workflow object (record)
;; values returned by key 1 will be entries in wfeditor.ui.gui.zest.providers/job-swt-colors
(def all-wf-swt-colors (ref {}))