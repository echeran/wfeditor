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
   org.eclipse.zest.layouts.algorithms.SpaceTreeLayoutAlgorithm
   org.eclipse.zest.layouts.LayoutStyles
   org.eclipse.swt.layout.GridData
   org.eclipse.swt.widgets.Display))


;;
;; ref declarations (initial bindings below)
;;

(declare gv)

(declare all-wf-swt-colors)

;;
;; functions
;;

(defn rows-list
  "create a rows list of the entities in the proxy horizontalshift algorithm"
  [entities]
  (let [
        delta (atom 10)
        ]
    (loop [es entities
           ;; going to implement rows as a map of vectors instead of a
           ;; vector of vectors (as the original java code would
           ;; suggest) b/c it makes using Clojure FP fn's easier, that
           ;; is, it's easier to use assoc-in on a map within loop than
           ;; it would to use nested vectors which would require a
           ;; zipper to handle easily
           rows {}]
      (if (empty? es)
        (vals rows)
        (let [entity (first es)
              layout-y (.. entity getLocation y)
              add-row-fn (fn [[idx row]]
                           (let [row-entity (first row)
                                 row-y (.. row-entity getLocation y)]
                             (when (and (>= layout-y (- row-y @delta))
                                        (<= layout-y (+ row-y @delta)))
                               idx)))
              add-row-idx (some add-row-fn rows)
              new-rows (if add-row-idx
                         (update-in rows [add-row-idx] conj entity)
                         (assoc rows (count rows) [entity]))]
          (recur (rest es) new-rows))))))

(defn hshift-layout-proxy
  "return a Clojure implementation of HorizontalShiftAlgorithm."
  []
  ;; recreated from source code at
  ;; http://git.eclipse.org/c/gef/org.eclipse.zest.git/tree/org.eclipse.zest.layouts/src/org/eclipse/zest/layouts/algorithms/HorizontalShiftAlgorithm.java
  (let [
        vspacing (atom 16) 
        context (atom nil)
        ]
    (proxy [LayoutAlgorithm]
        []
      (applyLayout [clean]
        (when clean
          (let [entities (.getEntities @context)
                rows (rows-list entities)
                entity-comparator-fn (fn [e1 e2]
                                       (let [e1y (.. e1 getLocation y)
                                             e2y (.. e2 getLocation y)]
                                         (< e1y e2y)))
                row-comparator-fn (fn [x y]
                                    (let [e1 (first x)
                                          e2 (first y)]
                                      (entity-comparator-fn e1 e2)))
                entity-comparator (comparator entity-comparator-fn)
                row-comparator (comparator row-comparator-fn)
                rows (sort row-comparator rows)
                bounds (.getBounds @context)]
            (loop [height-so-far 0
                   rs rows]
              (when (seq rs)
                (let [sorted-row (sort entity-comparator (first rs))
                      new-height-so-far (+ height-so-far @vspacing (.. (first sorted-row ) getSize height))]
                  (loop [i 1
                         width (- (/ (. bounds width) 2) (* 75 (.size sorted-row)))
                         entities sorted-row]
                    (when (seq entities)
                      (let [entity (first entities)
                            size (.getSize entity)
                            new-width (+ width (. size width))]
                        (.setLocation entity (+ width (* 10 i) (/ (. size width) 2)) (+ height-so-far (/ (. size height) 2)))
                        (recur (inc i) new-width (rest entities)))))
                  (recur new-height-so-far (rest rs))))))))
      (setLayoutContext [c]
        (reset! context c)))))

(defn graph-viewer-layout
  "create and return the layout algorithm used for the graph viewer"
  []
  (let [style LayoutStyles/NO_LAYOUT_NODE_RESIZING
        dag-layout ^LayoutAlgorithm (DirectedGraphLayoutAlgorithm. style)
        hshift-layout ^LayoutAlgorithm (HorizontalShift. style)
        tree-layout ^LayoutAlgorithm (TreeLayoutAlgorithm. style)
        space-tree-layout ^LayoutAlgorithm (SpaceTreeLayoutAlgorithm. style)
        hshift-layout-proxy ^LayoutAlgorithm (hshift-layout-proxy)
        ]
    (CompositeLayoutAlgorithm. style (into-array LayoutAlgorithm [tree-layout hshift-layout]))
    ))

(defn set-graph-from-wf
  "give a collection of workflow Job's to be set as the new canvas graph content. subsequently, refresh the graph"
  [wf]
  (let [return-viewer-with-new-input-fn (fn [viewer input]
                                          (.setInput viewer input)
                                          ;; since using in alter
                                          ;; statement, have to return viewer
                                          viewer)]
    ;; on startup, job statuses in client might get initialized from
    ;; disk and kick off a workflow update which subsequently might
    ;; kick off a graph paint operation here. thus, if graphviewer not
    ;; initialized, then we're not ready to paint, so ignore
    (when @gv
      (dosync
       (zproviders/dispose-job-swt-colors)
       (alter gv return-viewer-with-new-input-fn wf)))))

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
                               (.. Display getDefault (asyncExec
                                                       (fn []
                                                         (set-graph-from-wf new))))))

;; the ref's value is a multi-level map:
;; key 1: a Workflow object (record)
;; values returned by key 1 will be entries in wfeditor.ui.gui.zest.providers/job-swt-colors
(def all-wf-swt-colors (ref {}))
