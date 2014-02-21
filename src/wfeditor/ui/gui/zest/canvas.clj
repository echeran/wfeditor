(ns wfeditor.ui.gui.zest.canvas
  (:require
   [wfeditor.ui.gui.zest.providers :as zproviders]
   [wfeditor.model.workflow :as wflow]
   [clojure.set :as set]
   [clojure.string :as string]
   [clojure.contrib.math :as math-contrib]
   [wfeditor.ui.state.gui :as gui-state])
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
   org.eclipse.swt.widgets.Display
   (org.eclipse.jface.viewers ISelectionChangedListener StructuredSelection)
   wfeditor.model.workflow.Job))


;;
;; ref declarations (initial bindings below)
;;

(declare gv)

(declare all-wf-swt-colors)

(declare canvas-selection)

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
  "return a Clojure implementation of HorizontalShiftAlgorithm, but also fix it to make it look pretty (use equal-size widths)."
  []
  ;; based off of the source code at
  ;; http://git.eclipse.org/c/gef/org.eclipse.zest.git/tree/org.eclipse.zest.layouts/src/org/eclipse/zest/layouts/algorithms/HorizontalShiftAlgorithm.java
  (let [
        vspacing (atom 32) 
        context (atom nil)
        hspacing (atom 16)
        done-count (atom 0)
        ]
    (proxy [LayoutAlgorithm]
        []
      (applyLayout [clean]
        (when clean 
          (let [entities (.getEntities @context)
                rows (rows-list entities)
                entity-y-comparator-fn (fn [e1 e2]
                                         (let [e1y (.. e1 getLocation y)
                                               e2y (.. e2 getLocation y)]
                                           (< e1y e2y)))
                entity-x-comparator-fn (fn [e1 e2]
                                         (let [e1y (.. e1 getLocation x)
                                               e2y (.. e2 getLocation x)]
                                           (< e1y e2y)))
                row-comparator-fn (fn [x y]
                                    (let [e1 (first x)
                                          e2 (first y)]
                                      (entity-y-comparator-fn e1 e2)))
                layout-entity-name-fn (fn [entity]
                                        (:name (.getData (first (.getItems entity)))))
                entity-x-fn (fn [entity] (. (.getLocation (first (.getItems entity))) x))
                entity-y-fn (fn [entity] (. (.getLocation (first (.getItems entity))) y))
                entity-width-fn (fn [entity] (. (.getSize entity) width))
                entity-comparator (comparator entity-x-comparator-fn)
                row-comparator (comparator row-comparator-fn)
                rows (sort row-comparator rows)
                bounds (.getBounds @context)
                global-widths (map entity-width-fn entities)
                global-widest-e-wdith (when (seq global-widths) (apply max global-widths))]
            (loop [height-so-far 0
                   rs rows]
              (when (seq rs)
                (let [row (first rs)
                      sorted-row (sort entity-comparator row)
                      new-height-so-far (+ height-so-far @vspacing (.. (first sorted-row ) getSize height))]
                  (loop [i 1
                         entities sorted-row]
                    (when (seq entities)
                      (let [entity (first entities)
                            size (.getSize entity)
                            e-width (. size width)
                            midpoint (/ (. bounds width) 2)
                            old-x (entity-x-fn entity)
                            old-y (entity-y-fn entity)
                            ;; adjusting original impl to properly
                            ;; calculate width of each entity as the max
                            ;; width seen in the row. also, proper
                            ;; consideration for number of entities in
                            ;; a row in calculating upper left corner
                            ;; offset is added here
                            new-x (let [left-e (first sorted-row)
                                        right-e (last sorted-row)
                                        left-x (entity-x-fn left-e)
                                        right-x (entity-x-fn right-e)
                                        es-midpoint (/ (+ left-x right-x) 2)
                                        num-e (count sorted-row)
                                        widths (map entity-width-fn sorted-row)
                                        row-widest-e-width (apply max widths)

                                        fixed-e-width row-widest-e-width
                                        fixed-e-width global-widest-e-wdith
                                        
                                        idx (- (count sorted-row) (count entities))
                                        mid-idx (/ (dec (count sorted-row)) 2)
                                        idx-diff (int (math-contrib/abs (- mid-idx idx)))
                                        midpoint-diff (- midpoint es-midpoint)
                                        dilated-x (if (= 1 (count sorted-row)) 
                                                    old-x
                                                    (cond
                                                     (and (even? num-e) (< idx mid-idx)) (- es-midpoint (- (* (inc idx-diff) (+ @hspacing fixed-e-width)) (/ (+ @hspacing fixed-e-width) 2)))
                                                     (and (even? num-e) (> idx mid-idx)) (+ es-midpoint (- (* (inc idx-diff) (+ @hspacing fixed-e-width)) (/ (+ @hspacing fixed-e-width) 2)))
                                                     (and (odd? num-e) (< idx mid-idx)) (- es-midpoint (* idx-diff (+ @hspacing fixed-e-width)))
                                                     (and (odd? num-e) (> idx mid-idx)) (+ es-midpoint (* idx-diff (+ @hspacing fixed-e-width)))
                                                     true es-midpoint))]
                                    (+ dilated-x midpoint-diff))]
                        (.setLocation entity new-x (+ new-height-so-far (/ (. size height) 2)))
                        (recur (inc i) (rest entities)))))
                  (recur new-height-so-far (rest rs))))))
          (swap! done-count inc)))
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
    (CompositeLayoutAlgorithm. style (into-array LayoutAlgorithm [dag-layout hshift-layout-proxy]))
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
  (let [;; viewer (GraphViewer. parent SWT/BORDER)
        viewer (proxy [GraphViewer]
                   [parent SWT/BORDER]
                 (inputChanged [input old-input]
                   (let [pre-selection (. this getSelection)
                         pre-select-elems (if (.isEmpty pre-selection)
                                            #{}
                                            (into #{} (.toList pre-selection)))]
                     ;; info on proxy-super and `this' in a proxy
                     ;; http://kotka.de/blog/2010/03/proxy_gen-class_little_brother.html
                     ;; http://www.gettingclojure.com/articles:extending-java-classes-using-proxy
                     (. (. this getControl) setDynamicLayout false)
                     (proxy-super inputChanged input old-input)
                     (. (. this getControl) setDynamicLayout true)
                     (. (. this getControl) applyLayout)
                     (. this refresh)
                     (let [post-selection (. this getSelection)
                           post-select-elems (if (.isEmpty post-selection)
                                               #{}
                                               (into #{} (.toList post-selection)))
                           new-select-elems (set/union
                                             pre-select-elems
                                             post-select-elems)
                           new-select-elems (into new-select-elems [ (when (= Job (class @gui-state/job-to-edit)) @gui-state/job-to-edit) ])
                           new-selection (if (seq new-select-elems)
                                           (StructuredSelection. (to-array new-select-elems))
                                           (StructuredSelection.))]
                       (. this setSelection new-selection false)))))
        content-provider (zproviders/node-content-provider-proxy)
        label-provider (zproviders/label-provider-proxy)
        init-input (wflow/workflow)
        ;; have to convert the Clojure seq into a Java array to make
        ;; the Java classes of GEF/Zest happy
        layout (graph-viewer-layout)
        parent-grid-data (GridData. (GridData/FILL_BOTH))
        sel-chgd-listener (proxy [ISelectionChangedListener]
                              []
                            (selectionChanged [event]
                              (let [source (.getSource event)
                                    selection ^StructuredSelection (.getSelection event)]
                                (dosync
                                 (ref-set canvas-selection selection)))))]
    (.setLayoutData (.getControl viewer) parent-grid-data)
    (doto viewer
      (.setContentProvider content-provider)
      (.setLabelProvider label-provider)
      (.setInput init-input)
      (.addSelectionChangedListener sel-chgd-listener)
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
;; canvas selection functions
;;

(defn selected-jobs
  "get a sequential of the jobs that are currently selected in the canvas, if any"
  []
  (let [sel @canvas-selection]
    (when sel
      (let [sel-iter (.iterator sel)
            sel-seq (iterator-seq sel-iter)
            job-pred (fn [elem] (= (class elem) wfeditor.model.workflow.Job))]
        (filter job-pred sel-seq)))))

(defn selected-deps
  "get a sequential of the jobs that are currently selected in the canvas, if any"
  []
  (let [sel @canvas-selection]
    (when sel
      (let [sel-iter (.iterator sel)
            sel-seq (iterator-seq sel-iter)
            dep-pred (fn [elem] (= (class elem) org.eclipse.zest.core.viewers.EntityConnectionData))]
        (for [dep (filter dep-pred sel-seq)]
          {:source (. dep source)
           :dest (. dep dest)})))))

;;
;; ref initial bindings & add-watch forms
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


;; a ref to hold a ^StruturedSelection object whenever the selection
;; changes in the canvas
(def canvas-selection (ref nil))

;; watch for the user selection in canvas, and update the job editor
;; when only 1 job selected
(add-watch canvas-selection :re-bind (fn [key r old new]
                                       (let [selection ^StructuredSelection new
                                             selected-widgets (.toList selection)
                                             job-to-edit (if (and
                                                              (not (.isEmpty selection))
                                                              (= 1 (count (.toList selection)))
                                                              (= (class (.getFirstElement selection)) wfeditor.model.workflow.Job))
                                                           (.getFirstElement selection)
                                                           nil)]
                                         (dosync
                                          (ref-set gui-state/job-to-edit job-to-edit)))))