(ns wfeditor.ui.gui.zest.canvas
  (:require
   [wfeditor.ui.gui.zest.providers :as zproviders]
   [wfeditor.ui.gui.zest.graph :as zgraph])
  (:import
   org.eclipse.zest.core.viewers.GraphViewer
   org.eclipse.swt.SWT
   org.eclipse.zest.layouts.LayoutAlgorithm
   org.eclipse.zest.layouts.algorithms.DirectedGraphLayoutAlgorithm
   org.eclipse.zest.layouts.algorithms.HorizontalShift
   org.eclipse.zest.layouts.algorithms.CompositeLayoutAlgorithm
   org.eclipse.zest.layouts.LayoutStyles
   org.eclipse.swt.layout.GridData))

(defn graph-viewer-layout
  "create and return the layout algorithm used for the graph viewer"
  []
  (let [style LayoutStyles/NO_LAYOUT_NODE_RESIZING
        dag-layout ^LayoutAlgorithm (DirectedGraphLayoutAlgorithm. style)
        hshift-layout ^LayoutAlgorithm (HorizontalShift. style)]
    (CompositeLayoutAlgorithm. style (into-array LayoutAlgorithm [dag-layout hshift-layout]))
    ))

(defn graph-viewer-create
  "create (but don't return?) the Zest GraphViewer object creating the whole Zest canvas"
  [parent]
  (let [viewer (GraphViewer. parent SWT/BORDER)
        content-provider (zproviders/node-content-provider-proxy)
        label-provider (zproviders/label-provider-proxy)
        init-input (first (zgraph/graph))
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
    ))
