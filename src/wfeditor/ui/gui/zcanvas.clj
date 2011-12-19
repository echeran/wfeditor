(ns wfeditor.ui.gui.zcanvas
  (:import
   org.eclipse.zest.core.viewers.GraphViewer
   org.eclipse.swt.SWT
   org.eclipse.zest.layouts.algorithms.TreeLayoutAlgorithm
   org.eclipse.zest.layouts.LayoutStyles
      org.eclipse.swt.layout.GridData))


(defn graph-viewer-layout
  "create and return the layout algorithm used for the graph viewer"
  []
  (TreeLayoutAlgorithm. LayoutStyles/NO_LAYOUT_NODE_RESIZING))

(defn graph-viewer-create
  "create (but don't return?) the Zest GraphViewer object creating the whole Zest canvas"
  [parent]
  (let [viewer (GraphViewer. parent SWT/BORDER)
        layout (graph-viewer-layout)
        parent-grid-data (GridData. (GridData/FILL_BOTH))]
    (.setLayoutData (.getControl viewer) parent-grid-data)
    (doto viewer
      (.setLayoutAlgorithm layout true)
      (.applyLayout))
    ))