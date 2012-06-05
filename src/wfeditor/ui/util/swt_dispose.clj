(ns wfeditor.ui.util.swt-dispose
  (:require
   [wfeditor.ui.gui.zest.canvas :as zcanvas])
  ;; (:import
  ;;  org.eclipse.swt.SWT
  ;;  (org.eclipse.swt.events SelectionEvent SelectionAdapter)
  ;;  (org.eclipse.swt.graphics Color RGB)
  ;;  org.eclipse.swt.widgets.Display)
  )

(defn dispose-all
  "dipose of all of the SWT disposable (i.e., Resource?) objects in the program by calling all of the functions that dispose of those objects"
  []
  (zcanvas/dispose-all-wf-swt-colors))