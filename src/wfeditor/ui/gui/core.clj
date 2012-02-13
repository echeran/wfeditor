(ns wfeditor.ui.gui.core
  (:gen-class)
  ;; putting the import statement in the ns form is the preferred way
  ;; of importing Java classes in Clojure (see
  ;; http://pragprog.com/magazines/2010-11/getting-clojure)
  ;; (:require wfeditor.ui.gui.d2dcanvas)
  (:require wfeditor.ui.gui.zest.canvas
            [wfeditor.model.workflow :as wflow]
            [wfeditor.io.execution :as exec]
            [wfeditor.io.file.wfeformat :as fformat])
  (:import
   org.eclipse.jface.window.ApplicationWindow
   org.eclipse.swt.SWT
   (org.eclipse.swt.layout FillLayout FormLayout FormData FormAttachment GridLayout GridData)
   (org.eclipse.swt.widgets Display Shell Label Button Sash Composite FileDialog)
   (org.eclipse.swt.events SelectionEvent SelectionAdapter)
   (org.eclipse.draw2d.geometry Rectangle Point)
   ;; (org.eclipse.swt.graphics Rectangle Point) have to comment this
   ;; out since it conflicts with the draw2d import statements
   ;; according to Clojure compiler
   ;; TODO: refactor draw2d canvas to a separate namespace
   ))

(defmacro new-widget [widget-class parent style]
  `(do (new ~widget-class ~parent ~style)))

(defmacro create-widgets-with-names [parent widget-class style names]
  `(dorun (map #(.setText (new ~widget-class ~parent ~style) %1) ~names)))

(defn create-buttons-with-names [parent  style names]
  (dorun (map #(.setText (Button. parent style) %1) names)))


;; TODO: move this function to a gui.util namespace
;; TODO: also, create a one-stop, new widget convenience utility to end them all
;; i that namespace
(defn get-ancestor-shell
  "return the ancestor shell of the provided widget / element, or else nil"
  [widget]
  (cond
   (isa? (class widget) org.eclipse.swt.widgets.Shell) widget
   (isa? (class widget) org.eclipse.jface.window.Window) (.getShell widget)
   :else (when-let [parent (.getParent widget)] (get-ancestor-shell parent))))

(defn ui-editor-left-create [parent]
  (let [label1 (new Label parent SWT/CENTER)
        run-wf-button (new-widget Button parent SWT/PUSH)
        print-wf-button (new-widget Button parent SWT/PUSH)
        load-wf-button (new-widget Button parent SWT/PUSH)
        save-wf-button (new-widget Button parent SWT/PUSH)
        label2 (Label. parent  SWT/CENTER)]
    (do
      (.setLayout parent (FillLayout. SWT/VERTICAL)))
    (doto label1
      (.setText "Working button(s)")
      (.setBounds (.getClientArea parent)))
    (doto run-wf-button
      (.setText "Run workflow")
      (.addSelectionListener (proxy [SelectionAdapter]
                                 []
                               (widgetSelected [event]
                                 (exec/print-wf-command (wflow/workflow))
                                 (exec/run-workflow (wflow/workflow))))))
    (doto print-wf-button
      (.setText "Print workflow")
      (.addSelectionListener (proxy [SelectionAdapter]
                                 []
                               (widgetSelected [event]
                                 (println (fformat/workflow-to-string (wflow/workflow)))))))
    (doto load-wf-button
      (.setText "Load workflow")
      (.addSelectionListener (proxy [SelectionAdapter]
                                 []
                               (widgetSelected [event]
                                 (let [fd (new FileDialog (get-ancestor-shell parent) SWT/OPEN)]
                                   (when-let [in-file-name (.open fd)]
                                     (fformat/set-workflow-from-file in-file-name)))))))
    (doto save-wf-button
      (.setText "Save workflow")
      (.addSelectionListener (proxy [SelectionAdapter]
                                 []
                               (widgetSelected [event]
                                 (let [fd (FileDialog. (get-ancestor-shell parent) SWT/SAVE)]
                                   (when-let [out-file-name (.open fd)]
                                     (fformat/save-workflow-to-file (wflow/workflow) out-file-name)))))))
    (doto label2
      (.setText "Testing/non-working button(s)"))
    (create-widgets-with-names parent Button SWT/PUSH ["one" "two" "three"])
    (do
      (.setLayout parent (FillLayout. SWT/VERTICAL))
      (create-widgets-with-names parent Button SWT/RADIO ["Radio 1" "Radio 2" "Radio 3"])
      (create-widgets-with-names parent Button SWT/TOGGLE ["Tog 1" "Tog 2" "Tog 3"])
      (create-widgets-with-names parent Button SWT/CHECK [ "Check one" "...two" "...three"]))))

(defn ui-editor-right-create [parent]
  (let []
    
    ;; (wfeditor.ui.gui.d2dcanvas/create-diagram parent)
    (wfeditor.ui.gui.zest.canvas/graph-viewer-create parent)

    (.setLayout parent (GridLayout.))))

;; assume FormLayout of the parent widget to which the returned sash
;; will be attached
(defn ui-editor-sash [parent]
  (let [sash (Sash. parent SWT/VERTICAL)
        data (FormData.)]
    (do
      (set! (. data top) (FormAttachment. 0 0))
      (set! (. data bottom) (FormAttachment. 100 0))
      (set! (. data left) (FormAttachment. 50 0))
      (.setLayoutData sash data))
    sash))

(defn ui-editor-create [parent]
  (let [
        sash (Sash. parent (bit-or SWT/VERTICAL SWT/BORDER SWT/SMOOTH))
        ;; have to make the style of the elements next to Sash have
        ;; BORDER so that Sash is drawn identifiably
        comp-left (Composite. parent SWT/BORDER)
        comp-right (Composite. parent SWT/BORDER)
        ;; sash (create-sash parent comp-left comp-right)
        sash-fdata (FormData.)
        comp-left-fdata (FormData.)
        comp-right-fdata (FormData.)
        ]
    (do
      (.setLayout parent (FormLayout.))
      (ui-editor-left-create comp-left)
      (ui-editor-right-create comp-right))
    (do
      (set! (. sash-fdata top) (FormAttachment. 0 0))
      (set! (. sash-fdata bottom) (FormAttachment. 100 0))
      (set! (. sash-fdata left) (FormAttachment. 25 0))
      (.setLayoutData sash sash-fdata)
      (set! (. comp-left-fdata top) (FormAttachment. 0 0))
      (set! (. comp-left-fdata bottom) (FormAttachment. 100 0))
      (set! (. comp-left-fdata left) (FormAttachment. 0 0))
      (set! (. comp-left-fdata right) (FormAttachment. sash 0))
      (.setLayoutData comp-left comp-left-fdata)
      (set! (. comp-right-fdata top) (FormAttachment. 0 0))
      (set! (. comp-right-fdata bottom) (FormAttachment. 100 0))
      (set! (. comp-right-fdata left) (FormAttachment. sash 0))
      (set! (. comp-right-fdata right) (FormAttachment. 100 0))
      (.setLayoutData comp-right comp-right-fdata))
    (do
      (.addSelectionListener sash (proxy [SelectionAdapter]
                                      [] ;; do not call the
                                    ;; super-class constructor w/o
                                    ;; reason to, but provide it for
                                    ;; proxy's syntax's sake
                                    (widgetSelected [event]
                                      (set! (. (^FormData . sash getLayoutData) left) (FormAttachment. 0 (. event x)))
                                      (dorun
                                       (.. sash getParent layout))))))))

;; JFace way of creating a window is to subclass ApplicationWindow and
;; override createContents
;; This is the Clojure way of "subclassing", i.e., extending a class
;; via the proxy macro.  This function just returns the instance of
;; the anonymous class generated by proxy

(defn app-win-proxy []
  (proxy [ApplicationWindow]
      [nil]  ;; calling the ApplicationWindow constructor
    ;; note, should not implement any subclass constructor, as
    ;; explained in
    ;; http://pragprog.com/magazines/2010-11/getting-clojure
    ;; Instead, any code that would go in the subclass construtor
    ;; should be applied to the instance of the extended class
    ;; returned by proxy
    (createContents [parent]
      (ui-editor-create parent))
    (getInitialSize []
      ;; override default implementation (documented in Javadoc) with
      ;; one that ensures the window is "maximized"
      ;; note: a display can encompass multiple monitors, so get just
      ;; primary monitor in order not to be a screen-hog on a
      ;; multi-display setup
      ;; I wouldn't be doing this if I could get
      ;; shell.setMaximized(true) to work in Clojure easily, but I
      ;; can't.  I haven't tested doing an all-out gen-class for a
      ;; subclass of ApplicationWindow (instead of this proxy), but
      ;; that would be the next step.  For now, at least, this
      ;; workaround of setMaximized(true) is fine by me.
      (let [shell (.getShell this)
            display (.getDisplay shell)
            prim-mon (.getPrimaryMonitor display)
            client-area (.getClientArea prim-mon)]
        (.computeSize shell (. client-area width) (. client-area height) true)))))