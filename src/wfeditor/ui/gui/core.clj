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
   (org.eclipse.swt.layout FillLayout FormLayout FormData FormAttachment GridLayout GridData RowLayout)
   (org.eclipse.swt.widgets Display Shell Label Button Sash Composite FileDialog Group Text Combo)
   (org.eclipse.swt.events SelectionEvent SelectionAdapter)
   (org.eclipse.draw2d.geometry Rectangle Point)
   ;; (org.eclipse.swt.graphics Rectangle Point) have to comment this
   ;; out since it conflicts with the draw2d import statements
   ;; according to Clojure compiler
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
  (let [exec-group (new-widget Group parent SWT/SHADOW_ETCHED_IN)
        user-label (new-widget Label exec-group SWT/LEFT)
        user-text (new-widget Text exec-group (bit-or SWT/SINGLE SWT/BORDER))
        exec-dom-label (new-widget Label exec-group SWT/LEFT)
        exec-dom-combo (new-widget Combo exec-group (bit-or SWT/DROP_DOWN SWT/READ_ONLY))
        button-group (new-widget Group parent SWT/SHADOW_NONE)
        ;; label1 (new Label button-group SWT/CENTER)
        print-wf-cmd-button (new-widget Button button-group SWT/PUSH)
        print-wf-sge-test-button (new-widget Button button-group SWT/PUSH)
        run-wf-button (new-widget Button button-group SWT/PUSH)
        print-wf-button (new-widget Button button-group SWT/PUSH)
        load-wf-button (new-widget Button button-group SWT/PUSH)
        save-wf-button (new-widget Button button-group SWT/PUSH)
        print-wf-inst-button (new-widget Button button-group SWT/PUSH)
        update-wf-inst-button (new-widget Button button-group SWT/PUSH)
        testing-group (new-widget Group parent SWT/SHADOW_ETCHED_OUT)
        label2 (Label. testing-group  SWT/CENTER)]
    (do
      ;; (.setLayout parent (RowLayout. SWT/VERTICAL))
      (.setLayout parent (GridLayout.))
      )
    (doto exec-group
      (.setText "Execution Properties")
      (.setLayoutData (GridData. GridData/FILL_HORIZONTAL)))
    (let [layout (GridLayout.)]
      (do
        (set! (. layout numColumns) 2)
        (.setLayout exec-group layout)))
    (doto user-label
      (.setText "Enter username:")
      (.setLayoutData (GridData. GridData/HORIZONTAL_ALIGN_BEGINNING)))
    (doto user-text
      (.setLayoutData (GridData. GridData/FILL_HORIZONTAL)))
    (doto exec-dom-label
      (.setText "Select execution domain:")
      (.setLayoutData (GridData. GridData/HORIZONTAL_ALIGN_BEGINNING)))
    (doto exec-dom-combo
      (.add "SGE")
      (.add "shell")
      (.select 0)
      (.setLayoutData (GridData. GridData/FILL_HORIZONTAL)))
    (doto button-group
      (.setLayout (FillLayout. SWT/VERTICAL))
      (.setText "Buttons")
      (.setLayoutData (GridData. GridData/FILL_BOTH)))
    ;; (doto label1
    ;;   (.setText "Working button(s)")
    ;;   (.setBounds (.getClientArea parent)))
    (doto print-wf-cmd-button
      (.setText "Print workflow command")
      (.addSelectionListener (proxy [SelectionAdapter]
                                 []
                               (widgetSelected [event]
                                 (exec/print-wf-command (wflow/workflow))))))
    (doto print-wf-sge-test-button
      (.setText "Print SGE commands test")
      (.addSelectionListener (proxy [SelectionAdapter]
                                 []
                               (widgetSelected [event]
                                 (let [new-wf (wflow/wf-with-internal-ids (wflow/workflow))]
                                   (exec/print-deps-in-order new-wf))))))
    (doto run-wf-button
      (.setText "Run workflow")
      (.addSelectionListener (proxy [SelectionAdapter]
                                 []
                               (widgetSelected [event]                                 
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
    (doto print-wf-inst-button
      (.setText "Print WF instance")
      (.addSelectionListener (proxy [SelectionAdapter]
                                 []
                               (widgetSelected [event]
                                 (let [username (.getText user-text)
                                       workflow (wflow/workflow)
                                       exec-domain (.getItem exec-dom-combo (.getSelectionIndex exec-dom-combo))
                                       wf-inst (wflow/new-wfinstance-fn username exec-domain workflow)
                                       wf-inst-str (fformat/workflow-instance-to-string wf-inst)]
                                   (println wf-inst-str))))))
    (doto update-wf-inst-button
      (.setText "Update WF instance via server")
      (.addSelectionListener (proxy [SelectionAdapter]
                                 []
                               (widgetSelected [event]
                                 (let [username (.getText user-text)
                                       workflow (wflow/workflow)
                                       exec-domain (.getItem exec-dom-combo (.getSelectionIndex exec-dom-combo))
                                       wf-inst (wflow/new-wfinstance-fn username exec-domain workflow)]
                                   (exec/update-wfinst-and-set-everywhere wf-inst))))))
    (doto testing-group
      (.setText "Testing")
      (.setLayout (RowLayout. SWT/VERTICAL))
      (.setLayoutData (GridData. GridData/HORIZONTAL_ALIGN_BEGINNING)))
    (doto label2
      (.setText "Testing/non-working button(s)"))
    (create-widgets-with-names testing-group Button SWT/PUSH ["one" "two" "three"])
    (do
      (create-widgets-with-names testing-group Button SWT/RADIO ["Radio 1" "Radio 2" "Radio 3"])
      (create-widgets-with-names testing-group Button SWT/TOGGLE ["Tog 1" "Tog 2" "Tog 3"])
      (create-widgets-with-names testing-group Button SWT/CHECK [ "Check one" "...two" "...three"]))))

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