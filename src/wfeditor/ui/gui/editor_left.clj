(ns wfeditor.ui.gui.editor-left
  (:require [wfeditor.io.util.const :as io-const]
            [wfeditor.model.workflow :as wflow]
            [wfeditor.model.util.type :as type-util]
            [wfeditor.io.execution :as exec]
            [wfeditor.io.status.task-run :as task-status]
            [wfeditor.io.file.wfeformat :as fformat]
            [wfeditor.ui.state.gui :as gui-state]
            [clojure.string :as string]
            [clojure.zip :as zip])
  (:use [wfeditor.ui.util.swt :as swt-util])
  (:import
   org.eclipse.swt.SWT
   (org.eclipse.swt.layout FillLayout RowLayout GridLayout GridData FormLayout FormData FormAttachment)
   (org.eclipse.swt.widgets Label Button FileDialog Group Text Combo Composite Display TableColumn Table TableItem)
   (org.eclipse.swt.events SelectionEvent SelectionAdapter ModifyListener ModifyEvent)
   (org.eclipse.jface.viewers TreeViewer ITreeContentProvider ILabelProvider IDoubleClickListener TableViewer IStructuredContentProvider ITableLabelProvider ListViewer)
   java.net.URL
   (org.eclipse.swt.custom CTabFolder CTabItem)))

;;
;; refs (declarations here, initial bindings below)
;;

(declare exec-props)

;;
;; functions
;;

(defn wfinstance
  "create a WFInst object out of the current WF held in state along with the exec props held in the GUI (this method is a stopgap until WF's and WFInst's are held together in state)"
  ([]
     (wfinstance (wflow/workflow)))
  ([wf]
     (let [{:keys [user exec-dom]} @exec-props
           wf-inst (wflow/new-wfinstance-fn user exec-dom wf)]
       wf-inst)))

;;
;; General tab functions
;;

(defn- execution-group
  "create the group in the navpane storing the fields required for executing jobs on the remote server"
  [parent]
  (let [exec-group (new-widget {:keyname :execution-group :widget-class Group :parent parent :styles [SWT/SHADOW_ETCHED_IN] :text "Execution Properties"})
        user-label (new-widget {:keyname :user-label :widget-class Label :parent exec-group :styles [SWT/LEFT] :text "Enter username:"})
        user-text (new-widget {:keyname :user-text :widget-class Text :parent exec-group :styles [SWT/SINGLE SWT/BORDER] :text (:user @exec-props) })
        exec-dom-label (new-widget {:keyname :exec-dom-label :widget-class Label :parent exec-group :styles [SWT/LEFT] :text "Select execution domain:"})
        exec-dom-combo (new-widget {:keyname :exec-dom-combo :widget-class Combo :parent exec-group :styles [SWT/DROP_DOWN SWT/READ_ONLY]})
        rem-host-label (new-widget {:keyname :rem-host-label :widget-class Label :parent exec-group :styles [SWT/LEFT] :text "Remote host:"})
        rem-host-text (new-widget {:keyname :rem-host-text :widget-class Text :parent exec-group :styles [SWT/SINGLE SWT/BORDER] :text (:rem-host @exec-props)})
        rem-port-label (new-widget {:keyname :rem-port-label :widget-class Label :parent exec-group :styles [SWT/LEFT] :text "Remote port:"})
        rem-port-text (new-widget {:keyname :rem-port-text :widget-class Text :parent exec-group :styles [SWT/SINGLE SWT/BORDER] :text (str (:rem-port @exec-props))})
        loc-port-label (new-widget {:keyname :loc-port-label :widget-class Label :parent exec-group :styles [SWT/LEFT] :text "Local port:"})
        loc-port-text (new-widget {:keyname :loc-port-text :widget-class Text :parent exec-group :styles [SWT/SINGLE SWT/BORDER] :text (str (:loc-port @exec-props))})]
    (doto exec-group
      ;; (.setLayoutData (GridData. GridData/FILL_HORIZONTAL))
      )
    (let [layout (GridLayout.)]
      (do
        (set! (. layout numColumns) 2)
        (.setLayout exec-group layout)))
    (doto user-label
      (.setLayoutData (GridData. GridData/HORIZONTAL_ALIGN_BEGINNING)))
    (doto user-text
      (.setLayoutData (GridData. GridData/FILL_HORIZONTAL))
      (.addModifyListener (reify ModifyListener
                            (modifyText [this event]
                              (let [widget (. event widget)
                                    user (.getText widget)]
                                (dosync
                                 (alter exec-props assoc :user user)))))))
    (doto exec-dom-label
      (.setLayoutData (GridData. GridData/HORIZONTAL_ALIGN_BEGINNING)))
    (doto exec-dom-combo
      (.add "SGE")
      (.add "rem-piped-shell")
      (.select 0)
      ;; default selected value should match (:exec-dom @exec-props)
      (.setLayoutData (GridData. GridData/FILL_HORIZONTAL))
      (.addSelectionListener (proxy [SelectionAdapter]
                                 []
                               (widgetSelected [event]
                                 (let [exec-domain (.getItem exec-dom-combo (.getSelectionIndex exec-dom-combo))]
                                   (dosync
                                    (alter exec-props assoc :exec-dom exec-domain)))))))
    (doto rem-host-label
      (.setLayoutData (GridData. GridData/HORIZONTAL_ALIGN_BEGINNING)))
    (doto rem-host-text
      (.setLayoutData (GridData. GridData/FILL_HORIZONTAL))
      (.addModifyListener (reify ModifyListener
                            (modifyText [this event]
                              (let [widget (. event widget)
                                    rem-host (.getText widget)]
                                (dosync
                                 (alter exec-props assoc :rem-host rem-host)))))))
    (doto rem-port-label
      (.setLayoutData (GridData. GridData/HORIZONTAL_ALIGN_BEGINNING)))
    (doto rem-port-text
      (.setLayoutData (GridData. GridData/FILL_HORIZONTAL))
      (.addModifyListener (reify ModifyListener
                            (modifyText [this event]
                              (let [widget (. event widget)]
                                (try
                                  (let [rem-port (Integer/parseInt (.getText widget))]
                                    (dosync
                                     (alter exec-props assoc :rem-port rem-port)))
                                  (catch NumberFormatException e
                                    (.setText widget (str (:rem-port @exec-props))))))))))
    (doto loc-port-label
      (.setLayoutData (GridData. GridData/HORIZONTAL_ALIGN_BEGINNING)))
    (doto loc-port-text
      (.setLayoutData (GridData. GridData/FILL_HORIZONTAL))
      (.addModifyListener (reify ModifyListener
                            (modifyText [this event]
                              (let [widget (. event widget)]
                                (try
                                  (let [loc-port (Integer/parseInt (.getText widget))]
                                    (dosync 
                                     (alter exec-props assoc :loc-port loc-port)))
                                  (catch NumberFormatException e
                                    (.setText widget (str (:loc-port @exec-props))))))))))
    exec-group))

(defn update-job-statuses-from-server
  "get the latest job statuses from the server and update the local client copy accordingly"
  []
  (let [{:keys [user exec-dom rem-host rem-port loc-port]} @exec-props
        loc-host io-const/DEFAULT-LOCAL-HOST
        server-host io-const/DEFAULT-SERVER-HOST-REL-TO-REMOTE]
    (exec/update-statuses-sge exec-dom user rem-host rem-port loc-port loc-host server-host)))

(defmacro export-svg
  "a macro to encapsulate the SVG export code that prevents the import of the dependency libs, which, when imported, instantiate a Display object.  this needs to be prevented to ensure other SWT code works as stated"
  [out-stream]
  ;; page(s) describing Zest to SVG
  ;; export using GMF (+
  ;; dependencies)
  ;; http://standardout.org/2011/12/eclipse-draw2d-to-svg/
  ;; http://standardout.org/2011/12/off-screen-rendering-of-eclipse-zest-graphs/
  ;;
  ;; macro used to do "conditional importing" as described by this
  ;; page http://java.dzone.com/articles/clojure-conditionally which
  ;; uploaded this code snippet https://github.com/jaycfields/expectations/blob/f2a8687/src/clojure/expectations/scenarios.clj#L81-L86
  `(do
     (import ~''org.eclipse.gmf.runtime.draw2d.ui.render.awt.internal.svg.export.GraphicsSVG)
     (import ~''(javax.xml.transform OutputKeys Transformer TransformerFactory))
     (import ~''javax.xml.transform.dom.DOMSource)
     (import ~''javax.xml.transform.stream.StreamResult)
     (let [gv# @wfeditor.ui.gui.zest.canvas/gv
           graph# (.getGraphControl gv#)
           graph-root# (.getRootLayer graph#)
           viewbox# (.. graph-root# ~'getBounds ~'getCopy)
           graphics# (. org.eclipse.gmf.runtime.draw2d.ui.render.awt.internal.svg.export.GraphicsSVG ~'getInstance viewbox#)]
       (try
         (.paint graph-root# graphics#)
         (let [svg-root# (.getRoot graphics#)
               transformer# (.newTransformer (. javax.xml.transform.TransformerFactory ~'newInstance))
               dom-source# (new javax.xml.transform.dom.DOMSource svg-root#)]
           (.setAttributeNS svg-root# nil "view-box" (string/join " " [(. viewbox# ~'x) (. viewbox# ~'y) (. viewbox# ~'width) (. viewbox# ~'height)]))
           (doto transformer#
             (.setOutputProperty javax.xml.transform.OutputKeys/METHOD "xml")
             (.setOutputProperty javax.xml.transform.OutputKeys/ENCODING "UTF-8")
             (.setOutputProperty javax.xml.transform.OutputKeys/INDENT "yes"))
           (let [result# (new javax.xml.transform.stream.StreamResult ~out-stream)
                 source# (new javax.xml.transform.dom.DOMSource svg-root#)]
             (.transform transformer# source# result#)))
         (catch Throwable t# (.printStackTrace t#))
         (finally
          (.dispose graphics#))))))

(defn- button-group
  "create the Group widget containing all of the buttons in the left navpane that do something"
  [parent]
  (let [button-group (new-widget {:keyname :button-group :widget-class Group :parent parent :styles [SWT/SHADOW_NONE] :text "Buttons"})
        bounding-comp (new-widget {:keyname :bounding-comp :widget-class Composite :parent button-group :styles [SWT/NONE]})
        bounding-comp2 (new-widget {:keyname :bounding-comp :widget-class Composite :parent button-group :styles [SWT/NONE]})
        load-wf-button (new-widget {:keyname :load-wf-button :widget-class Button :parent bounding-comp :styles [SWT/PUSH] :text "Load workflow"})
        save-wf-button (new-widget {:keyname :save-wf-button :widget-class Button :parent bounding-comp :styles [SWT/PUSH] :text "Save workflow"})
        run-wf-inst-button (new-widget {:keyname :run-wf-inst-button :widget-class Button :parent bounding-comp :styles [SWT/PUSH] :text "Run WF instance"})
        export-to-svg-button (new-widget {:keyname :export-to-svg-button :widget-class Button :parent bounding-comp :styles [SWT/PUSH] :text "Export to SVG"})
        update-server-statuses-button (new-widget {:keyname :update-server-statuses-button :widget-class Button :parent  bounding-comp2 :styles [SWT/PUSH] :text "Update statuses on server"})
        get-server-statuses-button (new-widget {:keyname :get-server-statuses-button :widget-class Button :parent bounding-comp2 :styles [SWT/PUSH] :text "Get statuses from server"})
        refresh-wf-statuses-button (new-widget {:keyname :refresh-wf-statuses-button :widget-class Button :parent bounding-comp2 :styles [SWT/PUSH] :text "Refresh WF statuses"})]
    (doto button-group
      (.setLayout (RowLayout. SWT/VERTICAL)))
    (do
      (swt-util/stack-full-width bounding-comp {:margin 5} [load-wf-button save-wf-button run-wf-inst-button export-to-svg-button])
      (swt-util/stack-full-width bounding-comp2 {:margin 5} [update-server-statuses-button get-server-statuses-button refresh-wf-statuses-button])
      (.setLayout button-group (RowLayout.)))
    (update-button load-wf-button
                   {:widget-select-fn (fn [event]
                                        (swt-util/file-dialog-open-wf (get-ancestor-shell parent)))})
    (update-button save-wf-button
                   {:widget-select-fn (fn [event]
                                        (swt-util/file-dialog-save-as-wf (get-ancestor-shell parent)))})
    (update-button run-wf-inst-button
                   {:widget-select-fn (fn [event]
                                        (future (let [{:keys [rem-host rem-port loc-port]} @exec-props
                                                      wf-inst (wfinstance)
                                                      loc-host io-const/DEFAULT-LOCAL-HOST
                                                      server-host io-const/DEFAULT-SERVER-HOST-REL-TO-REMOTE]
                                                  (exec/create-wfinst-and-set-everywhere wf-inst rem-host rem-port loc-port loc-host server-host))))})
    (update-button export-to-svg-button
                   {:widget-select-fn (fn [event]
                                        (. (. Display getCurrent) asyncExec
                                           (fn []
                                             (let [fd (new-widget {:widget-class FileDialog :parent (get-ancestor-shell parent) :styles [SWT/SAVE]})]
                                               (when-let [out-file-name (.open fd)]
                                                 (with-open [out-stream (clojure.java.io/output-stream out-file-name)]
                                                   (export-svg out-stream)))))))})
    (update-button update-server-statuses-button
                   {:widget-select-fn (fn [event]
                                        (future (let [{:keys [user exec-dom rem-host rem-port loc-port]} @exec-props
                                                      loc-host io-const/DEFAULT-LOCAL-HOST
                                                      server-host io-const/DEFAULT-SERVER-HOST-REL-TO-REMOTE]
                                                  (exec/force-server-update-statuses-sge exec-dom user rem-host rem-port loc-port loc-host server-host))))})
    (update-button get-server-statuses-button
                   {:widget-select-fn (fn [event]
                                        (future (update-job-statuses-from-server)))})
    (update-button refresh-wf-statuses-button
                   {:widget-select-fn (fn [event]
                                        (let [curr-wfinst (wfinstance)
                                              updated-wfinst (exec/update-wfinst-sge curr-wfinst)
                                              updated-wf (:workflow updated-wfinst)]
                                          (wflow/set-workflow updated-wf)))})    
    button-group))

(defn- predefined-wfs-tree-group
  "create a JFace TreeViewer to represent predefined NGS & other workflows"
  [parent]
  (let [
        ;; pre-wf-tree ["Genetics" ["NGS" [(URL. "http://www.palmyrasoftware.com/wf/genetics/ngs/sample4.xml")]]]
        pre-wf-simple-zip-tree {"Genetics" [{"NGS" [(URL. "http://www.palmyrasoftware.com/wf/genetics/ngs/sample4.xml")
                                                    (URL. "http://www.palmyrasoftware.com/wf/genetics/ngs/sample6.xml")]}]}
        simple-zip-fn (fn [simple-zip-tree] (zip/zipper map? (comp seq second first) (fn [n cs] (let [map (if (seq n) n {n []}) k (first (first map)) vals (second (first map))] (assoc map k (concat vals (seq cs))))) simple-zip-tree))
        ;; JFace thinks that the nil value in the vector created when
        ;; first creating a zipper is another root element, and throws
        ;; an exception when it discovers null arguments in a
        ;; .setInput method
        ;; hence, have enclosed the zipper in a simplistic closure as
        ;; described in Joy of Clojure
        ;; TODO: simplify closure usage with reify (?) and/or
        ;; defrecord, protocol (??)
        
        ;; jface-simple-zip-fn (comp #(assoc-in % [1] {}) simple-zip-fn)
        tree-zip-closure-fn (fn closure-fn [z]
                              {:data z
                               :apply-fn (fn [new-fn]
                                           (closure-fn (new-fn z)))})
        ;; pre-wf-zip (jface-simple-zip-fn pre-wf-simple-zip-tree)
        pre-wf-zip-closure (tree-zip-closure-fn (simple-zip-fn pre-wf-simple-zip-tree))

        tree-content-provider (proxy [ITreeContentProvider]
                                  []
                                ;; the "content" that will be
                                ;; manipulated by the JFace tree
                                ;; viewer will be entirely closures of zippers
                                ;; located at nodes, not the actual
                                ;; node-data themselves
                                (getChildren [zc] 
                                  (let [first-child ((:apply-fn zc) zip/down)
                                        rest-children (loop [rcs []
                                                             czc ((:apply-fn first-child) zip/right)]
                                                        (if-not (:data czc)
                                                          rcs
                                                          (recur (conj rcs czc) ((:apply-fn czc) zip/right))))
                                        result (concat [first-child] rest-children)
                                        array-result (to-array result)]
                                    array-result))
                                (getElements [zc]
                                  (to-array [(tree-zip-closure-fn (simple-zip-fn (:data ((:apply-fn zc) zip/root))))]) )
                                (getParent [zc]
                                  ((:apply-fn zc) zip/up))
                                (hasChildren [zc]
                                  (if (and zc (:data zc) (:data ((:apply-fn zc) zip/node)) (map? (:data ((:apply-fn zc) zip/node))))
                                    true
                                    false))
                                (dispose [])
                                (inputChanged [viewer old-input new-input]))
        tree-label-provider (proxy [ILabelProvider]
                                []
                              ;; the label provider's responsibility
                              ;; is to take the zipper closure "content" and
                              ;; translate that into the real data to
                              ;; be presented in the GUI
                              (addListener [listener])
                              (dispose [])
                              (getImage [zc]
                                nil)
                              (getText [zc]
                                (let [node-subtree (:data ((:apply-fn zc) zip/node))
                                      node (cond
                                            (map? node-subtree) (first (first node-subtree))
                                            (nil? node-subtree) ""
                                            true node-subtree)
                                      result (condp = (class node)
                                               String node
                                               (str node))]
                                  result))
                              (isLabelProperty [zc property]
                                nil)
                              (removeListener [listener]))
        tree-group (new-widget {:keyname :pre-wf-tree-group :widget-class Group :parent parent :styles [SWT/SHADOW_NONE] :text "Predefined Workflows"})
        tree-viewer (TreeViewer. tree-group)

        dbl-click-listener (proxy [IDoubleClickListener]
                               []
                             (doubleClick [event]
                               (let [selection (.getSelection event)
                                     source (.getSource event)
                                     tree-paths (.getPaths selection)
                                     last-path (last tree-paths)
                                     last-segment (.getLastSegment last-path)
                                     elem (-> last-segment :data zip/node)]
                                 (when (= java.net.URL (class elem))
                                   (let [url-str (str elem)
                                         wf (fformat/workflow-from-stream url-str)]
                                     (wflow/set-workflow wf))))))]
    (doto tree-group
      (.setLayout (FillLayout.)))
    (doto tree-viewer
      (.setContentProvider tree-content-provider)
      (.setLabelProvider tree-label-provider)
      (.setInput pre-wf-zip-closure)
      (.addDoubleClickListener dbl-click-listener))
    tree-group))

(defn button-debugging-group
  "create the Group widget containing all of the debugging buttons in the left navpane"
  [parent]
  (let [button-group (new-widget {:keyname :button-debugging-group :widget-class Group :parent parent :styles [SWT/SHADOW_NONE] :text "Debugging Buttons"})
        bounding-comp (new-widget {:keyname :bounding-comp :widget-class Composite :parent button-group :styles [SWT/NONE]})
        print-global-statuses-button (new-widget {:keyname :print-global-statuses-button :widget-class Button :parent bounding-comp :styles [SWT/PUSH] :text "Print locally-saved statuses"})

        print-gui-map-button (new-widget {:keyname :print-gui-map-button :widget-class Button :parent bounding-comp :styles [SWT/PUSH] :text "Print gui-map"})
        print-gui-lookup-map-button (new-widget {:keyname :print-gui-lookup-map-button :widget-class Button :parent bounding-comp :styles [SWT/PUSH] :text "Print gui lookup-map"})]
    (doto button-group
      (.setLayout (RowLayout. SWT/VERTICAL)))
    (do
      (swt-util/stack-full-width bounding-comp {:margin 5} [print-global-statuses-button print-gui-map-button print-gui-lookup-map-button]))
    (update-button print-global-statuses-button
                   {:widget-select-fn (fn [event]
                                        (println (task-status/global-statuses)))})

    (update-button print-gui-map-button
                   {:widget-select-fn (fn [event]
                                        (println @gui-state/gui-map))})
    (update-button print-gui-lookup-map-button
                   {:widget-select-fn (fn [event]
                                        (println (gui-state/lookup-map)))})
    button-group))

(defn button-testing-group
  "create the Group widget containing all of the testing buttons in the left navpane that aren't (currently) directly useful for program execution"
  [parent]
  (let [button-group (new-widget {:keyname :button-testing-group :widget-class Group :parent parent :styles [SWT/SHADOW_NONE] :text "Testing Buttons"})
        print-wf-cmd-button (new-widget {:keyname :print-wf-cmd-button :widget-class Button :parent button-group :styles [SWT/PUSH] :text "Print workflow command"})
        print-wf-sge-test-button (new-widget {:keyname :print-wf-sge-test-button :widget-class Button :parent button-group :styles [SWT/PUSH] :text "Print SGE commands test"})
        run-wf-button (new-widget {:keyname :run-wf-button :widget-class Button :parent button-group :styles [SWT/PUSH] :text "Run workflow"})
        print-wf-button (new-widget {:keyname :print-wf-button :widget-class Button :parent button-group :styles [SWT/PUSH] :text "Print workflow"})
        print-wf-inst-button (new-widget {:keyname :print-wf-inst-button :widget-class Button :parent button-group :styles [SWT/PUSH] :text "Print WF instance"})]
    (doto button-group
      (.setLayout (RowLayout. SWT/VERTICAL))
      ;; (.setLayoutData (GridData. GridData/FILL_BOTH))
      )
    (update-button print-wf-cmd-button
                   {:widget-select-fn (fn [event]
                                        (exec/print-wf-command (wflow/workflow)))})
    
    (update-button print-wf-sge-test-button
                   {:widget-select-fn (fn [event]
                                        (let [new-wf (wflow/wf-with-internal-ids (wflow/workflow))]
                                          (exec/print-deps-in-order new-wf)))})
    (update-button run-wf-button
                   {:widget-select-fn (fn [event]
                                        (exec/run-workflow (wflow/workflow)))})
    (update-button print-wf-button
                   {:widget-select-fn (fn [event]
                                        (println (fformat/workflow-to-string (wflow/workflow))))})
    (update-button print-wf-inst-button
                   {:widget-select-fn (fn [event]
                                        (let [wf-inst (wfinstance)
                                              wf-inst-str (fformat/workflow-instance-to-string wf-inst)]
                                          (println wf-inst-str)))})
    button-group))

(defn testing-group
  "create a Group widget to contain all of the test code for widgets, etc."
  [parent]
  (let [testing-group (new-widget* Group parent SWT/SHADOW_ETCHED_OUT)
        label2 (Label. testing-group  SWT/CENTER)]
    (doto testing-group
      (.setText "Testing")
      (.setLayout (RowLayout. SWT/VERTICAL))
      ;; (.setLayoutData (GridData. GridData/HORIZONTAL_ALIGN_BEGINNING))
      )
    (doto label2
      (.setText "Testing/non-working button(s)"))
    (create-widgets-with-names testing-group Button SWT/PUSH ["one" "two" "three"])
    (do
      (create-widgets-with-names testing-group Button SWT/RADIO ["Radio 1" "Radio 2" "Radio 3"])
      (create-widgets-with-names testing-group Button SWT/TOGGLE ["Tog 1" "Tog 2" "Tog 3"])
      (create-widgets-with-names testing-group Button SWT/CHECK [ "Check one" "...two" "...three"]))
    testing-group))

(defn- general-ctab-content
  "create a tab in the CTabFolder for general widges (ex: SSH cnxn props, buttons(?), pre-defined WFs)"
  [parent]
  (let [comp (new-widget {:keyname :comp :widget-class Composite :parent parent :styles [SWT/BORDER]})
        exec-group (execution-group comp)
        button-group (button-group comp)
        ;; button-debugging-group (button-debugging-group comp)
        pre-wf-tree-group (predefined-wfs-tree-group comp)]
    (swt-util/stack-full-width comp {:margin 10} [exec-group button-group pre-wf-tree-group])
    comp))

;;
;; Edit WF tab functions
;;

(defn- edit-job-tree-table
  "create a regular SWT Table for editing a job in the WF"
  [parent]
  (let [table-group (new-widget {:keyname :table-group :widget-class Group :parent parent :styles [SWT/SHADOW_ETCHED_OUT] :text "Edit Workflow Job"}) 
        table (new-widget {:keyname :table :widget-class Table :parent table-group :styles [SWT/SINGLE SWT/FULL_SELECTION]})
        col1 (new-widget {:keyname :col1 :widget-class TableColumn :parent table :styles [SWT/LEFT]})
        ;; item1 (new-widget {:keyname :item1 :widget-class TableItem :parent table :styles [SWT/NONE]})
        item1 (TableItem. table SWT/NONE)]
    (doto table
      (.setHeaderVisible true)
      (.setLinesVisible true))
    (do
      (.pack col1))
    (doto table
      (.setRedraw true))
    (doto table-group
      (.setLayout (FillLayout.)))
    table-group))

(defn- edit-job-tree-table-viewer
  "create a JFace TreeTable viewer for editing a job in the WF"
  [parent]
  (let [job-fields (type-util/class-fields wfeditor.model.workflow.Job)
        ;; table-comp (new-widget {:keyname :table-comp :widget-class
        ;; Composite :parent parent :styles [SWT/NONE]})
        table-group (new-widget {:keyname :table-group :widget-class Group :parent parent :styles [SWT/SHADOW_ETCHED_OUT] :text "Edit Workflow Job"})
        ttv (TableViewer. table-group)
        content-provider (proxy [IStructuredContentProvider]
                             []
                           (getElements [input-data]
                             (to-array input-data))
                           (inputChanged [viewer old-input new-input])
                           (dispose []))
        label-provider (proxy [ITableLabelProvider]
                           []
                         (addListener [listener])
                         (dispose [])
                         (getColumnImage [element column-index]
                           nil)
                         (getColumnText [element column-index]
                           ;; (println "element = " element)
                           ;; (println "class element = " (class element))
                           (str element))
                         (isLabelProperty [element property]
                           false)
                         (removeListener [listener]))]
    (do
      (. (TableColumn. (.getTable ttv) SWT/LEFT) setText "Job field")
      (.setText (TableColumn. (.getTable ttv) (SWT/RIGHT)) "Extra field for testing")
      (.. ttv getTable (setLayoutData (GridData. (GridData/FILL_BOTH)))))
    (doto table-group
      (.setLayout (GridLayout.)))
    (doto ttv
      (.setContentProvider content-provider)
      (.setLabelProvider label-provider)
      (.setInput job-fields))
    ;; table-comp
    (doto (.getTable ttv)
      (.setHeaderVisible true)
      (.setLinesVisible true)
      (.setRedraw true)
      ;; (.pack)
      (.showColumn (last (.getColumns (.getTable ttv))))
      (.showColumn (first (.getColumns (.getTable ttv)))))
    table-group
    ))

(defn- test-list-viewer-group
  "create a ListViewer to test problems with TableViewer code"
  [parent]
  (let [group (new-widget {:keyname :list-group :widget-class Group :parent parent :styles [SWT/SHADOW_ETCHED_OUT] :text "List Viewer?"})
        list-viewer (ListViewer. group)
        test-input ["a" "c" "b" "yo"]
        content-provider (proxy [IStructuredContentProvider]
                             []
                           (getElements [input-data] 
                             (to-array input-data))
                           (inputChanged [viewer old-input new-input])
                           (dispose []))
        label-provider (proxy [ILabelProvider]
                           []
                         (addListener [listener])
                         (dispose [])
                         (getImage [element]
                           nil)
                         (getText [element]
                           (str element))
                         (isLabelProperty [element property]
                           false)
                         (removeListener [listener]))]
    (doto group
      (.setLayout (FillLayout.)))
    (doto list-viewer
      (.setContentProvider content-provider)
      (.setLabelProvider label-provider)
      (.setInput test-input))
    group))

(defn- edit-wf-ctab-content
  "create a tab for editing the WF"
  [parent]
  (let [comp (new-widget {:keyname :comp :widget-class Composite :parent parent :styles [SWT/BORDER]})
        label (new-widget {:keyname :some-label :widget-class Label :parent comp :styles [SWT/LEFT] :text "This is some label"})
        button (new-widget {:keyname :some-button :widget-class Button :parent comp :styles [SWT/PUSH] :text "This is some button"})
        edit-job-table-comp (edit-job-tree-table-viewer comp)
        list-viewer-group (test-list-viewer-group comp)
        edit-job-table2-comp (edit-job-tree-table comp)
        spacer-comp (new-widget {:keyname :spacer-comp :widget-class Composite :parent comp :styles [SWT/NONE]})
        ]
    (swt-util/stack-full-width comp {:marge 10} [label button edit-job-table-comp list-viewer-group
                                                 edit-job-table2-comp
                                                 spacer-comp])
    comp))

;;
;; build left nav pane
;;

(defn ui-editor-left
  "create the entire left-hand side navigation pane"
  [parent]
  (let [tab-folder (new-widget {:keyname :tab-folder :widget-class CTabFolder :parent parent :styles [SWT/TOP SWT/FLAT]})
        gen-tab (new-widget {:keyname :general-tab :widget-class CTabItem :parent tab-folder :styles [SWT/NONE] :extra-ctor-args [0] :text "General"})
        edit-wf-tab (new-widget {:keyname :edit-wf-tab :widget-class CTabItem :parent tab-folder :styles [SWT/NONE] :extra-ctor-args [1] :text "Edit WF"})]
    (do
      (.setControl gen-tab (general-ctab-content tab-folder))
      (.setControl edit-wf-tab (edit-wf-ctab-content tab-folder)))
    tab-folder))

;;
;; refs - binding initial values
;;

(def exec-props (ref {:user (. System getProperty "user.name")
                      :exec-dom "SGE"
                      :rem-host io-const/DEFAULT-HOST
                      :rem-port io-const/DEFAULT-PORT
                      :loc-port io-const/DEFAULT-LOCAL-PORT}))

;;

;; add-watch definitions
;;

;; updating the workflow currently held in state in model.workflow
;; automatically whenever the global-job-statuses changes value, using
;; the add-watch mechanism.  we have to trust that following this
;; add-watch, the workflow/wf gets instantiated before the
;; global-job-statuses changes
(add-watch task-status/global-job-statuses
           :re-bind (fn [key r old new]
                      (if (= :client @io-const/relay-type)
                        (let [curr-wfinst (wfinstance)
                              updated-wfinst (exec/update-wfinst-sge curr-wfinst)
                              updated-wf (:workflow updated-wfinst)]
                          (wflow/set-workflow updated-wf)))))