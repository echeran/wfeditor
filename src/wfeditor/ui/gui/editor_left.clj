(ns wfeditor.ui.gui.editor-left
  (:require [wfeditor.io.util.const :as io-const]
            [wfeditor.model.workflow :as wflow]
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
   (org.eclipse.swt.widgets Label Button FileDialog Group Text Combo Composite Display)
   (org.eclipse.swt.events SelectionEvent SelectionAdapter ModifyListener ModifyEvent)
   org.eclipse.gmf.runtime.draw2d.ui.render.awt.internal.svg.export.GraphicsSVG
   (javax.xml.transform OutputKeys Transformer TransformerFactory)
   javax.xml.transform.dom.DOMSource
   javax.xml.transform.stream.StreamResult
   (org.eclipse.jface.viewers TreeViewer ITreeContentProvider ILabelProvider)
   java.net.URL))

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
                                        ;; page(s) describing Zest to SVG
                                        ;; export using GMF (+
                                        ;; dependencies)
                                        ;; http://standardout.org/2011/12/eclipse-draw2d-to-svg/
                                        ;; http://standardout.org/2011/12/off-screen-rendering-of-eclipse-zest-graphs/
                                        (let [gv @wfeditor.ui.gui.zest.canvas/gv
                                              graph (.getGraphControl gv)
                                              graph-root (.getRootLayer graph)
                                              viewbox (.. graph-root getBounds getCopy)
                                              graphics (GraphicsSVG/getInstance viewbox)]
                                          (try
                                            (.paint graph-root graphics)
                                            (let [svg-root (.getRoot graphics)
                                                  transformer (.newTransformer (TransformerFactory/newInstance))
                                                  dom-source (DOMSource. svg-root)]
                                              (.setAttributeNS svg-root nil "view-box" (string/join " " [(. viewbox x) (. viewbox y) (. viewbox width) (. viewbox height)]))
                                              (doto transformer
                                                (.setOutputProperty OutputKeys/METHOD "xml")
                                                (.setOutputProperty OutputKeys/ENCODING "UTF-8")
                                                (.setOutputProperty OutputKeys/INDENT "yes"))
                                              (. (. Display getCurrent) asyncExec
                                                 (fn []
                                                   (let [fd (new-widget {:widget-class FileDialog :parent (get-ancestor-shell parent) :styles [SWT/SAVE]})]
                                                     (when-let [out-file-name (.open fd)]
                                                       (with-open [out-stream (clojure.java.io/output-stream out-file-name)]
                                                         (let [result (StreamResult. out-stream)
                                                               source (DOMSource. svg-root)]
                                                           (.transform transformer source result))))))))
                                            (finally
                                             (.dispose graphics)))))})
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
  (let [pre-wf-tree ["Genetics" ["NGS" [(URL. "http://www.palmyrasoftware.com/wf/genetics/ngs/sample4.xml")]]]
        pre-wf-simple-zip-tree {"Genetics" [{"NGS" [(URL. "http://www.palmyrasoftware.com/wf/genetics/ngs/sample4.xml")]}]}
        simple-zip-fn (fn [simple-zip-tree] (zip/zipper map? (comp seq second first) (fn [n cs] (let [map (if (seq n) n {n []}) k (first (first map)) vals (second (first map))] (assoc map k (concat vals (seq cs))))) simple-zip-tree))
        ;; JFace thinks that the nil value in the vector created when
        ;; first creating a zipper is another root element, and throws
        ;; an exception when it discovers null arguments in a
        ;; .setInput method
        jface-simple-zip-fn (comp #(assoc-in % [1] {}) simple-zip-fn)
        tree-zip-closure-fn (fn closure-fn [z]
                              {:data z
                               :apply-fn (fn [new-fn]
                                           (closure-fn (new-fn z)))})
        pre-wf-zip (jface-simple-zip-fn pre-wf-simple-zip-tree)
        pre-wf-zip-closure (tree-zip-closure-fn (simple-zip-fn pre-wf-simple-zip-tree))
        ;; _ (println "pref-wf-zip = " pre-wf-zip)
        tree-content-provider (proxy [ITreeContentProvider]
                                  []
                                ;; the "content" that will be
                                ;; manipulated by the JFace tree
                                ;; viewer will be entirely closures of zippers
                                ;; located at nodes, not the actual
                                ;; node-data themselves
                                (getChildren [zc] 
                                  ;; (println "JFace TreeViewer getting children for node = " (:data ((:apply-fn zc) zip/node)))
                                  (let [first-child ((:apply-fn zc) zip/down)
                                        rest-children (loop [rcs []
                                                             czc ((:apply-fn first-child) zip/right)]
                                                        (if-not (:data czc)
                                                          rcs
                                                          (recur (conj rcs czc) ((:apply-fn czc) zip/right))))
                                        result (concat [first-child] rest-children)
                                        array-result (to-array result)]
                                    ;; (println "children return = " result)
                                    array-result))
                                (getElements [zc]
                                  ;; (println "getting root nodes == root node = " (zip/node (simple-zip-fn (:data ((:apply-fn zc) zip/root)))) "for node = " (:data ((:apply-fn zc) zip/node)))
                                  (to-array [(tree-zip-closure-fn (simple-zip-fn (:data ((:apply-fn zc) zip/root))))]) )
                                (getParent [zc]
                                  ;; (println "getting parent for node = " (:data ((:apply-fn zc) zip/node))  " which is " (:data ((:apply-fn zc) (comp zip/node zip/up))))
                                  ((:apply-fn zc) zip/up))
                                (hasChildren [zc]
                                  ;; (when-not (and zc (:data zc))
                                  ;;   (println "zipper is nil"))
                                  ;; (println "checking .hasChildren for " (:data zc))
                                  ;; (println "node name = " (zip/node z) " has children? " (str (if (and z (zip/node z) (map? (zip/node z))) true false)))
                                  (if (and zc (:data zc) (:data ((:apply-fn zc) zip/node)) (map? (:data ((:apply-fn zc) zip/node))))
                                    true
                                    false))
                                (dispose [])
                                (inputChanged [viewer old-input new-input]
                                  ;; (println "input has changed. new input = " new-input)
                                  ))
        tree-label-provider (proxy [ILabelProvider]
                                []
                              ;; the label provider's responsibility
                              ;; is to take the zipper "content" and
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
                                  ;; (println "result = " result  "node = " node)
                                  result))
                              (isLabelProperty [zc property]
                                nil)
                              (removeListener [listener]))
        tree-group (new-widget {:keyname :pre-wf-tree-group :widget-class Group :parent parent :styles [SWT/SHADOW_NONE] :text "Predefined Workflows"})
        tree-viewer (TreeViewer. tree-group)]
    (doto tree-group
      (.setLayout (FillLayout.)))
    (doto tree-viewer
      (.setContentProvider tree-content-provider)
      (.setLabelProvider tree-label-provider)
      (.setInput pre-wf-zip-closure))
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

(defn ui-editor-left
  "create the entire left-hand side navigation pane"
  [parent]
  (let [comp-left (new-widget {:keyname :editor-left :widget-class Composite :parent parent :styles [SWT/BORDER]})
        exec-group (execution-group comp-left)
        button-group (button-group comp-left)
        ;; button-debugging-group (button-debugging-group comp-left)
        pre-wf-tree-group (predefined-wfs-tree-group comp-left)
        ]
    (swt-util/stack-full-width comp-left {:margin 10} [exec-group button-group pre-wf-tree-group])
    comp-left))

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