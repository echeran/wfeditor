(ns wfeditor.ui.gui.editor-left
  (:require [wfeditor.io.util.const :as io-const]
            [wfeditor.model.workflow :as wflow]
            [wfeditor.io.execution :as exec]
            [wfeditor.io.status.task-run :as task-status]
            [wfeditor.io.file.wfeformat :as fformat])
  (:use [wfeditor.ui.util.swt :as swt-util])
  (:import
   org.eclipse.swt.SWT
   (org.eclipse.swt.layout FillLayout RowLayout GridLayout GridData)
   (org.eclipse.swt.widgets Label Button FileDialog Group Text Combo)
   (org.eclipse.swt.events SelectionEvent SelectionAdapter ModifyListener ModifyEvent)))

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

(defn- execution-group-create
  "create the group in the navpane storing the fields required for executing jobs on the remote server"
  [parent]
  (let [exec-group (new-widget Group parent {:styles [SWT/SHADOW_ETCHED_IN] :text "Execution Properties"})
        user-label (new-widget Label exec-group {:styles [SWT/LEFT] :text "Enter username:"})
        user-text (new-widget Text exec-group {:styles [SWT/SINGLE SWT/BORDER] :text (:user @exec-props) })
        exec-dom-label (new-widget Label exec-group {:styles [SWT/LEFT] :text "Select execution domain:"})
        exec-dom-combo (new-widget Combo exec-group {:styles [SWT/DROP_DOWN SWT/READ_ONLY]})
        rem-host-label (new-widget Label exec-group {:styles [SWT/LEFT] :text "Remote host:"})
        rem-host-text (new-widget Text exec-group {:styles [SWT/SINGLE SWT/BORDER] :text (:rem-host @exec-props)})
        rem-port-label (new-widget Label exec-group {:styles [SWT/LEFT] :text "Remote port:"})
        rem-port-text (new-widget Text exec-group {:styles [SWT/SINGLE SWT/BORDER] :text (str (:rem-port @exec-props))})
        loc-port-label (new-widget Label exec-group {:styles [SWT/LEFT] :text "Local port:"})
        loc-port-text (new-widget Text exec-group {:styles [SWT/SINGLE SWT/BORDER] :text (str (:loc-port @exec-props))})]
    (doto exec-group
      (.setLayoutData (GridData. GridData/FILL_HORIZONTAL)))
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

(defn- button-group-create
  "create the Group widget containing all of the buttons in the left navpane that do something"
  [parent]
  (let [button-group (new-widget Group parent {:styles [SWT/SHADOW_NONE] :text "Buttons"})
        load-wf-button (new-widget Button button-group {:styles [SWT/PUSH] :text "Load workflow"})
        save-wf-button (new-widget Button button-group {:styles [SWT/PUSH] :text "Save workflow"})
        run-wf-inst-button (new-widget Button button-group {:styles [SWT/PUSH] :text "Run WF instance"})]
    (doto button-group
      (.setLayout (FillLayout. SWT/VERTICAL))
      (.setLayoutData (GridData. GridData/FILL_BOTH)))
    (update-button load-wf-button
                   {:widget-select-fn (fn [event]
                                        (swt-util/file-dialog-open-wf (get-ancestor-shell parent)))})
    (update-button save-wf-button
                   {:widget-select-fn (fn [event]
                                        (swt-util/file-dialog-save-as-wf (get-ancestor-shell parent)))})
    (update-button run-wf-inst-button
                   {:widget-select-fn (fn [event]
                                        (let [{:keys [rem-host rem-port loc-port]} @exec-props
                                              wf-inst (wfinstance)
                                              loc-host io-const/DEFAULT-LOCAL-HOST
                                              server-host io-const/DEFAULT-SERVER-HOST-REL-TO-REMOTE]
                                          (exec/create-wfinst-and-set-everywhere wf-inst rem-host rem-port loc-port loc-host server-host)))}) 
    button-group))

(defn button-debugging-group-create
  "create the Group widget containing all of the debugging buttons in the left navpane"
  [parent]
  (let [button-group (new-widget Group parent {:styles [SWT/SHADOW_NONE] :text "Debugging Buttons"})
        print-global-statuses-button (new-widget Button button-group {:styles [SWT/PUSH] :text "Print locally-saved statuses"})
        update-server-statuses-button (new-widget Button button-group {:styles [SWT/PUSH] :text "Update statuses on server"})
        get-server-statuses-button (new-widget Button button-group {:styles [SWT/PUSH] :text "Get statuses from server"})]
    (doto button-group
      (.setLayout (FillLayout. SWT/VERTICAL))
      (.setLayoutData (GridData. GridData/FILL_BOTH)))
    (update-button print-global-statuses-button
                   {:widget-select-fn (fn [event]
                                        (println (task-status/global-statuses)))})
    (update-button update-server-statuses-button
                   {:widget-select-fn (fn [event]
                                        (let [{:keys [user exec-dom rem-host rem-port loc-port]} @exec-props
                                              loc-host io-const/DEFAULT-LOCAL-HOST
                                              server-host io-const/DEFAULT-SERVER-HOST-REL-TO-REMOTE]
                                          (exec/force-server-update-statuses-sge exec-dom user rem-host rem-port loc-port loc-host server-host)))})
    (update-button get-server-statuses-button
                   {:widget-select-fn (fn [event]
                                        (update-job-statuses-from-server))})
    button-group))

(defn button-testing-group-create
  "create the Group widget containing all of the testing buttons in the left navpane that aren't (currently) directly useful for program execution"
  [parent]
  (let [button-group (new-widget Group parent {:styles [SWT/SHADOW_NONE] :text "Testing Buttons"})
        print-wf-cmd-button (new-widget Button button-group {:styles [SWT/PUSH] :text "Print workflow command"})
        print-wf-sge-test-button (new-widget Button button-group {:styles [SWT/PUSH] :text "Print SGE commands test"})
        run-wf-button (new-widget Button button-group {:styles [SWT/PUSH] :text "Run workflow"})
        print-wf-button (new-widget Button button-group {:styles [SWT/PUSH] :text "Print workflow"})
        print-wf-inst-button (new-widget Button button-group {:styles [SWT/PUSH] :text "Print WF instance"})]
    (doto button-group
      (.setLayout (FillLayout. SWT/VERTICAL))
      (.setLayoutData (GridData. GridData/FILL_BOTH)))
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

(defn testing-group-create
  "create a Group widget to contain all of the test code for widgets, etc."
  [parent]
  (let [testing-group (new-widget* Group parent SWT/SHADOW_ETCHED_OUT)
        label2 (Label. testing-group  SWT/CENTER)]
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
      (create-widgets-with-names testing-group Button SWT/CHECK [ "Check one" "...two" "...three"]))
    testing-group))

(defn ui-editor-left-create
  "create the entire left-hand side navigation pane"
  [parent]
  (let [exec-group (execution-group-create parent)
        button-group (button-group-create parent)
        button-debugging-group (button-debugging-group-create parent)]
    (do
      (.setLayout parent (GridLayout.)))))

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
(add-watch task-status/global-job-statuses :re-bind (fn [key r old new] 
                                          (let [curr-wfinst (wfinstance)
                                                updated-wfinst (exec/update-wfinst-sge curr-wfinst)
                                                updated-wf (:workflow updated-wfinst)]
                                            (wflow/set-workflow updated-wf))))