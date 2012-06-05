(ns wfeditor.ui.gui.editor-left
  (:require [wfeditor.io.util.const :as io-const]
            [wfeditor.model.workflow :as wflow]
            [wfeditor.io.execution :as exec]
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

(defn- button-group-create
  "create the Group widget containing all of the buttons in the left navpane that do something"
  [parent]
  (let [
        button-group (new-widget Group parent {:styles [SWT/SHADOW_NONE] :text "Buttons"})
        load-wf-button (new-widget Button button-group {:styles [SWT/PUSH] :text "Load workflow"})
        save-wf-button (new-widget Button button-group {:styles [SWT/PUSH] :text "Save workflow"})
        run-wf-inst-button (new-widget Button button-group {:styles [SWT/PUSH] :text "Run WF instance via server"})
        update-wf-inst-button (new-widget Button button-group {:styles [SWT/PUSH] :text "Update WF instance via server"})
        update-server-statuses-button (new-widget Button button-group {:styles [SWT/PUSH] :text "Update statuses on server"})
        print-global-statuses-button (new-widget Button button-group {:styles [SWT/PUSH] :text "Print global statuses"})

        print-wf-cmd-button (new-widget Button button-group {:styles [SWT/PUSH] :text "Print workflow command"})
        print-wf-sge-test-button (new-widget Button button-group {:styles [SWT/PUSH] :text "Print SGE commands test"})
        run-wf-button (new-widget Button button-group {:styles [SWT/PUSH] :text "Run workflow"})
        print-wf-button (new-widget Button button-group {:styles [SWT/PUSH] :text "Print workflow"})
        print-wf-inst-button (new-widget Button button-group {:styles [SWT/PUSH] :text "Print WF instance"})

        ]
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
    (update-button load-wf-button
                   {:widget-select-fn (fn [event]
                                        (let [fd (new FileDialog (get-ancestor-shell parent) SWT/OPEN)]
                                                         (when-let [in-file-name (.open fd)]
                                                           (fformat/set-workflow-from-file in-file-name))))})
    (update-button save-wf-button
                   {:widget-select-fn (fn [event]
                                        (let [fd (FileDialog. (get-ancestor-shell parent) SWT/SAVE)]
                                          (when-let [out-file-name (.open fd)]
                                            (fformat/save-workflow-to-file (wflow/workflow) out-file-name))))})
    (update-button print-wf-inst-button
                   {:widget-select-fn (fn [event]
                                        (let [workflow (wflow/workflow)
                                              {:keys [user exec-dom]} @exec-props
                                              wf-inst (wflow/new-wfinstance-fn user exec-dom workflow)
                                              wf-inst-str (fformat/workflow-instance-to-string wf-inst)]
                                          (println wf-inst-str)))})
    (update-button run-wf-inst-button
                   {:widget-select-fn (fn [event]
                                        (let [workflow (wflow/workflow)
                                              {:keys [user exec-dom rem-host rem-port loc-port]} @exec-props
                                              wf-inst (wflow/new-wfinstance-fn user exec-dom workflow)
                                              loc-host io-const/DEFAULT-LOCAL-HOST
                                              server-host io-const/DEFAULT-SERVER-HOST-REL-TO-REMOTE]
                                          (exec/create-wfinst-and-set-everywhere wf-inst rem-host rem-port loc-port loc-host server-host)))})
    (update-button update-wf-inst-button
                   {:widget-select-fn (fn [event]
                                        (let [workflow (wflow/workflow)
                                              {:keys [user exec-dom rem-host rem-port loc-port]} @exec-props
                                              wf-inst (wflow/new-wfinstance-fn user exec-dom workflow)
                                              loc-host io-const/DEFAULT-LOCAL-HOST
                                              server-host io-const/DEFAULT-SERVER-HOST-REL-TO-REMOTE]
                                          (exec/update-wfinst-and-set-everywhere wf-inst rem-host rem-port loc-port loc-host server-host)))})
    (update-button print-global-statuses-button
                   {:widget-select-fn (fn [event]
                                        (println (exec/global-statuses)))})
    (update-button update-server-statuses-button
                   {:widget-select-fn (fn [event]
                                        (let [{:keys [user exec-dom rem-host rem-port loc-port]} @exec-props
                                              loc-host io-const/DEFAULT-LOCAL-HOST
                                              server-host io-const/DEFAULT-SERVER-HOST-REL-TO-REMOTE]
                                          (exec/update-server-statuses-sge exec-dom user rem-host rem-port loc-port loc-host server-host)))})
    button-group))

(defn ui-editor-left-create
  "create the entire left-hand side navigation pane"
  [parent]
  (let [exec-group (execution-group-create parent)
        button-group (button-group-create parent) 
        testing-group (new-widget* Group parent SWT/SHADOW_ETCHED_OUT)
        label2 (Label. testing-group  SWT/CENTER)]
    (do
      (.setLayout parent (GridLayout.))) 
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

;;
;; refs - binding initial values
;;

(def exec-props (ref {:user (. System getProperty "user.name")
                      :exec-dom "SGE"
                      :rem-host io-const/DEFAULT-HOST
                      :rem-port io-const/DEFAULT-PORT
                      :loc-port io-const/DEFAULT-LOCAL-PORT}))