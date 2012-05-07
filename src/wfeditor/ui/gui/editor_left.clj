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
   (org.eclipse.swt.events SelectionEvent SelectionAdapter)))

;;
;; functions
;;


(defn ui-editor-left-create [parent]
  (let [exec-group (new-widget Group parent SWT/SHADOW_ETCHED_IN)
        user-label (new-widget Label exec-group SWT/LEFT)
        user-text (new-widget Text exec-group (bit-or SWT/SINGLE SWT/BORDER))
        exec-dom-label (new-widget Label exec-group SWT/LEFT)
        exec-dom-combo (new-widget Combo exec-group (bit-or SWT/DROP_DOWN SWT/READ_ONLY))
        rem-host-label (new-widget Label exec-group SWT/LEFT)
        rem-host-text (new-widget Text exec-group (bit-or SWT/SINGLE SWT/BORDER))
        rem-port-label (new-widget Label exec-group SWT/LEFT)
        rem-port-text (new-widget Text exec-group (bit-or SWT/SINGLE SWT/BORDER))
        loc-port-label (new-widget Label exec-group SWT/LEFT)
        loc-port-text (new-widget Text exec-group (bit-or SWT/SINGLE SWT/BORDER))
        button-group (new-widget Group parent SWT/SHADOW_NONE)
        print-wf-cmd-button (new-widget Button button-group SWT/PUSH)
        print-wf-sge-test-button (new-widget Button button-group SWT/PUSH)
        run-wf-button (new-widget Button button-group SWT/PUSH)
        print-wf-button (new-widget Button button-group SWT/PUSH)
        load-wf-button (new-widget Button button-group SWT/PUSH)
        save-wf-button (new-widget Button button-group SWT/PUSH)
        print-wf-inst-button (new-widget Button button-group SWT/PUSH)
        update-wf-inst-button (new-widget Button button-group SWT/PUSH)
        print-global-statuses-button (new-widget Button button-group SWT/PUSH)
        testing-group (new-widget Group parent SWT/SHADOW_ETCHED_OUT)
        label2 (Label. testing-group  SWT/CENTER)]
    (do
      (.setLayout parent (GridLayout.)))
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
      (.setText (. System getProperty "user.name"))
      (.setLayoutData (GridData. GridData/FILL_HORIZONTAL)))
    (doto exec-dom-label
      (.setText "Select execution domain:")
      (.setLayoutData (GridData. GridData/HORIZONTAL_ALIGN_BEGINNING)))
    (doto exec-dom-combo
      (.add "SGE")
      (.add "rem-piped-shell")
      (.select 0)
      (.setLayoutData (GridData. GridData/FILL_HORIZONTAL)))
    (doto rem-host-label
      (.setText "Remote host:")
      (.setLayoutData (GridData. GridData/HORIZONTAL_ALIGN_BEGINNING)))
    (doto rem-host-text
      (.setText io-const/DEFAULT-HOST)
      (.setLayoutData (GridData. GridData/FILL_HORIZONTAL)))
    (doto rem-port-label
      (.setText "Remote port:")
      (.setLayoutData (GridData. GridData/HORIZONTAL_ALIGN_BEGINNING)))
    (doto rem-port-text
      (.setText (str io-const/DEFAULT-PORT))
      (.setLayoutData (GridData. GridData/FILL_HORIZONTAL)))
    (doto loc-port-label
      (.setText "Local port:")
      (.setLayoutData (GridData. GridData/HORIZONTAL_ALIGN_BEGINNING)))
    (doto loc-port-text
      (.setText (str io-const/DEFAULT-LOCAL-PORT))
      (.setLayoutData (GridData. GridData/FILL_HORIZONTAL)))
    (doto button-group
      (.setLayout (FillLayout. SWT/VERTICAL))
      (.setText "Buttons")
      (.setLayoutData (GridData. GridData/FILL_BOTH)))


    
    
    ;; (doto print-wf-cmd-button
    ;;   (.setText "Print workflow command")
    ;;   (.addSelectionListener (proxy [SelectionAdapter]
    ;;                              []
    ;;                            (widgetSelected [event]
    ;;                              (exec/print-wf-command (wflow/workflow))))))
    ;; (doto print-wf-sge-test-button
    ;;   (.setText "Print SGE commands test")
    ;;   (.addSelectionListener (proxy [SelectionAdapter]
    ;;                              []
    ;;                            (widgetSelected [event]
    ;;                              (let [new-wf (wflow/wf-with-internal-ids (wflow/workflow))]
    ;;                                (exec/print-deps-in-order new-wf))))))
    ;; (doto run-wf-button
    ;;   (.setText "Run workflow")
    ;;   (.addSelectionListener (proxy [SelectionAdapter]
    ;;                              []
    ;;                            (widgetSelected [event]                                 
    ;;                              (exec/run-workflow (wflow/workflow))))))

    
    (update-button print-wf-cmd-button {:text "Print workflow command"
                                        :widget-select-fn (fn [event]
                                                            (exec/print-wf-command (wflow/workflow)))})
    (update-button print-wf-sge-test-button {:text "Print SGE commands test"
                                             :widget-select-fn (fn [event]
                                                                 (let [new-wf (wflow/wf-with-internal-ids (wflow/workflow))]
                                                                   (exec/print-deps-in-order new-wf)))})
    (update-button run-wf-button {:text "Run workflow"
                                  :widget-select-fn (fn [event]
                                                      (exec/run-workflow (wflow/workflow)))})


    

    
    ;; (doto print-wf-button
    ;;   (.setText "Print workflow")
    ;;   (.addSelectionListener (proxy [SelectionAdapter]
    ;;                              []
    ;;                            (widgetSelected [event]
    ;;                              (println (fformat/workflow-to-string (wflow/workflow)))))))
    ;; (doto load-wf-button
    ;;   (.setText "Load workflow")
    ;;   (.addSelectionListener (proxy [SelectionAdapter]
    ;;                              []
    ;;                            (widgetSelected [event]
    ;;                              (let [fd (new FileDialog (get-ancestor-shell parent) SWT/OPEN)]
    ;;                                (when-let [in-file-name (.open fd)]
    ;;                                  (fformat/set-workflow-from-file in-file-name)))))))
    ;; (doto save-wf-button
    ;;   (.setText "Save workflow")
    ;;   (.addSelectionListener (proxy [SelectionAdapter]
    ;;                              []
    ;;                            (widgetSelected [event]
    ;;                              (let [fd (FileDialog. (get-ancestor-shell parent) SWT/SAVE)]
    ;;                                (when-let [out-file-name (.open fd)]
    ;;                                  (fformat/save-workflow-to-file (wflow/workflow) out-file-name)))))))


    (update-button print-wf-button
                   {:text "Print workflow"                    
                    :widget-select-fn (fn [event]
                                        (println (fformat/workflow-to-string (wflow/workflow))))})
    (update-button load-wf-button
                   {:text "Load workflow"
                    :widget-select-fn (fn [event]
                                        (let [fd (new FileDialog (get-ancestor-shell parent) SWT/OPEN)]
                                                         (when-let [in-file-name (.open fd)]
                                                           (fformat/set-workflow-from-file in-file-name))))})
    (update-button save-wf-button
                   {:text "Save workflow"
                    :widget-select-fn (fn [event]
                                        (let [fd (FileDialog. (get-ancestor-shell parent) SWT/SAVE)]
                                          (when-let [out-file-name (.open fd)]
                                            (fformat/save-workflow-to-file (wflow/workflow) out-file-name))))})

    
    ;; (doto print-wf-inst-button
    ;;   (.setText "Print WF instance")
    ;;   (.addSelectionListener (proxy [SelectionAdapter]
    ;;                              []
    ;;                            (widgetSelected [event]
    ;;                              (let [username (.getText user-text)
    ;;                                    workflow (wflow/workflow)
    ;;                                    exec-domain (.getItem exec-dom-combo (.getSelectionIndex exec-dom-combo))
    ;;                                    wf-inst (wflow/new-wfinstance-fn username exec-domain workflow)
    ;;                                    wf-inst-str (fformat/workflow-instance-to-string wf-inst)]
    ;;                                (println wf-inst-str))))))
    
    ;; (doto update-wf-inst-button
    ;;   (.setText "Update WF instance via server")
    ;;   (.addSelectionListener (proxy [SelectionAdapter]
    ;;                              []
    ;;                            (widgetSelected [event]
    ;;                              (let [username (.getText user-text)
    ;;                                    workflow (wflow/workflow)
    ;;                                    exec-domain (.getItem exec-dom-combo (.getSelectionIndex exec-dom-combo))
    ;;                                    wf-inst (wflow/new-wfinstance-fn username exec-domain workflow)
    ;;                                    rem-host (.getText rem-host-text)
    ;;                                    rem-port (Integer/parseInt (.getText rem-port-text))
    ;;                                    loc-port (Integer/parseInt (.getText loc-port-text))
    ;;                                    loc-host io-const/DEFAULT-LOCAL-HOST
    ;;                                    server-host io-const/DEFAULT-SERVER-HOST-REL-TO-REMOTE]
    ;;                                (exec/update-wfinst-and-set-everywhere wf-inst rem-host rem-port loc-port loc-host server-host))))))
    ;; (doto print-global-statuses-button
    ;;   (.setText "Print global statuses")
    ;;   (.addSelectionListener (proxy [SelectionAdapter]
    ;;                              []
    ;;                            (widgetSelected [event]
    ;;                              (println (exec/global-statuses))))))


    (update-button print-wf-inst-button
                   {:text "Print WF instance"
                    :widget-select-fn (fn [event]
                                        (let [username (.getText user-text)
                                              workflow (wflow/workflow)
                                              exec-domain (.getItem exec-dom-combo (.getSelectionIndex exec-dom-combo))
                                              wf-inst (wflow/new-wfinstance-fn username exec-domain workflow)
                                              wf-inst-str (fformat/workflow-instance-to-string wf-inst)]
                                          (println wf-inst-str)))})
    (update-button update-wf-inst-button
                   {:text "Update WF instance via server"
                    :widget-select-fn (fn [event]
                                        (let [username (.getText user-text)
                                              workflow (wflow/workflow)
                                              exec-domain (.getItem exec-dom-combo (.getSelectionIndex exec-dom-combo))
                                              wf-inst (wflow/new-wfinstance-fn username exec-domain workflow)
                                              rem-host (.getText rem-host-text)
                                              rem-port (Integer/parseInt (.getText rem-port-text))
                                              loc-port (Integer/parseInt (.getText loc-port-text))
                                              loc-host io-const/DEFAULT-LOCAL-HOST
                                              server-host io-const/DEFAULT-SERVER-HOST-REL-TO-REMOTE]
                                          (exec/update-wfinst-and-set-everywhere wf-inst rem-host rem-port loc-port loc-host server-host)))})
    (update-button print-global-statuses-button
                   {:text "Print global statuses"
                    :widget-select-fn (fn [event]
                                        (println (exec/global-statuses)))})
    
    
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