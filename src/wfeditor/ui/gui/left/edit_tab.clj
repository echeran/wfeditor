(ns wfeditor.ui.gui.left.edit-tab
  (:require [wfeditor.io.util.const :as io-const]
            [wfeditor.model.workflow :as wflow]
            [wfeditor.model.util.type :as type-util]
            [wfeditor.io.execution :as exec]
            [wfeditor.io.status.task-run :as task-status]
            [wfeditor.io.file.wfeformat :as fformat]
            [wfeditor.ui.gui.left.general-tab :as general-tab]
            [wfeditor.ui.state.gui :as gui-state]
            [wfeditor.ui.util.const :as ui-const]
            [clojure.string :as string]
            [clojure.zip :as zip])
  (:use [wfeditor.ui.util.swt :as swt-util])
  (:import
   org.eclipse.swt.SWT
   (org.eclipse.swt.layout FillLayout RowLayout GridLayout GridData FormLayout FormData FormAttachment)
   (org.eclipse.swt.widgets Button FileDialog Group Text Combo Composite Display TableColumn Table TableItem Label)
   (org.eclipse.swt.events SelectionEvent SelectionAdapter ModifyListener ModifyEvent)
   (org.eclipse.jface.viewers TreeViewer ITreeContentProvider ILabelProvider IDoubleClickListener TableViewer IStructuredContentProvider ITableLabelProvider ListViewer ICellModifier TextCellEditor ViewerSorter ColumnLabelProvider ColumnViewerToolTipSupport)
   java.net.URL
   (org.eclipse.swt.custom CTabFolder CTabItem)
   (org.eclipse.jface.layout TableColumnLayout)
   (org.eclipse.jface.dialogs TitleAreaDialog)
   (org.eclipse.jface.window Window)
   wfeditor.model.workflow.Job))


;;
;; Edit WF tab functions
;;

(defn- edit-job-tree-table-viewer
  "create a JFace TreeTable viewer for editing a job in the WF"
  [parent]
  (let [table-group (new-widget {:keyname :table-group :widget-class Group :parent parent :styles [SWT/SHADOW_ETCHED_OUT] :text "Edit Workflow Job"})
        ;; job (atom (wflow/new-job-fn "Job Name" "Prog. Exec. Loc." "Prog. Args." "Prog. Opts."))
        ttv (TableViewer. table-group)
        ;; job (atom @gui-state/job-to-edit)
        job-fields (type-util/class-fields wfeditor.model.workflow.Job)
        column-headings ["Job field" "Value"]
        columns (doall
                 (for [ch column-headings]
                   (new-widget {:keyname (keyword (str "col-" ch)) :widget-class TableColumn :parent (.getTable ttv) :styles [SWT/LEFT] :text ch})))
        refresh-table-gui-fn (fn [ttv]
                               (.refresh ttv)
                               (dorun
                                (doseq [column (.. ttv getTable getColumns)]
                                  (.pack column)))
                               (dorun
                                (doseq [column (.. ttv getTable getColumns)]
                                  (.showColumn (.getTable ttv) column)))
                               (.showColumn (.getTable ttv) (first (.. ttv getTable getColumns)))
                               (.redraw (.getTable ttv))
                               (.update (.getTable ttv)))
        content-provider (proxy [IStructuredContentProvider]
                             []
                           (getElements [input-data]
                             (to-array input-data))
                           (inputChanged [viewer old-input new-input]
                             (when-not new-input
                               (.setInput viewer job-fields)
                               (dosync
                                (ref-set gui-state/job-to-edit nil)))
                             (refresh-table-gui-fn ttv))
                           (dispose []))
        label-provider (proxy [ITableLabelProvider]
                           []
                         (addListener [listener])
                         (dispose [])
                         (getColumnImage [element column-index]
                           nil)
                         (getColumnText [element column-index]
                           (let [result
                                 (if (string? element)
                                   (condp = column-index
                                     0 (ui-const/JOB-FIELD-FULL-NAMES (keyword element))
                                     1 ui-const/NIL-VAL-STR-REP)
                                   (condp = column-index
                                     0 (str (ui-const/JOB-FIELD-FULL-NAMES (nth element column-index)))
                                     1 (let [key (nth element 0)]
                                         (str (or (get @gui-state/job-editor-cache key) ui-const/NIL-VAL-STR-REP))
                                         (if-let [val (get @gui-state/job-editor-cache key)]
                                           (condp = key
                                             :task-statuses (if val (task-status/status-field val) ui-const/NIL-VAL-STR-REP)
                                             (str val))
                                           ui-const/NIL-VAL-STR-REP))))]
                             result))
                         (isLabelProperty [element property]
                           false)
                         (removeListener [listener]))
        col-props ["key" "value"]
        cell-modifier (proxy [ICellModifier]
                          []
                        (canModify [element property]
                          (if (or (nil? @gui-state/job-editor-cache)
                                  (string? element)
                                  (and (= Job (class @gui-state/job-editor-cache)) (:id @gui-state/job-editor-cache)))
                            false
                            (let [key (nth element 0)]
                              (if (and (= "value" property) (not (#{:id :task-statuses :prog-args :prog-opts :array} key)))
                                true
                                false))))
                        (getValue [element property]
                          (if (not (string? element))
                            (condp = property
                              "key" (name (nth element 0))
                              "value" (or (get @gui-state/job-editor-cache (nth element 0)) ui-const/NIL-VAL-STR-REP))
                            (condp = property
                              "key" element
                              "value" ui-const/NIL-VAL-STR-REP)))
                        (modify [element property value]
                          (let [element (if (= TableItem (class element))
                                          (.getData element)
                                          element)
                                key (nth element 0)
                                val (if (= value ui-const/NIL-VAL-STR-REP)
                                      nil
                                      value)]
                            (when-not (#{:id} key)
                              (let [alter-assoc-fn (fn [j k v] (when j (assoc j k v)))]
                                (dosync
                                 (alter gui-state/job-editor-cache alter-assoc-fn key val)
                                 (let [old-job @gui-state/job-to-edit
                                       new-job @gui-state/job-editor-cache
                                       wf (wflow/workflow)
                                       new-wf (wflow/replace-job wf old-job new-job)]
                                   (wflow/set-workflow new-wf))
                                 (ref-set gui-state/job-to-edit @gui-state/job-editor-cache)))
                              (.refresh ttv)))))
        cell-editors (for [col col-props]
                       (TextCellEditor. (.getTable ttv)))
        view-sorter (proxy [ViewerSorter]
                        []
                      (compare [viewer e1 e2]
                        (if (string? e1)
                          (compare (.indexOf job-fields e1) (.indexOf job-fields e2))
                          (compare (.indexOf job-fields (name (nth e1 0))) (.indexOf job-fields (name (nth e2 0)))))))]
    ;; TODO: fix extra column to the right using TableColumnLayout and
    ;; setting ColumnWeightData using proportions and minimum widths
    ;; http://javafact.com/2010/07/26/working-with-jface-tableviewer/
    ;; http://stackoverflow.com/questions/9211106/swt-table-layout-resize-the-column-of-a-table-to-fill-all-the-available-spac
    ;; TODO: figure out how to get the initial input to the tableviewer
    ;; to be nil and yet have all the job's keys appear as rows and
    ;; the table show all those rows.  somehow helpful related links:
    ;; http://www.eclipse.org/forums/index.php/t/158152/
    ;; http://stackoverflow.com/questions/4508564/make-a-jface-tableviewer-resize-with-its-surrounding-composite
    ;; http://www.eclipse.org/nebula/widgets/xviewer/xviewer.php
    ;; http://www.eclipse.org/forums/index.php/m/635630/
    ;; http://www.eclipsezone.com/eclipse/forums/t76524.html


    ;; add-watch
    (add-watch gui-state/job-to-edit :re-bind (fn [key r old new]
                                                (when-not (= @gui-state/job-editor-cache new)
                                                  (dosync
                                                   (ref-set gui-state/job-editor-cache new)))
                                                (refresh-table-gui-fn ttv)))
    ;; basic display config
    (doto table-group
      (.setLayout (GridLayout. 1 false)))
    (doto ttv
      (.setContentProvider content-provider)
      (.setLabelProvider label-provider)
      (.setInput @gui-state/job-to-edit)
      (.setSorter view-sorter)) 
    ;; configs to format table display and align cols properly
    (doto (.getTable ttv)
      (.setLayoutData (GridData. GridData/FILL_BOTH))
      (.setHeaderVisible true)
      (.setLinesVisible true)
      (.setRedraw true)
      ;; don't pack table - shrinks the right margin if not needed,
      ;; looks weird
      ;; (.pack)
      )
    (dorun
     (map #(.showColumn (.getTable ttv) %) (.. ttv getTable getColumns)))
    (dorun
     (map (memfn pack) (.. ttv getTable getColumns)))
    (dosync
     (ref-set gui-state/job-to-edit nil))
    (refresh-table-gui-fn ttv)
    ;; configs for control editors
    (doto ttv
      ;; into-array preserves the object type in a Java array better
      ;; than to-array
      (.setColumnProperties (into-array col-props))
      (.setCellModifier cell-modifier)
      (.setCellEditors (into-array cell-editors)))
    ;; return value
    table-group))

(defn- gui-add-job
  "create a new job, add it to the current wf, using a dialog (requires SWT ancestor shell widget)"
  [parent]
  (let [
        job-name (atom "")
        prog-exec-loc (atom "")
        prog-args (atom [])
        prog-opts (atom {})
        ;; TODO: create custom modal
        dlg (proxy [org.eclipse.jface.dialogs.TitleAreaDialog]
                [parent]
              (createContents [parent]
                (let [contents (proxy-super createContents parent)]
                  (do
                    (. this setMessage "Set properties of the new job")
                    (. this setTitle "Add new job"))
                  contents))
              (createDialogArea [parent]
                (let [comp (proxy-super createDialogArea parent)
                      ;; grid-comp (Composite. comp SWT/NONE)
                      comp2 (Composite. comp SWT/NONE)
                      ;; text2 (Text. comp2 SWT/BORDER)
                      ;; label2 (doto (Label. comp2 SWT/LEFT) (.setText "hello"))
                      job-name-label (doto (Label. comp2 SWT/LEFT) (.setText (str (ui-const/JOB-FIELD-FULL-NAMES :name) ":")))
                      job-name-text (Text. comp2 SWT/BORDER)
                      prog-exec-loc-label (doto (Label. comp2 SWT/LEFT) (.setText (str (ui-const/JOB-FIELD-FULL-NAMES :prog-exec-loc) ":")))
                      prog-exec-loc-text (Text. comp2 SWT/BORDER)
                      job-args-label (doto (Label. comp2 SWT/LEFT) (.setText (str (ui-const/JOB-FIELD-FULL-NAMES :prog-args) ":")))
                      job-args-text (Text. comp2 SWT/BORDER)
                      job-opts-label (doto (Label. comp2 SWT/LEFT) (.setText (str (ui-const/JOB-FIELD-FULL-NAMES :prog-opts) ":")))
                      job-opts-text (Text. comp2 SWT/BORDER)
                      grid-layout (GridLayout.)
                      ]
                  ;; do not set a layout for the composite given to
                  ;; createDialogArea by the super class because SWT
                  ;; seems to get confused
                  (.setLayout comp2 (FillLayout. SWT/VERTICAL))
                  (.setLayout comp2 grid-layout)
                  (set! (. grid-layout numColumns) 2)
                  (dorun (map #(.setLayoutData % (GridData. GridData/HORIZONTAL_ALIGN_BEGINNING)) [job-name-label prog-exec-loc-label job-args-label job-opts-label]))
                  (dorun (map #(.setLayoutData % (GridData. GridData/FILL_HORIZONTAL)) [job-name-text prog-exec-loc-text job-args-text job-opts-text]))
                  comp)))]
    (when (= (.open dlg) Window/OK)
      (let [new-job (wflow/nil-job-fn)]
        
        (dosync
         (alter wflow/wf wflow/add-job new-job))))))

(defn- mod-buttons-group
  "a set of buttons (and widgets) to modify (add/delete/etc.) the WF"
  [parent]
  (let [group (new-widget {:keyname :group :widget-class Group :parent parent :style [SWT/SHADOW_NONE] :text "Add/Delete Jobs"})
        bounding-comp (new-widget {:keyname :bounding-comp :widget-class Composite :parent group :style [SWT/NONE]})
        add-button (new-widget {:keyname :add-button :widget-class Button :parent bounding-comp :style [SWT/PUSH] :text "Add a Job"})
        del-button (new-widget {:keyname :del-button :widget-class Button :parent bounding-comp :style [SWT/PUSH] :text "Delete a Single Job"})]
    (swt-util/stack-full-width bounding-comp {:margin 10} [add-button del-button])
    (doto group
      (.setLayout (RowLayout. SWT/VERTICAL)))
    (update-button add-button
                   {:widget-select-fn (fn [event]
                                        (gui-add-job (get-ancestor-shell group)))})
    group))

(defn edit-wf-ctab-content
  "create a tab for editing the WF"
  [parent]
  (let [comp (new-widget {:keyname :comp :widget-class Composite :parent parent :styles [SWT/BORDER]})

        edit-job-table-group (edit-job-tree-table-viewer comp)
        mod-group (mod-buttons-group comp)
        spacer-comp (new-widget {:keyname :spacer-comp :widget-class Composite :parent comp :styles [SWT/NONE]})
        ]
    (swt-util/stack-full-width comp {:margin 10} [edit-job-table-group mod-group spacer-comp])
    comp))