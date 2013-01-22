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
            [clojure.contrib.seq :as seq]
            [clojure.contrib.zip-filter.xml :as zfx]
            [clojure.string :as string]
            [clojure.zip :as zip])
  (:use [wfeditor.ui.util.swt :as swt-util]
        [clojure.contrib.core :only (-?>)])
  (:import
   org.eclipse.swt.SWT
   org.eclipse.swt.SWTException
   org.eclipse.swt.graphics.GC
   org.eclipse.core.runtime.AssertionFailedException
   (org.eclipse.swt.layout FillLayout RowLayout GridLayout GridData FormLayout FormData FormAttachment)
   (org.eclipse.swt.widgets Button FileDialog Group Text Combo Composite Display TableColumn Table TableItem Label TreeItem TreeColumn)
   (org.eclipse.swt.events SelectionEvent SelectionAdapter ModifyListener ModifyEvent)
   (org.eclipse.jface.viewers TreeViewer ITreeContentProvider ILabelProvider IDoubleClickListener TableViewer IStructuredContentProvider ITableLabelProvider ListViewer ICellModifier TextCellEditor ViewerSorter ColumnLabelProvider ColumnViewerToolTipSupport ColumnViewerEditor TreeViewerColumn EditingSupport ITreeViewerListener
                              StyledCellLabelProvider)
   java.net.URL
   (org.eclipse.swt.custom CTabFolder CTabItem)
   (org.eclipse.jface.layout TableColumnLayout)
   (org.eclipse.jface.dialogs TitleAreaDialog)
   (org.eclipse.jface.window Window)
   wfeditor.model.workflow.Job))


;;
;; Edit WF tab functions
;;

;; (defrecord ZipperVector [zippers])

(defn- tree-viewer-test
  "create a test zip to create a TreeViewer"
  [parent]
  (let [
        is-branch-fn (every-pred map? (complement (partial instance? clojure.lang.IRecord)))
        simple-zip-fn (fn [simple-zip-tree] (zip/zipper is-branch-fn (comp seq second first) (fn [n cs] (let [map (if (seq n) n {n []}) k (first (first map)) vals (second (first map))] (assoc map k (concat vals (seq cs))))) simple-zip-tree))
        ;; JFace thinks that the nil value in the vector created when
        ;; first creating a zipper is another root element, and throws
        ;; an exception when it discovers null arguments in a
        ;; .setInput method
        ;; hence, have enclosed the zipper in a vector

        dummy-zip-tree {:a [{:b [:e :f]} {:c :g} {:d [{:h [:i]}]}]}
        dummy-zip (simple-zip-fn dummy-zip-tree)
        zipper-vector [dummy-zip]
      
        tree-content-provider (proxy [ITreeContentProvider]
                                  []
                                ;; the "content" that will be
                                ;; manipulated by the JFace tree
                                ;; viewer will be entirely closures of zippers
                                ;; located at nodes, not the actual
                                ;; node-data themselves
                                (getChildren [zc]
                                  (let [first-child (zip/down zc)
                                        rest-children (loop [rcs []
                                                             czc (zip/right first-child)]
                                                        (if-not czc
                                                          rcs
                                                          (recur (conj rcs czc) (zip/right czc))))
                                        result (concat [first-child] rest-children)
                                        array-result (to-array result)]
                                    array-result))
                                (getElements [zipper-vector]
                                  (to-array  zipper-vector))
                                (getParent [zc]
                                  (zip/up zc))
                                (hasChildren [zc] 
                                  (if (and zc (zip/node zc) (is-branch-fn (zip/node zc)))
                                    true
                                    false))
                                (dispose [])
                                (inputChanged [viewer old-input new-input]))
        get-node-fn (fn [zc]
                      (let [node-subtree (zip/node zc)
                            node (cond
                                  (is-branch-fn node-subtree) (first (first node-subtree))
                                  (nil? node-subtree) ""
                                  true node-subtree)]
                        node))
        tree-label-provider (proxy [ColumnLabelProvider]
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
                                (let [node (get-node-fn zc)
                                      result (condp = (class node)
                                               String node
                                               clojure.lang.Keyword (name node)
                                               (str node))]
                                  result))
                              (isLabelProperty [zc property]
                                nil)
                              (removeListener [listener]))
        tree-group (new-widget {:keyname :tree-viewer-test-group :widget-class Group :parent parent :styles [SWT/SHADOW_NONE] :text "TreeViewer Test"})
        tree-viewer (TreeViewer. tree-group)

        dbl-click-listener (proxy [IDoubleClickListener]
                               []
                             (doubleClick [event]
                               (let [selection (.getSelection event)
                                     source (.getSource event)
                                     tree-paths (.getPaths selection)
                                     last-path (last tree-paths)
                                     last-segment (.getLastSegment last-path)
                                     elem (-> last-segment zip/node)]
                                 (when (= clojure.lang.Keyword (class elem))
                                   (name elem))
                                 )))]
    (doto tree-group
      (.setLayout (FillLayout.)))
    (doto tree-viewer
      (.setContentProvider tree-content-provider)
      (.setLabelProvider tree-label-provider)
      (.setInput zipper-vector)
      (.addDoubleClickListener dbl-click-listener))
    (do
      (ColumnViewerToolTipSupport/enableFor tree-viewer))
    tree-group))

(defn- edit-job-table-tree-viewer
  "create a JFace TreeTable viewer for editing a job in the WF"
  [parent]
  (let [table-group (new-widget {:keyname :table-group :widget-class Group :parent parent :styles [SWT/SHADOW_ETCHED_OUT] :text "Edit Workflow Job"})
        ;; job (atom (wflow/new-job-fn "Job Name" "Prog. Exec. Loc." "Prog. Args." "Prog. Opts."))
        ttv (TreeViewer. table-group)
        ;; job (atom @gui-state/job-to-edit)
        job-fields (type-util/class-fields wfeditor.model.workflow.Job)
        job-to-edit-ref gui-state/job-to-edit
        job-cache-ref gui-state/job-editor-cache

        ;; TODO: refactor is-branch-fn and simple-zip-fn into a
        ;; separate ns
        is-branch-fn (every-pred map? (complement (partial instance? clojure.lang.IRecord)))
        simple-zip-fn (fn [simple-zip-tree] (zip/zipper is-branch-fn (comp seq second first) (fn [n cs] (let [map (if (seq n) n {n []}) k (first (first map)) vals (second (first map))] (assoc map k (concat vals (seq cs))))) simple-zip-tree))       
        
        ttv-table-fn (fn [ttv] (.getTree ttv))
        zip-elem-tag-fn (fn [zc] (:tag (zip/node zc)))
        zip-children-fn (fn [zc]
                          ;; need to return a coll of the locs of
                          ;; children nodes, not the children nodes themselves
                          (let [first-child (zip/down zc)
                                rest-children (loop [rcs []
                                                     czc (zip/right first-child)]
                                                (if-not czc
                                                  rcs
                                                  (recur (conj rcs czc) (zip/right czc))))
                                result (concat [first-child] rest-children)]
                            result))
        has-children-fn (fn [zc] (if (-?> zc zip/down zip/branch?) true false))
        expand-table-fn (fn [ttv]
                          (let [input-zip-vector (.getInput ttv)
                                z (first input-zip-vector)]
                            (when z
                              (let [elems-to-expand (loop [loc z
                                                           exp-elems []]
                                                      (if (zip/end? loc)
                                                        exp-elems
                                                        (let [elem-tag (zip-elem-tag-fn loc)
                                                              show-expanded (boolean (@gui-state/job-editor-expanded-fields elem-tag))
                                                              new-exp-elems (if (and show-expanded (has-children-fn loc))
                                                                              (conj exp-elems loc)
                                                                              exp-elems)]
                                                          (recur (zip/next loc) new-exp-elems))))]
                                ;; have to put in this try-catch form
                                ;; in order to catch Exception thrown
                                ;; only when window is closed through
                                ;; the close button
                                (try
                                  (do
                                    (.setExpandedElements ttv (to-array elems-to-expand)))
                                  (catch AssertionFailedException afe
                                    (.printStackTrace afe)))))))
        refresh-table-gui-fn (fn [ttv]
                               (.refresh ttv)
                               (dorun
                                (doseq [column (.. ttv getTree getColumns)]
                                  (.pack column)))
                               (dorun
                                (doseq [column (.. ttv getTree getColumns)]
                                  (.showColumn (.. ttv getTree) column)))
                               (.showColumn (.. ttv getTree) (first (.. ttv getTree getColumns)))
                               (.redraw (.. ttv getTree))
                               (.update (.. ttv getTree)))
        tree-content-provider (proxy [ITreeContentProvider]
                                  []
                                ;; the "content" that will be
                                ;; manipulated by the JFace tree
                                ;; viewer will be entirely closures of zippers
                                ;; located at nodes, not the actual
                                ;; node-data themselves
                                (getChildren [zc]
                                  (let [result (zip-children-fn zc)
                                        array-result (to-array result)]
                                    array-result))
                                (getElements [zipper-vector]
                                  (let [z (first zipper-vector)
                                        elements (zip-children-fn z)]
                                    (to-array elements)))
                                (getParent [zc]
                                  (let [parent-zip (zip/up zc)
                                        parent-tag (zip-elem-tag-fn parent-zip)]
                                    (if (= parent-tag :job) 
                                      nil 
                                      (zip/up zc))))
                                (hasChildren [zc]
                                  (let [has-chil (has-children-fn zc)
                                        elem-tag (zip-elem-tag-fn zc)]
                                    (condp = elem-tag
                                      :opt false
                                      :task-statuses false
                                      has-chil)))
                                (dispose [])
                                (inputChanged [viewer old-input new-input]
                                  (when-not new-input
                                    (let [empty-job (wflow/nil-job-fn)
                                          empty-job-zip (fformat/zip-from-job empty-job)] 
                                      (.setInput viewer [empty-job-zip])
                                      (dosync
                                       (ref-set job-to-edit-ref nil))))
                                  (refresh-table-gui-fn ttv)))
        arg-key-fn (fn [element]
                     (let [idx (count (zip/lefts element))]
                       (str (ui-const/JOB-FIELD-FULL-NAMES :arg) " " (inc idx))))
        arg-val-fn (fn [element]
                     (-> element zfx/text))
        opt-key-fn (fn [element]
                     (let [flag (zfx/xml1-> element :flag zfx/text)]
                       flag))
        opt-val-fn (fn [element]
                     (let [val (fformat/nil-pun-empty-str (zfx/xml1-> element :val zfx/text))]
                       (or val ui-const/NIL-VAL-STR-REP)))
        array-map-val-fn (fn [element elem-tag]
                           (when-let [array-map (get @job-cache-ref :array)]
                             (str (get array-map elem-tag))))
        elem-key-fn (fn [element]
                      (let [elem-tag (zip-elem-tag-fn element)]
                        (condp = elem-tag
                          :arg (arg-key-fn element)
                          :opt (opt-key-fn element)
                          (str (get ui-const/JOB-FIELD-FULL-NAMES elem-tag ui-const/NIL-VAL-STR-REP)))))
        elem-val-fn (fn [element]
                      (let [elem-tag (zip-elem-tag-fn element)]
                        (condp = elem-tag
                          :arg (arg-val-fn element)
                          :opt (opt-val-fn element)
                          :task-statuses (let [statuses-map (fformat/task-statuses-from-zip element)]
                                           (task-status/status-field statuses-map))
                          :start (array-map-val-fn element elem-tag)
                          :end (array-map-val-fn element elem-tag)
                          :step (array-map-val-fn element elem-tag)
                          :index-var (array-map-val-fn element elem-tag)
                          (if-let [val (or
                                        (and (#{:prog-args :prog-opts} elem-tag)
                                             (not (get @gui-state/job-editor-expanded-fields elem-tag))
                                             (let [coll (seq (get @job-cache-ref elem-tag))]
                                               (condp = elem-tag
                                                 :prog-args (exec/args-str coll)
                                                 :prog-opts (exec/opts-str coll)
                                                 nil))
                                             )
                                        (and (not (has-children-fn element))
                                             (get @job-cache-ref elem-tag)))]
                            (str val)
                            ui-const/NIL-VAL-STR-REP))))
        is-zipper-fn (fn [element]
                       ;; this fn gives an crude approximation for
                       ;; testing if a datum is a zipper or not
                       (and (vector? element) (map? (first element))))
        col-1-label-provider (proxy [StyledCellLabelProvider]
                                 []
                               ;; change of label provider from TreeViewer-wide subclass of
                               ;; ITableLabelProvider to per-TreeViewerColumn sublcass of
                               ;; StyledCellLabelProvider is explained here: http://stackoverflow.com/questions/9172543/how-to-Create-a-jfacetreeviewer-with-multi-column
                               (update [cell]
                                 (let [element (.getElement cell)
                                       elem-tag (zip-elem-tag-fn element)
                                       text (if (is-zipper-fn element)
                                              (elem-key-fn element)
                                              (ui-const/JOB-FIELD-FULL-NAMES (keyword element)))]
                                   (.setText cell text)
                                   (proxy-super update cell))))
        col-2-label-provider (proxy [StyledCellLabelProvider]
                                 []
                               ;; TODO: use info in following links to
                               ;; figure out how to make adding args
                               ;; and opts localized and easy
                               ;; http://www.vogella.com/articles/EclipseJFaceTableAdvanced/article.html
                               ;; http://llqc.wordpress.com/2010/03/20/painting-table-items-in-swtjface/
                               ;; directly set the TextStyle's font
                               ;; field as described and shown in:
                               ;; http://stackoverflow.com/questions/10430976/trying-to-add-basic-rich-text-support-via-html-to-a-jface-tableviewer
                               ;; http://bingjava.appspot.com/snippet.jsp?id=2211
                               (update [cell]
                                 (let [element (.getElement cell)
                                       elem-tag (zip-elem-tag-fn element)
                                       text (if (is-zipper-fn element)
                                              (elem-val-fn element)
                                              ui-const/NIL-VAL-STR-REP)]
                                   (.setText cell text)
                                   (proxy-super update cell))))
        col-lbl-providers [col-1-label-provider col-2-label-provider]
        col-props ["key" "value"]
        cell-editors (for [col col-props]
                       (TextCellEditor. (.. ttv getTree)))
        cell-editor (TextCellEditor. (.. ttv getTree))
        column-headings ["Job field" "Value"]
        columns (doall
                 (for [ch column-headings]
                   (new-widget {:keyname (keyword (str "col-" ch)) :widget-class TreeViewerColumn :parent ttv :styles [SWT/LEFT]})))
        edit-opt-key-fn (fn [element value]
                          (let [up-zip (zip/up element)
                                up-elem-tag (zip-elem-tag-fn up-zip)
                                key (zfx/xml1-> element :flag zfx/text)
                                keyval-seq (fformat/map-keyval-seq-from-zip up-zip up-elem-tag fformat/map-of-coll-vals-list-fn)
                                idx (count (zip/lefts element))
                                keyval (nth keyval-seq idx)
                                val-vec (get keyval key)
                                new-key (fformat/nil-pun-empty-str value)]
                            (when idx
                              (let [new-keyval (cond
                                                (and (not new-key) val-vec) keyval
                                                new-key {new-key val-vec}
                                                :default nil)
                                    new-keyval-seq (if new-keyval
                                                     (concat
                                                      (take idx keyval-seq)
                                                      [{new-key val-vec}]
                                                      (drop (inc idx) keyval-seq))
                                                     (concat
                                                      (take idx keyval-seq)
                                                      (drop (inc idx) keyval-seq)))
                                    new-prog-opts (apply merge-with fformat/merge-with-fn new-keyval-seq)]
                                (dosync
                                 (alter job-cache-ref assoc :prog-opts new-prog-opts))))))
        edit-arg-val-fn (fn [element value]
                          (let [idx (count (zip/lefts element))
                                val (fformat/nil-pun-empty-str value)]
                            (if val
                              (dosync
                               (alter job-cache-ref assoc-in [:prog-args idx] val))
                              (let [up-zip (zip/up element)
                                    up-elem-tag (zip-elem-tag-fn up-zip)
                                    args (fformat/vector-from-zip up-zip up-elem-tag)
                                    new-args (concat (subvec args 0 idx) (subvec args (inc idx)))]
                                (dosync
                                 (alter job-cache-ref assoc :prog-args new-args))))))
        edit-opt-val-fn (fn [element value]
                          (let [up-zip (zip/up element)
                                up-elem-tag (zip-elem-tag-fn up-zip)
                                key (zfx/xml1-> element :flag zfx/text)
                                keyval-seq (fformat/map-keyval-seq-from-zip up-zip up-elem-tag fformat/map-of-coll-vals-list-fn)
                                idx (count (zip/lefts element))
                                val (fformat/nil-pun-empty-str value)
                                ;; TODO: make this code handle vectors
                                ;; of more than one val (would require
                                ;; rework in other places as well, undoubtedly)
                                new-val-vec (if val [val] nil)]
                            (when idx
                              (let [new-keyval-seq (concat
                                                    (take idx keyval-seq)
                                                    [{key new-val-vec}]
                                                    (drop (inc idx) keyval-seq))
                                    new-prog-opts (apply merge-with fformat/merge-with-fn new-keyval-seq)]
                                (dosync
                                 (alter job-cache-ref assoc :prog-opts new-prog-opts))))))
        edit-array-val-fn (fn [element value]
                            (let [elem-tag (zip-elem-tag-fn element)]
                              (cond
                               (#{:start :end :step} elem-tag) (try
                                                                 (let [val-int (Integer/parseInt value)]
                                                                   (dosync
                                                                    (alter job-cache-ref assoc-in [:array elem-tag] val-int)))
                                                                 (catch NumberFormatException e))
                               :default (dosync
                                         (alter job-cache-ref assoc-in [:array elem-tag] value)))))
        edit-key-fn (fn [element value]
                      (let [elem-tag (zip-elem-tag-fn element)]
                        (cond
                         (= :opt elem-tag) (edit-opt-key-fn element value))))
        edit-val-fn (fn [element value]
                      (let [elem-tag (zip-elem-tag-fn element)]
                        (cond
                         (= :opt elem-tag) (edit-opt-val-fn element value)
                         (= :arg elem-tag) (edit-arg-val-fn element value)
                         (#{:start :end :step :index-var} elem-tag) (edit-array-val-fn element value)
                         :default (do
                                    (dosync
                                     (alter job-cache-ref assoc elem-tag value))))))
        col1-edit-support (proxy [EditingSupport]
                              [ttv]
                            (canEdit [element]
                              (let [elem-tag (zip-elem-tag-fn element)]
                                (boolean (and (= :opt elem-tag) (nil? (fformat/nil-pun-empty-str (zfx/xml1-> element :val zfx/text)))))))
                            (getCellEditor [element]
                              cell-editor)
                            (getValue [element]
                              (elem-key-fn element))
                            (setValue [element value]
                              (edit-key-fn element value)))
        col2-edit-support (proxy [EditingSupport]
                              [ttv]
                            (canEdit [element]
                              (let [elem-tag (and (not (nil? @job-cache-ref)) (zip-elem-tag-fn element))]
                                (if (or (nil? @job-cache-ref)
                                        (and (= Job (class @job-cache-ref)) (:id @job-cache-ref)))
                                  false
                                  (boolean (not (#{:id :task-statuses :prog-args :prog-opts :array} elem-tag))))))
                            (getCellEditor [element] 
                              cell-editor)
                            (getValue [element]
                              (elem-val-fn element))
                            (setValue [element value]
                              (edit-val-fn element value)))
        col-edit-supports [col1-edit-support col2-edit-support]
        tree-viewer-listener (proxy [ITreeViewerListener]
                                 []
                               (treeCollapsed [event]
                                 (let [element (.getElement event)
                                       elem-tag (zip-elem-tag-fn element)]
                                   (dosync
                                    (alter gui-state/job-editor-expanded-fields disj elem-tag))
                                   ;; TODO: optimize this, perhaps by
                                   ;; using the 
                                   (.. Display getCurrent (asyncExec (fn [] (.refresh ttv))))))
                               (treeExpanded [event]
                                 (let [element (.getElement event)
                                       elem-tag (zip-elem-tag-fn element)]
                                   (dosync
                                    (alter gui-state/job-editor-expanded-fields conj elem-tag))
                                   (.. Display getCurrent (asyncExec (fn [] (.refresh ttv)))))))]
    (doall
     (map (fn [col ch edit-supp lbl-prov]
            (.. col getColumn (setText ch))
            (.setEditingSupport col edit-supp)
            (.setLabelProvider col lbl-prov))
          columns column-headings col-edit-supports col-lbl-providers))
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
    (add-watch job-to-edit-ref :re-bind (fn [key r old new]
                                          (when-not (= @job-cache-ref new)
                                            
                                            (dosync
                                             (ref-set job-cache-ref new)))))

    (add-watch job-cache-ref :re-bind (fn [key r old new]
                                        ;; testing that old != new
                                        ;; prevents problems when
                                        ;; focus is lost from
                                        ;; textcelleditor to another
                                        ;; widget which may be stale/disposed
                                        (when (not= old new)
                                          (when (and
                                                 (and old new)
                                                 (not= new @job-to-edit-ref))
                                            (let [wf (wflow/workflow)
                                                  new-wf (wflow/replace-job wf old new)]
                                              (wflow/set-workflow new-wf)
                                              (dosync
                                               (ref-set job-to-edit-ref new))))
                                          (let [new-job (or new (wflow/nil-job-fn))
                                                new-job-zip (fformat/zip-from-job new-job)]
                                            (.setInput ttv [new-job-zip]))
                                          (.refresh ttv)
                                          (expand-table-fn ttv))))

    ;; basic display config
    (doto table-group
      (.setLayout (GridLayout. 1 false)))
    (doto ttv
      (.setContentProvider tree-content-provider)
      ;; (.setLabelProvider label-provider)
      (.setInput [(fformat/zip-from-job @job-to-edit-ref)]))
    
    ;; configs to format table display and align cols properly
    (doto (.. ttv getTree)
      ;; (.setLayoutData (GridData. GridData/FILL_BOTH))
      (.setHeaderVisible true)
      (.setLinesVisible true)
      (.setLayoutData (GridData. GridData/FILL_BOTH))
      (.setRedraw true)
      ;; don't pack table - shrinks the right margin if not needed,
      ;; looks weird
      ;; (.pack)
      )
    (dorun
     (map #(.showColumn (.. ttv getTree) %) (.. ttv getTree getColumns)))
    (dorun
     (map (memfn pack) (.. ttv getTree getColumns)))
    (dosync
     (ref-set job-to-edit-ref nil))
    (refresh-table-gui-fn ttv)
    ;; configs for control editors
    (doto ttv
      ;; into-array preserves the object type in a Java array better
      ;; than to-array
      (.setColumnProperties (into-array col-props))

      ;; TODO: uncomment, get to work with TableTreeViewer
      ;; TODO: rework CellEditor code to use the SWT example snippet
      ;; code that uses TreeViewerEditor and ColumnEditors:
      ;; http://wiki.eclipse.org/JFaceSnippets
      ;; http://git.eclipse.org/c/platform/eclipse.platform.ui.git/tree/examples/org.eclipse.jface.snippets/Eclipse%20JFace%20Snippets/org/eclipse/jface/snippets/viewers/Snippet026TreeViewerTabEditing.java
      ;; (.setCellModifier cell-modifier)
      ;; (.setCellEditors (into-array cell-editors))

      (.addTreeListener tree-viewer-listener)
      )
    ;; return value
    table-group))

(defn- edit-job-table-viewer
  "create a JFace TreeTable viewer for editing a job in the WF"
  [parent]
  (let [table-group (new-widget {:keyname :table-group :widget-class Group :parent parent :styles [SWT/SHADOW_ETCHED_OUT] :text "Edit Workflow Job"})
        ;; job (atom (wflow/new-job-fn "Job Name" "Prog. Exec. Loc." "Prog. Args." "Prog. Opts."))
        ttv (TableViewer. table-group)
        ;; job (atom @job-to-edit-ref)
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
        edit-job-table-tree-group (edit-job-table-tree-viewer comp)        
        ;; edit-job-table-group (edit-job-table-viewer comp)
        mod-group (mod-buttons-group comp)
        spacer-comp (new-widget {:keyname :spacer-comp :widget-class Composite :parent comp :styles [SWT/NONE]})
        ]
    (swt-util/stack-full-width comp {:margin 10} [
                                                  edit-job-table-tree-group
                                                  ;; edit-job-table-group
                                                  mod-group spacer-comp
                                                  ])
    comp))