(ns wfeditor.ui.gui.editor-left
  (:require [wfeditor.model.workflow :as wflow]
            [wfeditor.ui.gui.left.edit-tab :as edit-tab]
            [wfeditor.ui.gui.left.general-tab :as general-tab])
  (:use [wfeditor.ui.util.swt :as swt-util])
  (:import
   org.eclipse.swt.SWT
   (org.eclipse.swt.layout FillLayout RowLayout GridLayout GridData FormLayout FormData FormAttachment)
   (org.eclipse.swt.widgets Label Button FileDialog Group Text Combo Composite Display TableColumn Table TableItem)
   (org.eclipse.swt.events SelectionEvent SelectionAdapter ModifyListener ModifyEvent)
   (org.eclipse.jface.viewers TreeViewer ITreeContentProvider ILabelProvider IDoubleClickListener TableViewer IStructuredContentProvider ITableLabelProvider ListViewer ICellModifier TextCellEditor ViewerSorter ColumnLabelProvider ColumnViewerToolTipSupport)
   java.net.URL
   (org.eclipse.swt.custom CTabFolder CTabItem)
   (org.eclipse.jface.layout TableColumnLayout)
   wfeditor.model.workflow.Job))




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
      (.setControl gen-tab (general-tab/general-ctab-content tab-folder))
      (.setControl edit-wf-tab (edit-tab/edit-wf-ctab-content tab-folder)))
    tab-folder))

