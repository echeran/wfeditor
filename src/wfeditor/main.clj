(ns wfeditor.main)


;; (import 'org.eclipse.swt.SWT
;;        'org.eclipse.swt.graphics.Color
;;        'org.eclipse.swt.layout.FillLayout
;;        'org.eclipse.swt.widgets.Display
;;        'org.eclipse.swt.widgets.Label
;;        'org.eclipse.swt.widgets.Shell)

;; (let [display (Display.)
;;      shell (Shell. display)
;;      label (Label. shell SWT/CENTER)
;;      red (Color. nil 255 0 0)]
;; (doto shell
;;  (.setText "Hello World")
;;  (.setBounds 100 100 200 50)
;;  (.setLayout (FillLayout.)))

;; (doto label
;;  (.setText "Hello World")
;;  (.setForeground red))

;; (. shell open)
;; (while (not (. shell isDisposed))
;;  (if (not (. display readAndDispatch)) (. display sleep)))

;; (. red dispose)
;; (. display dispose))



(defstruct person :first :last :age :child)

(def example
     (map #(apply (partial struct person)  %)
           (partition 4 ["Dan" "Rubel" 38 [(struct person "Beth" "Rubel" 8)
                                           (struct person "David" "Rubel" 3)]
                         "Eric" "Clayberg" 39 [(struct person "Lauren" "Clayberg" 6)
                                               (struct person "Lee" "Clayberg" 4)]
                         "Mike" "Taylor" 52 nil])))

(import '(org.eclipse.swt.widgets
          Display Shell Table TableColumn)
        'org.eclipse.swt.SWT
        'org.eclipse.swt.layout.FillLayout
        '(org.eclipse.jface.viewers
          ArrayContentProvider TableViewer LabelProvider ITableLabelProvider))

(let [display (Display.)
      shell (doto (Shell. display)
              (.setText "Table Viewer Example")
              (.setBounds 100 100 325 200)
              (.setLayout (FillLayout.)))
      tableViewer (TableViewer.  shell (bit-or SWT/SINGLE SWT/FULL_SELECTION))
      table (. tableViewer getTable)]

  (. table setHeaderVisible true)
  (. table setLinesVisible true)

  (doall (map #(doto (TableColumn. table %1) (.setText %2) (.setWidth %3))
                [SWT/LEFT SWT/LEFT SWT/CENTER SWT/CENTER]
               ["First Name" "Last Name" "Age" "Num Children"]
                [100 100 35 75]))

  (. tableViewer setLabelProvider
                 (proxy [LabelProvider ITableLabelProvider] []
                    (getColumnImage [_1 _2] nil)
                   (getColumnText [element idx]
                     (str (cond (= idx 0) (:first element)
                                 (= idx 1) (:last element)
                                 (= idx 2) (:age element)
                                 (= idx 3) (count (:child element)))))))

  (. tableViewer setContentProvider (ArrayContentProvider.))
  (. tableViewer setInput (to-array example))

  (. shell open)
  (while (not (. shell isDisposed))
    (if (not (. display readAndDispatch)) (. display sleep)))
  (. display dispose))