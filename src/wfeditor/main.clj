(ns wfeditor.main)

(import 'org.eclipse.swt.SWT
       'org.eclipse.swt.graphics.Color
       'org.eclipse.swt.layout.FillLayout
       'org.eclipse.swt.widgets.Display
       'org.eclipse.swt.widgets.Label
       'org.eclipse.swt.widgets.Shell)

(let [display (Display.)
     shell (Shell. display)
     label (Label. shell SWT/CENTER)
     red (Color. nil 255 0 0)]
(doto shell
 (.setText "Hello World")
 (.setBounds 100 100 200 50)
 (.setLayout (FillLayout.)))

(doto label
 (.setText "Hello World")
 (.setForeground red))

(. shell open)
(while (not (. shell isDisposed))
 (if (not (. display readAndDispatch)) (. display sleep)))

(. red dispose)
(. display dispose))

(println "Hallo, Welt!")