(ns wfeditor.main
  ;; putting the import statement in the ns form is the preferred way
  ;; of importing Java classes in Clojure (see http://pragprog.com/magazines/2010-11/getting-clojure)
  (:import
  org.eclipse.jface.window.ApplicationWindow
  org.eclipse.swt.SWT
  (org.eclipse.swt.layout FillLayout FormLayout FormData FormAttachment)
  (org.eclipse.swt.widgets Display Shell Label Button Sash Composite)
  org.eclipse.swt.events.SelectionEvent
  org.eclipse.swt.events.SelectionAdapter))

(defmacro new-widget [widget-class parent style]
  `(do (new ~widget-class ~parent ~style)))

(defmacro create-widgets-with-names [parent widget-class style names]
  `(dorun (map #(.setText (new ~widget-class ~parent ~style) %1) ~names))
  )

(defn create-buttons-with-names [parent  style names]
  (dorun (map #(.setText (Button. parent style) %1) names))
  )

(defn ui-editor-left-create [parent]
  (let [label2 (Label. parent  SWT/CENTER)]
    (do
      (.setLayout parent (FillLayout. SWT/VERTICAL)))
    (doto label2
      (.setText "Hello, World")
      (.setBounds (.getClientArea parent)))
    (create-widgets-with-names parent Button SWT/PUSH ["one" "two" "three"])))

(defn ui-editor-right-create [parent]
  (let [label1 (new Label parent SWT/RIGHT)]
    (do
      (.setLayout parent (FillLayout. SWT/VERTICAL))
      (create-widgets-with-names parent Button SWT/RADIO ["Radio 1" "Radio 2" "Radio 3"])
      (create-widgets-with-names parent Button SWT/TOGGLE ["Tog 1" "Tog 2" "Tog 3"])
      (create-widgets-with-names parent Button SWT/CHECK [ "Check one" "...two" "...three"]))
    (doto label1
      (.setText "hw 2.1"))))

;; assume FormLayout of the parent widget to which the returned sash
;; will be attached
(defn ui-editor-sash [parent]
  (let [sash (Sash. parent SWT/VERTICAL)
        data (FormData.)]
    (do
      (set! (. data top) (FormAttachment. 0 0))
      (set! (. data bottom) (FormAttachment. 100 0))
      (set! (. data left) (FormAttachment. 50 0))
      (.setLayoutData sash data))
    sash))

;; (defn create-sash [parent left right]
;;   (let [sash (Sash. parent SWT/VERTICAL)
;;         sash-fdata (FormData.)
;;         left-fdata (FormData.)
;;         right-fdata (FormData.)]
;;     (do
;;       (set! (. sash-fdata top) (FormAttachment. 0 0))
;;       (set! (. sash-fdata bottom) (FormAttachment. 100 0))
;;       (set! (. sash-fdata left) (FormAttachment. 50 0))
;;       (.setLayoutData sash sash-fdata))
;;     (do
;;       (set! (. left-fdata top) (FormAttachment. 0 0))
;;       (set! (. left-fdata bottom) (FormAttachment. 100 0))
;;       (set! (. left-fdata left) (FormAttachment. 0 0))
;;       (set! (. left-fdata right) (FormAttachment. sash 0))
;;       (.setLayoutData left left-fdata))
;;     (do
;;       (set! (. right-fdata top) (FormAttachment. 0 0))
;;       (set! (. right-fdata bottom) (FormAttachment. 100 0))
;;       (set! (. right-fdata left) (FormAttachment. sash 0))
;;       (set! (. right-fdata right) (FormAttachment. 100 0))
;;       (.setLayoutData right right-fdata))
;;     sash))

(defn ui-editor-create [parent]
  (let [
        sash (Sash. parent (bit-or SWT/VERTICAL SWT/BORDER SWT/SMOOTH))
        ;; have to make the style of the elements next to Sash have
        ;; BORDER so that Sash is drawn identifiably
        comp-left (Composite. parent SWT/BORDER)
        comp-right (Composite. parent SWT/BORDER)
        ;; sash (create-sash parent comp-left comp-right)
        sash-fdata (FormData.)
        comp-left-fdata (FormData.)
        comp-right-fdata (FormData.)
        ]
    (do
      (.setLayout parent (FormLayout.))
      (ui-editor-left-create comp-left)
      (ui-editor-right-create comp-right))
    (do
      (set! (. sash-fdata top) (FormAttachment. 0 0))
      (set! (. sash-fdata bottom) (FormAttachment. 100 0))
      (set! (. sash-fdata left) (FormAttachment. 50 0))
      (.setLayoutData sash sash-fdata))
    (do
      (set! (. comp-left-fdata top) (FormAttachment. 0 0))
      (set! (. comp-left-fdata bottom) (FormAttachment. 100 0))
      (set! (. comp-left-fdata left) (FormAttachment. 0 0))
      (set! (. comp-left-fdata right) (FormAttachment. sash 0))
      (.setLayoutData comp-left comp-left-fdata))
    (do
      (set! (. comp-right-fdata top) (FormAttachment. 0 0))
      (set! (. comp-right-fdata bottom) (FormAttachment. 100 0))
      (set! (. comp-right-fdata left) (FormAttachment. sash 0))
      (set! (. comp-right-fdata right) (FormAttachment. 100 0))
      (.setLayoutData comp-right comp-right-fdata))
    (do
      (.addSelectionListener sash (proxy [SelectionAdapter]
                                      [] ;; do not call the
                                    ;; super-class constructor w/o
                                    ;; reason to, but provide it for
                                    ;; proxy's syntax's sake
                                    (widgetSelected [event]
                                      (set! (. (^FormData . sash getLayoutData) left) (FormAttachment. 0 (. event x)))
                                      (dorun
                                        (.. sash getParent layout)))
                                      )))))

;; JFace way of creating a window is to subclass ApplicationWindow and
;; override createContents
;; This is the Clojure way of "subclassing", i.e., extending a class
;; via the proxy macro.  This function just returns the instance of
;; the anonymous class generated by proxy
(defn app-win-proxy []
  (proxy [ApplicationWindow]
      [nil]  ;; calling the ApplicationWindow constructor
    ;; note, should not implement any subclass constructor, as
    ;; explained in
    ;; http://pragprog.com/magazines/2010-11/getting-clojure
    ;; Instead, any code that would go in the subclass construtor
    ;; should be applied to the instance of the extended class
    ;; returned by proxy
    (createContents [parent]
      (ui-editor-create parent))))

;; The JFace idiomatic way of displaying a window.  As it seems, using
;; the "plain SWT" idiom for displaying a window doesn't work for
;; situations like a simple Label attached to the Shell, in the
;; following way: no Label widgets are painted on the Shell until the
;; user resizes the Shell.  The JFace style of doing things works
;; properly as expected.
;; If there is any code that should've gone in what would've been a
;; subclass constructor, that should go somewhere here, where the
;; instance is actually being returned and manipulated
(let [app-win (app-win-proxy)]
  (. app-win setBlockOnOpen true)
  (. app-win open)
  (.dispose (. Display getCurrent))
  )
