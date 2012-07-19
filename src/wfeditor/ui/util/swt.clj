(ns wfeditor.ui.util.swt
  (:require [wfeditor.io.file.wfeformat :as fformat]
            [wfeditor.model.workflow :as wflow]
            [wfeditor.ui.state.gui :as gui-state])
  (:import
   org.eclipse.swt.SWT
   (org.eclipse.swt.events SelectionEvent SelectionAdapter)
   (org.eclipse.swt.graphics Color RGB)
   (org.eclipse.swt.widgets Display FileDialog Sash Button)
   (org.eclipse.swt.layout FormLayout FormData FormAttachment)))


(defn active-shell
  "return the active Shell of the current Display"
  []
  (.. Display getCurrent getActiveShell))


;; TODO: also, create a one-stop, new widget convenience utility to end them all
;; i that namespace
(defn get-ancestor-shell
  "return the ancestor shell of the provided widget / element, or else nil"
  [widget]
  (cond
   (isa? (class widget) org.eclipse.swt.widgets.Shell) widget
   (isa? (class widget) org.eclipse.jface.window.Window) (.getShell widget)
   :else (when-let [parent (.getParent widget)] (get-ancestor-shell parent))))

;; creating this as a convenience for all of the ns's that require
;; this ns in order to call the new-widget macro so that they will not
;; have to additionally require the wfeditor.ui.state.gui ns
(def add-widget gui-state/add-widget)


;; naming convention using asterisk at end explained in this SO post:
;; http://stackoverflow.com/questions/5082850/whats-the-convention-for-using-an-asterisk-at-the-end-of-a-function-name-in-clo
(defmacro new-widget* [widget-class parent style]
  `(new ~widget-class ~parent ~style))

(defmacro new-widget
  "create a new SWT widget of type widget-class, with given parent, and properties (incl. style) as determined by opts map.
opts map keys and values:
:styles - vector of SWT style constants for this widget
:text - a string to be added to the widget via .setText"
  ([widget-class parent opts]
      (let [{:keys [styles text]} opts]
        `(let [style# (condp = (count ~styles)
                        0 SWT/NONE
                        1 (first ~styles)
                        (apply bit-or ~styles))
               widget# (new ~widget-class ~parent style#)]
           (when ~text (.setText widget# ~text))
           widget#)))
  ([opts]
     (let [{:keys [styles text widget-class parent keyname] :or {styles [SWT/NONE] widget-class Button parent (. Display getCurrent)}} opts]
       `(let [style# (condp = (count ~styles)
                       0 SWT/NONE
                       1 (first ~styles)
                       (apply bit-or ~styles))
              widget# (new ~widget-class ~parent style#)]
          (when ~text (.setText widget# ~text))
          (wfeditor.ui.util.swt/add-widget ~parent widget# ~keyname)
          widget#))))

(def remove-widget gui-state/remove-widget)

(defmacro create-widgets-with-names [parent widget-class style names]
  `(dorun (map #(.setText (new ~widget-class ~parent ~style) %1) ~names)))

(defmacro fnify
  "take the name of a Java member method and return a Clojure function that represents that Java method taking an object and optional other values as arguments.  Use this when you do not want to be restricted by clojure.core/memfn to specifying the number of arguments that the function should accept upfront.
Note: This has compiled but never run for me (aside from test cases in an interactive REPL) without generating an exception, so use at your own risk"
  [method-name]
  `(fn [obj# & args#]
     (eval (concat (list '. obj# '~method-name) args#))))

(defn update-button
  "update an SWT Button using a map of options. options map keys and values:
:text  String of the button's text
:widget-select-fn  a fn of one argument (rep.'ing the selection event) that extends the SelectionAdapter and is given to the SelectionListener"
  [button opts]
  (let [ws-fn (:widget-select-fn opts)
        proxied-adapter-ws-fn (proxy [SelectionAdapter]
                                  []
                                (widgetSelected [event]
                                  (ws-fn event)))
        opts (assoc opts :widget-select-fn proxied-adapter-ws-fn)
        opts-fns-map {:text (memfn setText s)
                      :widget-select-fn (memfn addSelectionListener f)}
        opts-fns-reduce-fn (fn [b [k v]]
                             (if-let [b-upd-fn (opts-fns-map k)]
                               (doto b
                                 (b-upd-fn v))
                               b))
        updated-button (reduce opts-fns-reduce-fn button opts)]
    updated-button))

(defn create-color
  "create and return a Color object given the RGB values. can optionally provide a Widget at the beginning of the arg list whose Display should be used to construct the Color"
  ([r g b]
     (create-color (. Display getCurrent) r g b))
  ([widget r g b]
     (let [disp (if (= (class widget) Display)
                  widget
                  (.getDisplay (get-ancestor-shell widget)))
           rgb (new RGB r g b)
           color (new Color disp rgb)]
       color)))

(defn file-dialog-open-wf
  "create an open file dialog and set the resulting file's workflow as state"
  [parent]
  (let [fd (new-widget {:widget-class FileDialog :parent parent :styles [SWT/OPEN]})]
    (when-let [in-file-name (.open fd)]
      (fformat/set-workflow-from-file in-file-name))))

(defn file-dialog-save-as-wf
  "create a save file dialog and output state to the file selected/created by the dialog"
  [parent]
  (let [fd (new-widget {:widget-class FileDialog :parent parent :styles [SWT/SAVE]})]
    (when-let [out-file-name (.open fd)]
      (fformat/save-workflow-to-file (wflow/workflow) out-file-name))))

;; assume FormLayout of the parent widget to which the returned sash
;; will be attached
(defn sash-ify
  "create a functional sash, given a parent widget and 2 'sibling' (parent of both is same as sash), and create the layout for the parent and return the sash. an optional Ratio can be given to set the initial position of the sash"
  ([parent w1 w2]
     (sash-ify parent w1 w2 (/ 50 100)))
  ([parent w1 w2 init-pos-ratio]
     (let [sash (new-widget {:widget-class Sash :parent parent :styles [SWT/VERTICAL SWT/BORDER SWT/SMOOTH]})
           ;; have to make the style of the elements next to Sash have
           ;; BORDER so that Sash is drawn identifiably
           sash-fdata (FormData.)
           w1-fdata (FormData.)
           w2-fdata (FormData.)]
       (do
         (.setLayout parent (FormLayout.)))
       (do
         (set! (. sash-fdata top) (FormAttachment. 0 0))
         (set! (. sash-fdata bottom) (FormAttachment. 100 0))
         (let [n (numerator init-pos-ratio)
               d (denominator init-pos-ratio)]
           (set! (. sash-fdata left) (FormAttachment. (int n) (int d) 0)))
         (.setLayoutData sash sash-fdata)
         (set! (. w1-fdata top) (FormAttachment. 0 0))
         (set! (. w1-fdata bottom) (FormAttachment. 100 0))
         (set! (. w1-fdata left) (FormAttachment. 0 0))
         (set! (. w1-fdata right) (FormAttachment. sash 0))
         (.setLayoutData w1 w1-fdata)
         (set! (. w2-fdata top) (FormAttachment. 0 0))
         (set! (. w2-fdata bottom) (FormAttachment. 100 0))
         (set! (. w2-fdata left) (FormAttachment. sash 0))
         (set! (. w2-fdata right) (FormAttachment. 100 0))
         (.setLayoutData w2 w2-fdata))
       (do
         (.addSelectionListener sash (proxy [SelectionAdapter]
                                         [] ;; do not call the
                                       ;; super-class constructor w/o
                                       ;; reason to, but provide it for
                                       ;; proxy's syntax's sake
                                       (widgetSelected [event]
                                         (set! (. (^FormData . sash getLayoutData) left) (FormAttachment. 0 (. event x)))
                                         (dorun
                                          (.. sash getParent layout))))))
       sash)))

(defn stack-full-width
  "given a parent Composite (and an options map) and a sequential (list/vector) of Widget's, put them in a FormLayout where each Widget gets added in order from top to bottom. options map takes the following
:margin - uniform border in pixels inside the Composite surrounding the Widgets"
  [parent opts widgets]
  (let [{:keys [margin] :or {margin 0}} opts
        first-fdata (let [fd (FormData.)]
                      (set! (. fd top) (FormAttachment. 0 margin))
                      (set! (. fd left) (FormAttachment. 0 margin))
                      (set! (. fd right) (FormAttachment. 100 (- margin)))
                      fd)
        middle-fdatas (for [[w1 w2] (butlast (partition 2 1 widgets))]
                        (let [fd2 (FormData.)]
                          (set! (. fd2 top) (FormAttachment. w1 margin))
                          (set! (. fd2 left) (FormAttachment. 0 margin))
                          (set! (. fd2 right) (FormAttachment. 100 (- margin)))
                          fd2))
        last-fdata (let [fd (FormData.)]
                     (set! (. fd top) (FormAttachment. (last (butlast widgets)) margin))
                     (set! (. fd left) (FormAttachment. 0 margin))
                     (set! (. fd right) (FormAttachment. 100 (- margin)))
                     (set! (. fd bottom) (FormAttachment. 100 (- margin)))
                     fd)
        all-fdatas (concat [first-fdata] middle-fdatas [last-fdata])]
    (do
      (.setLayout parent (FormLayout.))
      (dorun
       (map #(.setLayoutData %1 %2) widgets all-fdatas)))))

;; (defn stack-same-width
;;   "same as stack-full-width, but make all the widgets the same width.  this is achieved by making all widgets the same width as the widest widget"
;;   [parent opts widgets]
;;   (let [bounding-comp (new-widget Composite button-group {:styles [SWT/NONE]})]
;;     (stack-full-width boundin-comp opts widgets)))