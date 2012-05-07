(ns wfeditor.ui.util.swt
  (:import
   (org.eclipse.swt.events SelectionEvent SelectionAdapter)))

(defmacro new-widget [widget-class parent style]
  `(do (new ~widget-class ~parent ~style)))

(defmacro create-widgets-with-names [parent widget-class style names]
  `(dorun (map #(.setText (new ~widget-class ~parent ~style) %1) ~names)))

;; TODO: also, create a one-stop, new widget convenience utility to end them all
;; i that namespace
(defn get-ancestor-shell
  "return the ancestor shell of the provided widget / element, or else nil"
  [widget]
  (cond
   (isa? (class widget) org.eclipse.swt.widgets.Shell) widget
   (isa? (class widget) org.eclipse.jface.window.Window) (.getShell widget)
   :else (when-let [parent (.getParent widget)] (get-ancestor-shell parent))))

(defmacro fnify
  "take the name of a Java member method and return a Clojure function that represents that Java method taking an object and optional other values as arguments.  Use this when you do not want to be restricted by clojure.core/memfn to specifying the number of arguments that the function should accept upfront.
Note: This has compiled but never run for me without generating an exception, so use at your own risk"
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
                             (let [b-upd-fn (opts-fns-map k)]
                               (doto b
                                 (b-upd-fn v))))
        updated-button (reduce opts-fns-reduce-fn button opts)]
    updated-button))