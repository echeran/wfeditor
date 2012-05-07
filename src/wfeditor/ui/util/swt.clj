(ns wfeditor.ui.util.swt)

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



;; (defmacro fnify
;;   "take the name of a Java member method and return a Clojure function that represents that Java method taking an object and optional other values as arguments"
;;   [method-name]
;;   `(fn [obj & args]
;;     ~(into `(. obj ~method-name) `args)))

;; this works
;; (defmacro test3 [obj & args] `(.charAt ~obj ~@args))
;; (test3 "hello world" 0)
;; ;=> \h

;; this works
;; (defmacro test5 [method obj & args] `(. ~obj ~method ~@args))
;; (test5 charAt "hello world" 0)
;; ;=> \h

;; almost works
;; (defmacro test6 [method] `(fn [obj# & args#] (~concat (. obj# ~method) args#)))
;; ((test6 charAt) "hello world" 0)
;; ; IllegalArgumentException No matching field found: charAt for class java.lang.String  clojure.lang.Reflector.getInstanceField (Reflector.java:289)

;; works, albeit potentially ugly
;; (defmacro test7 [method] `(fn [obj# & args#] (eval (concat (list '. obj# '~method) args#))))
;; ((test7 charAt) "hello world" 2)
;; ;=> \l

;; doesn't work
;; (defmacro test8 [method] (fn [obj# & args#] (concat `(. obj# ~method) args#)))
;; ((test8 charAt) "hello world" 2)
;; ; IllegalArgumentException No matching ctor found for class user$test8$fn__1054  clojure.lang.Reflector.invokeConstructor (Reflector.java:166)

;; TODO: figure out if this is the best way to achieve function-ifying
;; a Java method
(defmacro fnify
  "take the name of a Java member method and return a Clojure function that represents that Java method taking an object and optional other values as arguments"
  [method-name]
  `(fn [obj# & args#]
     (eval (concat (list '. obj# '~method) args#))))

(defn update-button
  "update an SWT Button using a map of options. options map keys and values:
:text  String of the button's text
:select-fn  a fn of one argument (rep.'ing the selection event) that extends the SelectionAdapter and is given to the SelectionListener"
  [button opts]
  (let [opts-fns-map {:text .setText :select-fn .addSelectionListener}
        ;; opts-fns-map-fn (fn [k v] (if k (fn [b] ((opts-fns-map k) b v)) identity))
        opts-fns-reduce-fn (f [b [k v]] (if k ((opts-fns-map k) b v) b))
        updated-button (reduce opts-fns-reduce-fn button opts-fns-map)]
    updated-button))