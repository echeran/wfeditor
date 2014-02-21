(ns wfeditor.ui.state.gui
  (:require [clojure.zip :as zip]
            [clojure.contrib.zip-filter :as zf]
            [wfeditor.model.workflow :as wflow])
  (:import org.eclipse.swt.SWT
           [org.eclipse.swt.widgets Display Shell Button Composite]))

;;
;; records
;;

;; the node type for each element in the gui-map nested-map/tree.
;;modeled after the Clojure XML tree type, so it is assumed that
;;XML-related libs (e.g., those in zip-filter.xml) might apply similarly
;;
;; keyname - a keyword that is used as the name as well as for indexing
;; class - the class of the object
;; children - a vector of the children of this object. it may be that all child
;;objects may exist, or only a subset of all child objects which are of interest
(defrecord SWTWidget [keyname obj class children])

;;
;; refs (declarations here, initial bindings below)
;;

(declare gui-map)

(declare job-to-edit)

(declare job-editor-cache)

(declare job-editor-expanded-fields)

;;
;; functions
;;

(defn new-swtwidget
  "create a new SWTWidget. opts is a map whose keys are that of the SWTWidget record type"
  [opts]
  (let [{:keys [keyname obj class children] :or {:children []}} opts
        ;; can't use the built-in :or construct for map destructuring
        ;; for class and keyname because they depend on other
        ;; destructured fields
        class (or class (if obj
                          (class obj)
                          java.lang.Object))
        keyname (or keyname (keyword
                             (if obj
                               (gensym (.getSimpleName class))
                               (gensym))))]
    (SWTWidget. keyname obj class children)))


(defn gui-zip
  "create a zip from a nested map/structure of SWTWidget types. based on xml-zip from clojure.zip"
  [root]
  (zip/zipper (complement string?)
              (comp seq :children)
              (fn [node children]
                  (assoc node :children (and children (apply vector children))))
              root))

(defn keyname=
  "Returns a query predicate that matches a node when its is a tag
named tagname. based on fns from clojure.contrib.zip-filter.xml"
  [keyname]
    (fn [loc]
      (filter #(and (zip/branch? %) (= keyname (get (zip/node %) :keyname)))
              (if (zf/auto? loc)
                (zf/children-auto loc)
                (list (zf/auto true loc))))))

(defn gui->
  "The loc is passed to the first predicate. If the predicate returns
a collection, each value of the collection is passed to the next
predicate. If it returns a location, the location is passed to the
next predicate. If it returns true, the input location is passed to
the next predicate. If it returns false or nil, the next predicate
is not called.

This process is repeated, passing the processed results of each
predicate to the next predicate. xml-> returns the final sequence.
The entire chain is evaluated lazily.

There are also special predicates: keywords are converted to keyname=

See the footer of zip-query.clj for examples."
  [loc & preds]
    (zf/mapcat-chain loc preds
                     #(cond (keyword? %) (keyname= %)
                            ;; (string? %) (text= %)
                            ;; (vector? %) (seq-test %)
                            )))

(defn gui1->
  "Returns the first item from loc based on the query predicates
given. See gui->"
  [loc & preds] (first (apply gui-> loc preds)))

(defn get-widget-zip
  "Returns the zipper that is located in the gui-map as specified by the predicates gives as arguments. see gui1->/gui->. Will try to gracefully handle if a swtw-zip-path sequential is supplied instead of predicates"
  [& preds]
  (let [gz (gui-zip @gui-map)
        wz (if (and (= 1 (count preds)) (sequential? (first preds)))
                 (apply gui1-> gz (rest (first preds)))
                 (apply gui1-> gz preds))]
    wz))

(defn get-widget
  "Returns the widget as specified from get-widget-zip (same function arguments apply)"
  [& preds]
  (let [wz (apply get-widget-zip preds)
        widget (:obj (zip/node wz))]
    widget))

(defn swtw-zip-path
  "given a zipper location of the gui-map, return an ordered list of keynames that address the node pointed to by the zipper location.
Note: when using this address with gui-> and gui1->, the first element of the address should be dropped"
  [z]
  (if-let [path-seq (zip/path z)]
    (concat (map :keyname path-seq) [(:keyname (zip/node z))])
    (list (:keyname (zip/node z)))))

(defn lookup-map
  "create a lookup map where, given a widget object (reference), will return an 'address' for the object in the gui map as given by swtw-zip-path"
  ([]
     (lookup-map @gui-map))
  ([gmap]
     (let [gz (gui-zip gmap)]
       (into {}
             (loop [loc gz
                    ret []]
               (if (zip/end? loc)
                 ret
                 (let [swtwidget (zip/node loc)]
                   (recur (zip/next loc) (conj ret [(:obj swtwidget) (swtw-zip-path loc)])))))))))

(defn add-widget
  "given the parent object, add a new widget to the gui-map"
  ([parent widget]
     (add-widget parent widget nil))
  ([parent widget keyname]
     (dosync
      (let [gm @gui-map
            p-addr (get (lookup-map gm) parent)
            gz (gui-zip gm)
            pz (apply gui1-> gz (rest p-addr))
            new-swtw (new-swtwidget {:keyname keyname :obj widget :class (class widget) :children []})
            edit-add-fn (fn [n w] (update-in n [:children] conj w))
            new-map-zip (zip/edit pz edit-add-fn new-swtw)
            new-map (zip/root new-map-zip)]
        (ref-set gui-map new-map)))))

(defn remove-widget
  "given a widget in the gui map, remove it from the gui-map. the remove operation also removes all descendants in the gui-map"
  [widget]
  (dosync
   (let [gm gui-map]
     (when-let [w-addr (get (lookup-map gm) widget)]
       (let [gz (gui-zip gm)
             wz (gui1-> gz (rest w-addr))
             new-map-zip (zip/remove wz)
             new-map (zip/root new-map-zip)]
         (ref-set gui-map new-map))))))

(defn initialize-gui-map
  "given a 'root' widget for the gui-map, initialize gui-map with that widget. widget is supplied in the opts map, which takes the same keys as new-swtwidget"
  [opts]
  (let [swtw (new-swtwidget opts)]
    (dosync
     (ref-set gui-map swtw))))

;;
;; ref initializations
;;

;; this map is designed to contain the entire state (and by extension,
;; a pointer to every active widget) of the GUI
;; the functional way of handling nested/tree structures uses zippers
;; for manipulation but still assumes (?) that changes are preserved
;; in original data structure
;; http://stackoverflow.com/questions/2872921/insertions-into-zipper-trees-on-xml-files-in-clojure
;; if breadth-first search (BFS) is ever needed, this is the best way
;; to do it for zippers afaict
;; http://stackoverflow.com/questions/11409140/stumped-with-functional-breadth-first-tree-traversal-in-clojure
;; this is the way to traverse a zipper entirely http://www.ibm.com/developerworks/java/library/j-treevisit/index.html
(def gui-map (ref {}))

;; the Job object that is edited in the table in the left nav pane
;; (def job-to-edit (ref (wflow/new-job-fn "<Job Name>" "<Prog. Exec. Loc.>" ["<Prog. Args.>"] {"<Prog. Opts.>" nil})))
(def job-to-edit (ref (wflow/nil-job-fn)))

(def job-editor-cache (ref @job-to-edit))


;; the Job object that is edited/created when the Add Job button is
;; clicked

(def creator-job (ref (wflow/new-job-fn "<Job Name>" "<Prog. Exec. Loc.>" ["<Prog. Args.>"] {"<Prog. Opts.>" nil})))

(def creator-job-cache (ref @creator-job))



;; ;; the Job object that is edited in the table in the left nav pane

(def job-editor-expanded-fields (ref #{}))