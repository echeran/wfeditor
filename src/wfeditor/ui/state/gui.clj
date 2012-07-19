(ns wfeditor.ui.state.gui
  (:require [clojure.zip :as zip]
            [clojure.contrib.zip-filter :as zf])
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

;;
;; functions
;;

(defn new-swtwidget
  "create a new SWTWidget. opts is a map whose keys are that of the SWTWidget record type"
  [opts]
  (let [{:keys [keyname obj class children] :or {:children []}} opts
        ;; can't use the built-in :or construct for map destructuring
        ;; for class and keyname ecause they depend on other
        ;; destructured fields
        class (or class (if obj
                          (class obj)
                          java.lang.Object))
        keyname (or keyname (keyword
                             (if obj
                               (gensym (.getSimpleName class))
                               (gensym))))]
    (SWTWidget. keyname obj class children)))

;;
;; functions based on clojure.zip
;;

(defn gui-zip
  [root]
  (zip/zipper (complement string?)
              (comp seq :children)
              (fn [node children]
                  (assoc node :children (and children (apply vector children))))
              root))

;;
;; functions based on clojure.contrib.zip-filter.xml
;;

(defn keyname=
  "Returns a query predicate that matches a node when its is a tag
named tagname."
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

There are also special predicates: keywords are converted to tag=,
strings to text=, and vectors to sub-queries that return true if
they match.

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
     (let [gm @gui-map
           p-addr (get (lookup-map gm) parent)
           gz (gui-zip gm)
           pz (apply gui1-> gz (rest p-addr))
           new-swtw (new-swtwidget {:keyname keyname :obj widget :class (class widget) :children []})
           edit-add-fn (fn [n w] (update-in n [:children] conj w))
           new-map-zip (zip/edit pz edit-add-fn new-swtw)
           new-map (zip/root new-map-zip)]
       (dosync
        (ref-set gui-map new-map)))))

;;
;; ref initializations
;;

;; this map is designed to contain the entire state (and by extension,
;; a pointer to every active widget) of the GUI
;; I also wonder if this should be a zipper instead of a nested map,
;; since that is the pure-functional way of handling trees...  perhaps
;; to do at some point later, if beneficial

(def gui-map (ref {}))