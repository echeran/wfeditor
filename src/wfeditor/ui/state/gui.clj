(ns wfeditor.ui.state.gui
  (:require [clojure.zip :as zip]
            [clojure.contrib.zip-filter :as zf]))

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

;; (defn new-swtwidget
;;   "create a new SWTWidget. opts is a map whose keys are that of the SWTWidget record type"
;;   [opts]
;;   (let [{:keys [keyname obj class children] :or {:obj nil :class java.lang.Object :keyname (keyword (if class (gensym class) (gensym))) :children []}} opts]
;;     (SWTWidget. keyname obj class children)))

;; (defn get-children-swtwidget
;;   "given an SWTWidget and the name (keyword) of a child widget, return all such children widgets"
;;   [w keyname]
;;   (let [children (:children w)        
;;         ret (filter #(= (:keyname %) keyname) children)]
;;     ret))

;; (defn get-child-swtwidget
;;   "given an SWTWidget and the name (keyword) of a child widget, return the child widget (or at least the first child with such a name)"
;;   [w keyname]
;;   (first (get-children-swtwidget w keyname)))

;; (defn get-swtwidget
;;   "given (an optional SWTWidget and) a vector of keynames that index into the gui-map of state, return the widget that is addressed by the keynames"
;;   ([keynames]
;;      (get-swtwidget @gui-map keynames))
;;   ([w keynames]
;;      (reduce get-child-swtwidget w keynames)))

;; (defn add-swtwidget
;;   "add an already-created SWTWidget to the gui-map that contains the state of the GUI according to the 'address' indicated by the vector of keynames"
;;   [swtw kns]
;;   (let [leaf-widget (get-swtwidget kns)
;;         ;; TODO: update gui-map and/or these associated functions to
;;         ;; use a zipper... can't update a nested data struture
;;         ;; functionally easily without a zipper, i think
;;         ]))


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

;;
;; ref initializations
;;

;; this map is designed to contain the entire state (and by extension,
;; a pointer to every active widget) of the GUI
;; I also wonder if this should be a zipper instead of a nested map,
;; since that is the pure-functional way of handling trees...  perhaps
;; to do at some point later, if beneficial
(def gui-map {})