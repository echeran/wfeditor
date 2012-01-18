(ns wfeditor.io.file.wfeformat
  (:require [wfeditor.io.util.xml :as xml-util]
            [wfeditor.model.workflow :as wf]
            [clojure.string :as string])
  (:import [wfeditor.model.workflow Job Graph]))


(def format-reqd-states {:workflow :reqd
                         :wf-name :req-when-parent, :wf-ver :req-when-parent, :wf-format-ver :req-when-parent, :name :req-when-parent, :prog-exec-loc :req-when-parent, :prog-args :req-when-parent, :prog-opts :req-when-parent, :flag :req-when-parent, :val :req-when-parent, :dep :req-when-parent
                         :meta nil, :parent nil, :parent-ver nil, :parent-file nil, :parent-hash nil, :jobs nil, :job nil, :desc nil, :prog-name nil, :prog-ver nil, :prog-exec-ver nil, :arg nil, :op nil, :std-out-file nil, :std-err-file nil, :job-deps nil})

(def format-hierarchy {:workflow [:meta :jobs], :meta [:wf-name :wf-ver :wf-format-ver :parent], :parent [:parent-ver :parent-file :parent-hash], :jobs :job, :job [:name :desc :prog-name :prog-ver :prog-exec-loc :prog-exec-ver :prog-args :prog-opts :std-out-file :std-err-file :job-deps], :prog-args :arg, :prog-opts :opt, :opt [:flag :val], :job-deps :dep})

(defn- remove-fn
  "use this function to prune null/empty non-essential info being represented in a XML tree reprsentation"
  [obj]
  (or (nil? obj) (and (coll? obj) (empty? obj)) (and (string? obj) (empty? obj))))

(defn xml-tree-leaf
  "create a leaf node in the XML tree if the tag has content or is required, but return nil if the node is empty and optional"
  ([tag attrs content]
     (xml-tree-leaf tag attrs content (format-reqd-states tag)))
  ([tag attrs content reqd]
      (let []
        (if (and (not reqd) (remove-fn content))
          nil
          (if (remove-fn content)
            {:tag tag :attrs nil :content []}
            {:tag tag :attrs nil :content [content]})))))

;; TODO: create test case that asserts that all of the fields that are
;; children of the :job type in format-hierarchy are present in the
;; Job defrecord
;; TODO: create test case that asserts that the  reqd/optional
;; statuses in format-reqd-states match with new-job-fn and/or
;; pre-post condition checks

(defn xml-subtree
  "helper method for creating XML trees to represent values stored in WFE types.
assumes that no attributes are present in any of the tags. (this is acceptable for WFE since attributes are eschewed and substituted by representing them as child elements.)"
  [tag val]
  (cond
   (remove-fn val) (if-not (format-reqd-states tag)
                     nil
                     {:tag tag :attrs nil :content []})
   (string? val) {:tag tag :attrs nil :content [val]}
   (vector? val) {:tag tag :attrs nil :content (into [] (remove nil? (for [x val] (xml-subtree (format-hierarchy tag) x))))}
   (map? val) (let [keyval-tag (format-hierarchy tag)
                    [key-tag val-tag] (format-hierarchy keyval-tag)]
                {:tag tag :attrs nil :content
                 (into [] (remove nil? (for [[k v] val]
                                         {:tag keyval-tag :attrs nil :content
                                          [(xml-subtree key-tag k)
                                           (xml-subtree val-tag v)]})))})))

(defn job-xml-tree
  "implementation of defmethod for xml-tree multimethod for the Job record class"
  [job]
  (let [job-keys (keys job)]
    {:tag :job :attrs nil :content
     (into [] (remove nil? (for [key job-keys]
                             (let [val (get job key)]
                               (xml-subtree key val)))))}))

(defn graph-xml-tree
  ""
  [graph])

;; return an XML tree for a given object as XML trees are returned by clojure.xml/parse
(defmulti xml-tree class)
(defmethod xml-tree wfeditor.model.workflow.Job [obj] (job-xml-tree obj))
(defmethod xml-tree wfeditor.model.workflow.Graph [obj] (graph-xml-tree obj))

(defn workflow-to-string
  "return a string reprsentation of the workflow"
  []
  (let [job-seq (wf/wf-job-seq (wf/flow-graph))]
    (println (string/join "\n--\n" (map (comp xml-util/tree-to-ppxml xml-tree) job-seq)))
    ))
