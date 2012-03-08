(ns wfeditor.io.file.wfeformat
  (:require [wfeditor.io.util.xml :as xml-util]
            [wfeditor.model.workflow :as wf]
            [clojure.string :as string]
            [clojure.contrib.zip-filter.xml :as zfx]
            [wfeditor.ui.gui.zest.canvas :as canvas])
  (:import [wfeditor.model.workflow Job Workflow WFInstance]
           [java.io ByteArrayInputStream]))


(def file-filter-extensions ["*.xml"])
(def file-filter-names ["Extensible Markup Language (XML) Files"])

(def format-reqd-states {:workflow :reqd, :wfinstance :reqd
                         :username :req-when-parent, :exec-domain :req-when-parent, :wf-name :req-when-parent, :wf-ver :req-when-parent, :wf-format-ver :req-when-parent, :name :req-when-parent, :prog-exec-loc :req-when-parent, :prog-args :req-when-parent, :prog-opts :req-when-parent, :flag :req-when-parent, :val :req-when-parent, :dep :req-when-parent, :task-id :req-when-parent, :status :req-when-parent
                         :meta nil, :parent nil, :parent-ver nil, :parent-file nil, :parent-hash nil, :jobs nil, :job nil, :id nil :desc nil, :prog-name nil, :prog-ver nil, :prog-exec-ver nil, :arg nil, :opt nil, :std-out-file nil, :std-err-file nil, :deps nil, :task-statuses nil, :task nil})

(def format-hierarchy {:wfinstance [:username :exec-domain :workflow], :workflow [:meta :jobs], :meta [:wf-name :wf-ver :wf-format-ver :parent], :parent [:parent-ver :parent-file :parent-hash], :jobs :job, :job [:id :name :desc :prog-name :prog-ver :prog-exec-loc :prog-exec-ver :prog-args :prog-opts :std-out-file :std-err-file :job-deps :task-statuses], :prog-args :arg, :prog-opts :opt, :opt [:flag :val], :deps :dep, :task-statuses :task, :task [:task-id :status]})

;;
;; functions to create XML from datatypes
;;


(defn- remove-fn
  "use this function to prune null/empty non-essential info being represented in a XML tree reprsentation"
  [obj]
  (or (nil? obj) (and (coll? obj) (empty? obj)) (and (string? obj) (empty? obj))))


;; TODO: create test case that asserts that all of the fields that are
;; children of the :job type in format-hierarchy are present in the
;; Job defrecord
;; TODO: create test case that asserts that the  reqd/optional
;; statuses in format-reqd-states match with new-job-fn and/or
;; pre-post condition checks

(defn- xml-subtree
  "helper method for creating XML trees to represent values stored in WFE types.
assumes that no attributes are present in any of the tags. (this is acceptable for WFE since attributes are eschewed and substituted by representing them as child elements.)"
  [tag val]
  (cond
   (remove-fn val) (if-not (format-reqd-states tag)
                     nil
                     {:tag tag :attrs nil :content []})
   (string? val) {:tag tag :attrs nil :content [val]}
   (sequential? val) {:tag tag :attrs nil :content (into [] (remove nil? (for [x val] (xml-subtree (format-hierarchy tag) x))))}
   (map? val) (let [keyval-tag (format-hierarchy tag)
                    [key-tag val-tag] (format-hierarchy keyval-tag)]
                {:tag tag :attrs nil :content
                 (into [] (remove nil? (for [[k v] val]
                                         {:tag keyval-tag :attrs nil :content
                                          [(xml-subtree key-tag k)
                                           (xml-subtree val-tag v)]})))})))

(defn- job-xml-tree
  "implementation of defmethod for xml-tree multimethod for the Job record class"
  [job]
  (let [job-keys (keys job)
        job-deps (map #(get % :name) ((:neighbors (wf/dep-graph)) job))]
    {:tag :job :attrs nil :content
     (into [] (remove nil? (concat (for [key job-keys]
                                      (let [val (get job key)] 
                                        (xml-subtree key val)))
                                   (list (xml-subtree :deps job-deps)))))}))

(defn- wf-meta-xml-tree
  "helper method for wf-xml-tree"
  [wf]
  (let [wf-get-fn (fn [key] (get wf key))
        meta-wf-tags [:wf-name :wf-ver :wf-format-ver]
        meta-wf-vals (map wf-get-fn meta-wf-tags)
        meta-parent-tags [:parent-ver :parent-file :parent-hash]
        meta-parent-vals (map wf-get-fn meta-parent-tags)
        meta-tags (concat meta-wf-tags meta-parent-tags)
        meta-vals (concat meta-wf-vals meta-parent-vals)
        or-fn (fn ([a] a) ([a b] (or a b)))]
    (when (reduce or-fn meta-vals)
      (let [new-tag-fn (fn [tag] (xml-subtree tag (wf-get-fn tag)))]
        {:tag :meta :attrs nil :content (remove nil?  [(new-tag-fn :wf-name) (new-tag-fn :wf-ver) (new-tag-fn :wf-format-ver) (when (reduce or-fn meta-parent-vals) {:tag :parent :attrs nil :content (remove nil? [(new-tag-fn :parent-ver) (new-tag-fn :parent-file) (new-tag-fn :parent-hash)])})])}))))

(defn- wf-xml-tree
  "implementation of defmethod for xml-tree multimethod for the Workflow record class"
  [wf]
  (let [meta-subtree (wf-meta-xml-tree wf)
        job-seq (wf/wf-job-seq wf)
        jobs-subtree {:tag :jobs :attrs nil :content (remove nil? (into [] (map job-xml-tree job-seq)))}]
    {:tag :workflow :attrs nil :content (remove nil? [meta-subtree jobs-subtree])}))

(defn- wfinstance-xml-tree
  "implementation of defmethod for xml-tree multimethod for the WFInstance record class"
  [wfinst]
  (let [username (:username wfinst) 
        exec-domain (:exec-domain wfinst)
        wf (:workflow wfinst)]
    {:tag :wfinstance :attrs nil :content [(xml-subtree :username username) (xml-subtree :exec-domain exec-domain) (wf-xml-tree wf)]}))

;; return an XML tree for a given object as XML trees are returned by clojure.xml/parse
(defmulti xml-tree class)
(defmethod xml-tree wfeditor.model.workflow.Job [obj] (job-xml-tree obj))
(defmethod xml-tree wfeditor.model.workflow.Workflow [obj] (wf-xml-tree obj))
(defmethod xml-tree wfeditor.model.workflow.WFInstance [obj] (wfinstance-xml-tree obj))

(defn workflow-to-string
  "return an XML string representation of the current workflow object"
  [wf]
  (xml-util/tree-to-ppxml-str (xml-tree wf)))

(defn workflow-instance-to-string
  "return an XML string representation of the current workflow instance object"
  [wfinst]
  (xml-util/tree-to-ppxml-str (xml-tree wfinst)))

;;
;; functions to create datatypes from XML
;;

(defn- nil-pun-empty-str
  "if the input is an empty string, return nil.  else, return the input val"
  [s]
  (if (and (string? s) (empty? s))
    nil
    s))

(defn- map-from-zip
  [z tag]
  "return a map of string keys -> string values created from an XML zip (z) using a tag (tag), representing the map, that has children which contain pairs of leaf tags representing the key-value pairs of the map"
  (let [keyval-tag (format-hierarchy tag)
        keyval-zip-seq (zfx/xml-> z tag keyval-tag)
        [key-tag val-tag] (format-hierarchy keyval-tag)]
    (apply merge
           (for [keyval keyval-zip-seq]
             (let [key (first (zfx/xml-> keyval key-tag zfx/text))
                   val (nil-pun-empty-str (first (zfx/xml-> keyval val-tag zfx/text)))]
               {key val})))))

(defn- vector-from-zip
  [z tag]
  "return a vector of string values created from an XML zip (z) using a tag (tag), representing the vector, that has 0+ children of leaf tags, representing the values"
  (into [] (zfx/xml-> z tag (format-hierarchy tag) zfx/text)))

(defn- scalar-from-zip
  [z tag]
  "return a scalar, of type string, created from an XML zip (z) within a child tag (tag)"
  (first (zfx/xml-> z tag zfx/text)))

(defn- deps-from-zip
  "returns the dependencies of job given a job zipper as a vector of job names"
  [z]
  (vector-from-zip z :deps))

(defn- map-to-flat-vector
  "return a vector where each key in the map is followed by its value. if function fn is provided, then it will be applied to every vector of key-val pairs"
  ([map]
     (map-to-flat-vector map identity))
  ([map fn]
      (flatten (into [] (into {} (for [[k v] map] (fn [k v])))))))

(defn- job-from-zip
  "return a new Job instance when given a XML zipper that is currently at a job node"
  [z]
  (let [fields (format-hierarchy :job)
        field-map (apply merge
                         (letfn [(field-val [field]
                                   (condp = field
                                     :prog-args (vector-from-zip z field)
                                     :prog-opts (map-from-zip z field)
                                     :task-statuses (map-from-zip z field)
                                     (scalar-from-zip z field)))]
                           (for [f fields :when (not (#{:deps} f))]
                             {f (field-val f)})))
        reqd-fields [:name :prog-exec-loc :prog-args :prog-opts]
        reqd-vals (map #(field-map %) reqd-fields)
        optional-fields (remove (set reqd-fields) fields)
        optional-field-vals (map-to-flat-vector field-map (fn [[k v]] (when ((set optional-fields) k) [k v])))]
    ;; TODO: make sure that required and optional fields match up with
    ;; format-reqd-states map above and
    ;; wfeditor.model.workflow/new-job-fn as well    
    (apply wfeditor.model.workflow/new-job-fn (concat reqd-vals optional-field-vals))))

(defn- meta-from-zip
  "return the meta info of the workflow given an XML zipper of the workflow as a map of tags to values"
  [z]
  (when-let [meta-zip (first (zfx/xml-> z :meta))]
    (let [meta-map (into {} (for [tag [:wf-name :wf-ver :wf-format-ver]] [tag (scalar-from-zip meta-zip tag)]))]
      (if-let [parent-zip (first (zfx/xml-> meta-zip :parent))]
        (let [parent-meta-map (into {} (for [tag [:parent-ver :parent-file :parent-hash]] [tag (scalar-from-zip parent-zip tag)]))]
          (merge meta-map parent-meta-map))
        meta-map))))

(defn- workflow-from-zip
  "return a workflow given an XML zipper of the workflow"
  [z]
  (let [meta-map (meta-from-zip z)]
    (loop [job-zip-seq (zfx/xml-> z :jobs :job)
           job-set #{}
           dep-name-map {}]      
      (if (empty? job-zip-seq)
        (let [graph (wf/new-graph-fn :nodes job-set :neighbors (wf/job-dep-map job-set dep-name-map))
              wf-meta-fields (remove #(= :parent %) (concat (format-hierarchy :meta) (format-hierarchy :parent)))
              wf-meta-map (into {} (map (fn [k] [k (get meta-map k)]) wf-meta-fields))
              meta-field-vals (map-to-flat-vector wf-meta-map)
              wf (apply wfeditor.model.workflow/new-workflow-fn (concat [:graph graph] meta-field-vals))]
          wf)
        (let [jz (first job-zip-seq)
              job (job-from-zip jz)
              job-name (:name job)
              job-deps (deps-from-zip jz)]
          (recur (rest job-zip-seq) (conj job-set job) (assoc dep-name-map job-name job-deps)))))))

(defn- workflow-from-file
  "return a workflow based on an input XML string representation"
  [file-name]
  (let [wf-xml-tree (xml-util/xml-file-to-tree file-name)
        wf-xml-zip (xml-util/xml-tree-to-zip wf-xml-tree)]
    (workflow-from-zip wf-xml-zip)))

(defn set-workflow
  "set the current state of the workflow.  also, update the canvas graph accordingly"
  [wf]
  (dosync
   (ref-set wf/wf wf)))

(defn- wfinstance-from-zip
  "return a workflow instance given an XML zipper of the workflow"
  [z]
  (let [username (scalar-from-zip z :username)
        exec-domain (scalar-from-zip z :exec-domain)
        wf-zip (first (zfx/xml-> z :workflow))
        wf (workflow-from-zip wf-zip)]
    (wfeditor.model.workflow/new-wfinstance-fn username exec-domain wf)))

(defn- wfinstance-from-file
  "return a workflow instance based on an input XML string representation"
  [file-name]
  (let [wfinstance-xml-tree (xml-util/xml-file-to-tree file-name)
        wfinstance-xml-zip (xml-util/xml-tree-to-zip wfinstance-xml-tree)]
    (wfinstance-from-zip wfinstance-xml-zip)))

;; TODO: clean up the __-from-__ style of methods that parse text and
;; return objects so that zipper creation is abstracted 

(defn wfinstance-from-stream
  "return a workflow instance from the input stream"
  [in-stream]
  (let [wfinstance-xml-tree (xml-util/xml-stream-to-tree in-stream)
        wfinstance-xml-zip (xml-util/xml-tree-to-zip wfinstance-xml-tree)]
    (wfinstance-from-zip wfinstance-xml-zip)))

(defn string-input-stream
  "return an input stream from a string"
  [str]
  (ByteArrayInputStream. (.getBytes str)))

;;
;; developer-friendly high-level functions for loading and saving
;; datatypes from/to XML
;;

(defn set-workflow-from-file
  "set the current state of the workflow based on an input XML string representation"
  [file-name]
  (let [wf (workflow-from-file file-name)]
    (set-workflow wf)))

(defn save-workflow-to-file
  "save (using spit) the contents of the workflow object (wf) to the file provided (file-name) or create it if it doesn't exist"
  [wf file-name]
  (spit file-name (workflow-to-string wf)))