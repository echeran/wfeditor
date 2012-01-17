(ns wfeditor.io.file.wfeformat
  (:require [wfeditor.io.util.xml :as xml-util]
            [wfeditor.model.workflow :as wf]
            [clojure.string :as string]))

(defn job-str
  "return a string representation of a job"
  [job]
  (let [job-keys (keys job)
        print-remove-fn (fn [obj] (or (nil? obj) (and (coll? obj) (empty? obj)) (and (string? obj) (empty? obj))))]
    (string/join "\n"
                 (remove print-remove-fn
                         (for [key job-keys]
                           (let [key-str (name key)
                                 val (condp = key
                                       :deps (into [] (map #(get % :name) (wf/depends-upon job)))
                                       (get job key))]
                             (when-not (print-remove-fn val)
                               (str key-str ": " val))))))))

(defn workflow-to-string
  "return a string reprsentation of the workflow"
  []
  (let [wfseq (wf/wf-job-seq (wf/flow-graph))
        job-str-seq (map job-str wfseq)]
    (string/join "\n" job-str-seq)))
