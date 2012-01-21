(ns wfeditor.model.util.type)

(defn class-fields
  "returns a best-guess effort sequence of the fields in a Clojure record type, given the class as input. underscores are translated to hyphens"
  [c]
  ;; info on getting fields from a Java class from
  ;; http://tech.puredanger.com/2010/08/08/learning-clojure-class-reference/
  (let [all-fields (seq (.getFields c))
        java-fields (remove nil? (map (comp (fn [s] (if (re-seq #"java.lang.Object" s) s)) #(.toString %)) all-fields))
        user-fields (remove #(re-seq #"__" %) java-fields)
        field-full-names (map last (map #(clojure.string/split % #"\s+") user-fields))
        field-names (map last (map #(clojure.string/split % #"\.") field-full-names))
        hyphen-trans-names (map #(clojure.string/replace % "_" "-") field-names)]
    hyphen-trans-names))