(ns wfeditor.io.util.xml
  (:require [clojure.xml :as xml]
            [clojure.zip :as zip]
            ;; [clojure.contrib.zip-filter.xml :as zfx]
            [clojure.contrib.lazy-xml :as lxml]
            ))

(defn ppxml
  "pretty-prints an input xml string (i.e., reformats with indentation)
taken from http://nakkaya.com/2010/03/27/pretty-printing-xml-with-clojure/"
  [xml]
  (let [in (javax.xml.transform.stream.StreamSource.
            (java.io.StringReader. xml))
        writer (java.io.StringWriter.)
        out (javax.xml.transform.stream.StreamResult. writer)
        transformer (.newTransformer 
                     (javax.xml.transform.TransformerFactory/newInstance))]
    (.setOutputProperty transformer javax.xml.transform.OutputKeys/INDENT "yes")
    (.setOutputProperty transformer "{http://xml.apache.org/xslt}indent-amount" "2")
    (.setOutputProperty transformer javax.xml.transform.OutputKeys/METHOD "xml")
    (.transform transformer in out)
    (-> out .getWriter .toString)))


(defn tree-to-xml-str
  "convenience function to 'compose' creating an XML stream out of an XML tree, and capturing the stream (by default, goes to std. out.) to a string. this cannot be done with the comp function because with-out-str is a macro"
  [xml-tree]
  (with-out-str (lxml/emit xml-tree)))

(defn tree-to-ppxml-str
  "pretty-print XML from an XML tree"
  [xml-tree]
  (ppxml (tree-to-xml-str xml-tree)))

(def xml-file-to-tree xml/parse)

(def xml-tree-to-zip zip/xml-zip)