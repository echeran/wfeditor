(ns wfeditor.io.util.xml
  (:require [clojure.xml :as xml]
            [clojure.zip :as zip]
            [clojure.contrib.zip-filter.xml :as zfx]))

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

(def parse-as-zip
  "composes the work of parsing of an input XML stream into a tree, and turning that tree into a zipper"
  (comp zip/xml-zip xml/parse))

(defn pprint-xml-zip
  "pretty-prints XML from an XML zipper"
  [xmlzipper]
  (ppxml (zfx/xml-> xmlzipper)))