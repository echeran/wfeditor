(ns wfeditor.io.util.xml
  (:require [clojure.xml :as xml]
            [clojure.zip :as zip]
            [clojure.contrib.lazy-xml :as lxml])
  (:import org.apache.commons.lang.StringEscapeUtils))

(defn ppxml
  "pretty-prints an input xml string (i.e., reformats with indentation)
borrowed from http://nakkaya.com/2010/03/27/pretty-printing-xml-with-clojure/ which is the Clojure version of Java SDK code (as shown on this SO page http://stackoverflow.com/a/4472580)"
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

(def xml-stream-to-tree xml/parse)

(def xml-tree-to-zip zip/xml-zip)

(defn unescaped-xml-zip
  "returns an XML but modifies all text so that any XML-encoded elements of the string (e.g., escaped characters) are decoded.
Note: this method was created in order to unescape the XML-escaped characters (e.g., < > &) that may appear in the files of saved workflows. This phenomenon is the subject of this SO post: http://stackoverflow.com/questions/1059854/how-do-you-prevent-a-javax-transformer-from-escaping-whitespace.
However, note: 1. the escaping of characters in XML should happen, according to this: and  So users just need to be aware that if they edit something in text, load it, and save it using the GUI, they will see punctation (especially for complex Unix commands) get escaped in the saved file.  This should not effect execution, however.   2. xml/parse seems to handle the unescaping, meaning that this method is most likely superfluous."
  [xz]
  (loop [loc xz]
    (if (zip/end? loc)
      (zip/root loc)
      (recur
       (zip/next
        (if (string? (zip/node loc))
          (zip/replace loc (StringEscapeUtils/unescapeXml (zip/node loc)))
          loc))))))