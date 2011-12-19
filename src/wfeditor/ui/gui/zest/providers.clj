(ns wfeditor.ui.gui.zest.providers
  (:require wfeditor.ui.gui.zest.types)
  ;; need to import the Clojure defrecord, etc. (Java-interop types)
  ;; as according to
  ;; http://dbostwick.posterous.com/using-clojures-deftype-and-defrecord-and-name
  (:import
   [wfeditor.ui.gui.zest.types MyNode MyConnection]
   org.eclipse.jface.viewers.LabelProvider
   org.eclipse.zest.core.viewers.EntityConnectionData
   org.eclipse.jface.viewers.ArrayContentProvider
   org.eclipse.zest.core.viewers.IGraphEntityContentProvider))

(defn label-provider-proxy []
  "Return a proxy (anon. impl.) of a label provider for the Zest+JFace MVC setup"
  (proxy [LabelProvider] []
    (getText [element]
      (condp = (class element)
        MyNode (:name element)
        MyConnection (:label element)
        EntityConnectionData ""
        (str "Wrong type: " (str (class element)))))))

(defn node-content-provider-proxy []
  "Return a proxy (anon. impl.) of a content provider for the Zest+JFace MVC setup"
  (proxy [ArrayContentProvider IGraphEntityContentProvider] []
    (getConnectedTo [entity]
      (condp = (class entity)
        MyNode (to-array (:connected-to entity))
        (RuntimeException. "Type not supported")))))