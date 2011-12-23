(ns wfeditor.ui.gui.zest.providers
  (:require [wfeditor.model.workflow :as wflow])
  ;; need to import the Clojure defrecord, etc. (Java-interop types)
  ;; as according to
  ;; http://dbostwick.posterous.com/using-clojures-deftype-and-defrecord-and-name
  (:import
   [wfeditor.model.workflow Job Dependency]
   org.eclipse.jface.viewers.LabelProvider
   org.eclipse.zest.core.viewers.EntityConnectionData
   org.eclipse.jface.viewers.ArrayContentProvider
   org.eclipse.zest.core.viewers.IGraphEntityContentProvider))

(defn label-provider-proxy
  "Return a proxy (anon. impl.) of a label provider for the Zest+JFace MVC setup"
  []
  (proxy [LabelProvider] []
    (getText [element]
      (condp = (class element)
        Job (:name element)
        Dependency (:label element)
        EntityConnectionData ""
        (str "Wrong type: " (str (class element)))))))

(defn node-content-provider-proxy
  "Return a proxy (anon. impl.) of a content provider for the Zest+JFace MVC setup"
  []
  (proxy [ArrayContentProvider IGraphEntityContentProvider] []
    (getConnectedTo [entity]
      (condp = (class entity)
        ;; have to convert the Clojure seq into a Java array to make
        ;; the Java classes of GEF/Zest happy
        Job (to-array (wflow/dependent-upon entity))
        (RuntimeException. "Type not supported")))))