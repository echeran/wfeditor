(ns wfeditor.ui.gui.zest.providers
  (:require [wfeditor.model.workflow :as wflow]
            [wfeditor.model.execution :as mexec])
  ;; need to import the Clojure defrecord, etc. (Java-interop types)
  ;; as according to
  ;; http://dbostwick.posterous.com/using-clojures-deftype-and-defrecord-and-name
  (:import
   [wfeditor.model.workflow Job Dependency]
   org.eclipse.jface.viewers.LabelProvider
   org.eclipse.zest.core.viewers.EntityConnectionData
   org.eclipse.jface.viewers.ArrayContentProvider
   [org.eclipse.zest.core.viewers IGraphEntityContentProvider IEntityStyleProvider IEntityConnectionStyleProvider]
   org.eclipse.draw2d.Label
   org.eclipse.zest.core.widgets.ZestStyles))

(defn label-provider-proxy
  "Return a proxy (anon. impl.) of a label provider for a GraphViewer of the Zest+JFace MVC setup"
  []
  (proxy [LabelProvider IEntityStyleProvider IEntityConnectionStyleProvider] []
    ;; LabelProvider methods
    (getText [element]
      (condp = (class element)
        Job (:name element)
        Dependency (:label element)
        EntityConnectionData ""
        (str "Wrong type: " (str (class element)))))
    ;; IEntityStyleProvider methods
    ;; take defaults for most methods using the values that trigger
    ;; the defaults, as according to the documentation
    (getNodeHighlightColor [entity]
      nil)
    (getBorderColor [entity]
      nil)
    (getBorderHighlightColor [entity]
      nil)
    (getBorderWidth [entity]
      -1)
    (getBackgroundColour [entity]
      nil)
    (getForegroundColour [entity]
      nil)
    (getTooltip [entity]
      (if (= (class entity) wfeditor.model.workflow.Job)
        (Label. (mexec/job-command entity))
        nil))
    (fisheyeNode [entity]
      false)
    ;; IEntityConnectionStyleProvider methods
    (getConnectionStyle [src dest]
      ZestStyles/CONNECTIONS_DIRECTED)
    (getColor [src dest]
      nil)
    (getHighlightColor [src dest]
      nil)
    (getLineWidth [src dest]
      -1)
    (selfStyleConnection [entity connection]
      nil)
    ))

(defn node-content-provider-proxy
  "Return a proxy (anon. impl.) of a content provider for a GraphViewer of the Zest+JFace MVC setup"
  []
  (proxy [ArrayContentProvider IGraphEntityContentProvider] []
    (getConnectedTo [entity]
      (condp = (class entity)
        ;; have to convert the Clojure seq into a Java array to make
        ;; the Java classes of GEF/Zest happy
        Job (to-array (wflow/dependent-upon entity))
        (RuntimeException. "Type not supported")))
    (getElements [input]
      ;; assume the input parameter is an array of the Job objects in
      ;; the canvas
      (to-array input)
      )))