(ns wfeditor.ui.gui.zest.providers
  (:require [wfeditor.model.workflow :as wflow]
            [wfeditor.io.execution :as exec]
            [clojure.string :as string])
  ;; need to import the Clojure defrecord, etc. (Java-interop types)
  ;; as according to
  ;; http://dbostwick.posterous.com/using-clojures-deftype-and-defrecord-and-name
  (:import
   [wfeditor.model.workflow Job]
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
      (when (= (class entity) wfeditor.model.workflow.Job)
        (let [tooltip-field-names ["Name" "ID" "Prog. Name" "Command" "Out file" "Err file"]
              tooltip-field-vals [(:name entity) (:id entity) (:prog-name entity) (exec/job-command entity) (:std-out-file entity) (:std-err-file entity)]
              tooltip-field-fn (fn [name val] (let [pr-val (or val "")] (str name ": " pr-val)))
              tooltip-string-parts (map tooltip-field-fn tooltip-field-names tooltip-field-vals)
              tooltip-string (string/join "\n" tooltip-string-parts)]
          (Label. tooltip-string))
        ))
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