(ns wfeditor.ui.gui.zest.providers
  (:require [wfeditor.model.workflow :as wflow]
            [wfeditor.io.execution :as exec]
            [clojure.string :as string]
            [wfeditor.ui.util.swt :as swt-util]
            [wfeditor.io.util.const :as io-const])
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

;;
;; ref declarations (initial bindings below)
;;

(declare job-swt-colors)

;;
;; functions
;;

(defn- str-abbr
  "contract a string to a certain length, only showing the beginning and end parts separated by an ellipsis. can provide either just the string, or the string along with the lengths of the beginning, ellipsis, and end parts"
  ([s]
     (str-abbr s 62 3 15))
  ([s begin ellipsis end]
     (if (<= (count s) (+ begin ellipsis end))
         s
         (clojure.string/join (concat (subs s 0 begin) (take ellipsis (repeat ".")) (subs s (- (count s) end) (count s)))))))

(defn- status-tooltip-field
  "given a map of the tasks ids and statuses, return the one-line string for the tooltip giving the full status frequencies info"
  [task-status-map]
  (let [status-order [:error :failed :uncertain :killed :running :waiting :success]
        statuses (vals task-status-map)
        total (count statuses)
        freqs (frequencies statuses)
        status-freq-line-fn (fn [s] (when (freqs s) (str (name s) " (" (freqs s) "/" total ")")))
        parts (remove nil? (map status-freq-line-fn status-order))
        tooltip-line (string/join ", " parts)]
    tooltip-line))

(defn- display-status
  "given a job's task statuses map, return a single status for coloring a Job in the canvas containing the workflow graph"
  [task-status-map]
  (or (get task-status-map io-const/NON-ARRAY-JOB-TASK-ID)
      (let [statuses (vals task-status-map)
            status-order [:error :failed :uncertain :killed :running :waiting :success]]
        (loop [order status-order]
          (if (empty? order)
            nil
            (if-let [result (some #{(first order)} statuses)]
              result
              (recur (rest order))))))))

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
      ;; remember, returning nil means method is ignored (i.e.,
      ;; default value is returned)
      (when (= (class entity) wfeditor.model.workflow.Job)
        (let [status (display-status (:task-statuses entity))]
          (when status
            (let [rgb (condp = status
                        :success [0 255 0]
                        :running [255 255 0]
                        ;; using UNC blue for waiting state color, from http://en.wikipedia.org/wiki/Carolina_blue
                        :waiting [86 160 211]
                        :error [255 0 0]
                        :failed [255 0 0]
                        :uncertain [255 127 0]
                        :killed [255 0 0])
                  color (apply swt-util/create-color rgb)]
              (dosync
               (alter job-swt-colors assoc-in [entity :background] color))
              color)))))
    (getForegroundColour [entity]
      nil)
    (getTooltip [entity]
      (when (= (class entity) wfeditor.model.workflow.Job)
        (let [tooltip-field-names ["Name" "ID" "Status" "Prog. Name" "Command" "Out file" "Err file"]
              tooltip-field-vals [(:name entity) (:id entity) (status-tooltip-field (:task-statuses entity)) (:prog-name entity) (str-abbr (exec/job-command entity)) (:std-out-file entity) (:std-err-file entity)]
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
    (getRouter [src dest]
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
      ;; assume the input parameter is WF containing the Job objects in
      ;; the canvas
      ;; TODO: generalize this, as necessary, based on type of input.
      ;; regardless of type, must return coll. of Job's as the Zest Element's
      ;; (ex: if input is WFInstance, not plain WF, then do diff work
      ;; to pull out Job's)
      (to-array (:nodes (wflow/dep-graph input)))
      )))


;;
;; SWT util functions
;; 

(defn dispose-job-swt-colors
  "dispose the SWT Color objects created"
  []
  (dosync
   (doseq [job-map (vals @job-swt-colors)
           color (vals job-map)]
     (.dispose color))
   (ref-set job-swt-colors {})))

;;
;; ref initial bindings
;;

;; the ref's value is a multi-level map:
;; key 1: a Job object (record) that exists in the workflow
;; key 2: a key indicating what the color corresponds to, in
;; the set: #{:background}
;; value: a Color object (or nil) for the 
(def job-swt-colors (ref {}))