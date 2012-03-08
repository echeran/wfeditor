(ns wfeditor.io.relay.server-views.welcome
  (:require [wfeditor.io.relay.server-views.common :as common]
            [wfeditor.io.file.wfeformat :as fformat]
            [wfeditor.io.execution :as exec]
            [wfeditor.model.workflow :as wflow]
            [noir.content.getting-started]
            [noir.response :as resp]
            [noir.validation :as vali]
            [clojure.java.io])
  (:use [noir.core :only [defpage pre-route defpartial render]]
        [noir.request :only [ring-request]]
        [hiccup.core :only [html]]
        [hiccup form-helpers page-helpers]
        ))

(defpage [:get "/"] [] "This is a get") ;; same as (defpage "/" [] ..)
(defpage [:post "/"] [] "This is a post")
(defpage [:put "/"] [] "This is a put")
(defpage [:any "/"] [] "This is any request type")

(defpage "/error" []
  {:status 500
   :body "Oh no! An error has occurred"})

(defn wfinst-from-req
  "retun the WFInst object that is encoded in the body of the current request object"
  []
  (let [req-body-parser (:body (ring-request))]
    (with-open [rdr (clojure.java.io/reader req-body-parser)]
      (let [wfinst-str (slurp rdr)
            wfinst-stream (fformat/string-input-stream wfinst-str)
            wfinst (fformat/wfinstance-from-stream wfinst-stream)]
        wfinst))))

(defpage [:get "/wfinstance"] []
  (let [wfinst (wfinst-from-req)
        wf (:workflow wfinst)
        new-wf (wflow/wf-with-internal-ids wf)
        new-wfinst (assoc wfinst :workflow new-wf)]
    ;; TODO: replace these printing statements with actual code execution
    (println "I have pretended to run:")
    (exec/print-deps-in-order new-wf)
    (fformat/workflow-instance-to-string new-wfinst)))

(defpage [:put "/wfinstance"] []
  "POST: Create a new instance of a workflow")