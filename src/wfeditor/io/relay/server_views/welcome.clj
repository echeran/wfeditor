(ns wfeditor.io.relay.server-views.welcome
  (:require [wfeditor.io.relay.server-views.common :as common]
            [wfeditor.io.file.wfeformat :as fformat]
            [wfeditor.io.execution :as exec]
            [wfeditor.model.workflow :as wflow]
            [noir.content.getting-started]
            [noir.response :as resp]
            [noir.validation :as vali]
            [clj-commons-exec :as commons-exec])
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
            ;; wfinst-stream (fformat/string-input-stream wfinst-str)
            ;; wfinst (fformat/wfinstance-from-stream wfinst-stream)
            wfinst (fformat/wfinstance-from-string wfinst-str)
            ]
        wfinst))))

(defpage [:get "/wfinstance"] []
    (let [wfinst (wfinst-from-req)
        new-wfinst (exec/update-wfinst-sge wfinst)]
    (println "updating jobs via SGE... (SGE hard-coded a.t.m.)")
    ;; TODO: turn enqueuing into multi-method where SGE is just one poss.
    (println "finished updating jobs via SGE")
    (fformat/workflow-instance-to-string new-wfinst)))

(defpage [:post "/wfinstance"] []
  (let [wfinst (wfinst-from-req)
        new-wfinst (exec/enqueue-wfinst-sge wfinst)]
    (println "enqueuing jobs via SGE... (SGE hard-coded a.t.m.)")
    ;; TODO: turn enqueuing into multi-method where SGE is just one poss.
    (println "finished enqueuing jobs via SGE")
    (fformat/workflow-instance-to-string new-wfinst)))