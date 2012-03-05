(ns wfeditor.io.relay.server-views.welcome
  (:require [wfeditor.io.relay.server-views.common :as common]
            [noir.content.getting-started]
            [noir.response :as resp]
            [noir.validation :as vali])
  (:use [noir.core :only [defpage pre-route defpartial render]]
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

(defpage [:get "/wfinstance"] {:keys [wfinstance]}
  (str "You are requesting the status of the following workflow instance"
       "\n"
       wfinstance))

(defpage [:post "/wfinstance"] []
  "Create a new instance of a workflow")