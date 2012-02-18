(ns wfeditor.io.relay.server-views.welcome
  (:require [wfeditor.io.relay.server-views.common :as common]
            [noir.content.getting-started]
            [noir.response :as resp]
            [noir.validation :as vali])
  (:use [noir.core :only [defpage pre-route defpartial render]]
        [hiccup.core :only [html]]
        [hiccup form-helpers page-helpers]
        ))

(defpage "/welcome" []
         (common/layout
           [:p "Welcome to WFE server test"]))

;; ;; A very simple page definition that maps to the root of your site.
;; (defpage "/" []
;;   "hello")

(defpage [:get "/"] [] "This is a get") ;; same as (defpage "/" [] ..)
(defpage [:post "/"] [] "This is a post")
(defpage [:put "/"] [] "This is a put")
(defpage [:any "/"] [] "This is any request type")

(defpage [:post "/login"] {:keys [username password]}
  (str "You tried to login as " username " with the password " password))

(defpage "/login" []
  (str "You need to login?"))

(defpage "/error" []
  {:status 500
   :body "Oh no! An error has occurred"})

;; (pre-route "/admin/*" {}
;;            (when-not (users/admin?)
;;              (resp/redirect "/login")))


(defpartial layout [& content]
  (html5
   [:head
    [:title "Forms"]]
   [:body
    content]))

(defpartial user-fields [{:keys [firstname lastname]}]
  (label "firstname" "First name: ")
  (text-field "firstname" firstname)
  (label "lastname" "Last name: ")
  (text-field "lastname" lastname))

(defpage "/user/add" {:as user}
  (layout
   (form-to [:post "/user/add"]
            (user-fields user)
            (submit-button "Add user"))))

(defn valid? [{:keys [firstname lastname]}]
  true)

(defpage [:post "/user/add"] {:as user}
  (if (valid? user)
    (layout
     [:p "User added!"])
    (render "/user/add" user)))

(defn valid? [{:keys [firstname lastname]}]
  (vali/rule (vali/min-length? firstname 5)
             [:firstname "Your first name must have more than 5 letters."])
  (vali/rule (vali/has-value? lastname)
             [:lastname "You must have a last name"])
  (not (vali/errors? :lastname :firstname)))

(defpage [:post "/user/add"] {:as user}
  (if (valid? user)
    (layout
     [:p "User added!"])
    (render "/user/add" user)))