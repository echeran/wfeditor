(ns wfeditor.io.relay.client
  (:require [clj-http.client :as client]
            [wfeditor.io.relay.server :as wfeserver]
            [wfeditor.io.file.wfeformat :as fformat]))

;;
;; constants
;;

(def DEFAULT-HOST "localhost")

;;
;; functions
;;

(defn req-wfinst
  "send an HTTP request to the server containing a WFInstance. WFInstance sent as its XML string representation in the request body.  req-type (HTTP request type) is one of [:get :post]"
  ;; TODO: make the host and port values optional, supplying defaults from
  ;; client.clj and server.clj when necessary
  ;; ([wfinst]
  ;;    (req-wfinst DEFAULT-HOST wfeserver/DEFAULT-PORT))
  ([req-type wfinst host port path]
     (let [req-fn (condp = req-type
                    :get client/get
                    :post client/post)
           ;; TODO: clean up the URL building process with Ring's
           ;; codec-util or java.net.URI or URL
           url (str "http://" host ":" port path)
           wfinst-str (fformat/workflow-instance-to-string wfinst)]
       ;; even for GET, where there is a limit to the query string
       ;; length in most browswers in the range 2KB-8KB, there is no
       ;; limit to the length of the entire message (and no mention of
       ;; a length limit in the HTTP spec)
       ;; SO: http://stackoverflow.com/questions/2659952/maximum-length-of-http-get-request
       (req-fn url {:body wfinst-str}))))