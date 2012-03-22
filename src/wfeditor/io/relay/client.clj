(ns wfeditor.io.relay.client
  (:require [clj-http.client :as client]
            [wfeditor.io.file.wfeformat :as fformat]
            [wfeditor.model.workflow :as wflow]
            [wfeditor.io.util.const :as const]))


;;
;; functions
;;

(defn- req-wfinst
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
       ;; limit to the length of the entire message (but no mention of
       ;; an overall length limit in the HTTP spec)
       ;; from SO: http://stackoverflow.com/questions/2659952/maximum-length-of-http-get-request
       ;; TODO: put this function call in a Clojure state-aware call
       ;; for I/O, so probably asynchronous, like send or send-off
       ;; (not sure agent is what we need), or better yet, wrap the
       ;; function from the UI ultimately calling this
       (req-fn url {:body wfinst-str}))))

(defn- update-request
  "send an HTTP GET request to the server to get the status for a wf-instance, and the response message is returned"
  [wfinst]
  (req-wfinst :get wfinst const/DEFAULT-HOST const/DEFAULT-PORT "/wfinstance"))

(defn- response-msg
  "retrieve the body of the HTTP response message from the server"
  [resp]
  (:body resp))

(defn- wfinst-from-response-msg
  "extract the WFInstance object encoded in the HTTP response message sent back from the server"
  [resp]
  (let [wfinst-str (response-msg resp)
        wfinst-str-stream (fformat/string-input-stream wfinst-str)
        wfinst (fformat/wfinstance-from-stream wfinst-str-stream)]
    wfinst))

(defn- sge-response-wfinst
  "take the WFInstance input, send it to the server running SGE, and return the WFInstance returned containing an updated state"
  [wfinst]
  (let [resp (update-request wfinst)]
    (wfinst-from-response-msg resp)))

;; return the response WFInstance returned by the server (from an
;; update operation)
(defmulti response-wfinst :exec-domain)
(defmethod response-wfinst "SGE" [wfinst] (sge-response-wfinst wfinst))