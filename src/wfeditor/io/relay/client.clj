(ns wfeditor.io.relay.client
  (:use clj-ssh.ssh)
  (:require [clj-http.client :as client]
            [wfeditor.io.file.wfeformat :as fformat]
            [wfeditor.model.workflow :as wflow]
            [wfeditor.io.util.const :as const]))

;;
;; util functions
;;

(defn default-ssh-tunnel-params
  "return the parameters (local and/or remote hosts and/or ports) necessary for an SSH tunnel from the client to server, as needed by the functions in this namespace"
  []
  [const/DEFAULT-HOST const/DEFAULT-PORT const/DEFAULT-LOCAL-PORT const/DEFAULT-LOCAL-HOST const/DEFAULT-SERVER-HOST-REL-TO-REMOTE])

;;
;; functions
;;

(defn- req-wfinst
  "send an HTTP request to the server containing a WFInstance. WFInstance sent as its XML string representation in the request body.  req-type (HTTP request type) is one of [:get :post]"
  ([req-type wfinst path host port]
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

(defn- req-wfinst-over-ssh-tunnel
  "send the HTTP request as in the fn req-wfinst, but do it over an ssh tunnel, a.k.a. local port forwarding. server-host is the host that WFEditor's server mode process is running on, relative to the remote host.  If the server process is running on the remote hos, then this is 'localhost' "
  ;; ([req-type wfinst path]
  ;;    (apply req-wfinst-over-ssh-tunnel req-type wfinst path (default-ssh-tunnel-params)))
  ;; ([req-type wfinst path rem-host rem-port loc-port]
  ;;    (req-wfinst-over-ssh-tunnel wfinst path rem-host rem-port loc-port const/DEFAULT-LOCAL-HOST))
  ;; ([req-type wfinst path rem-host rem-port loc-port loc-host]
  ;;    (println "params are= " {:req-type req-type :wfinst wfinst :path path :rem-host rem-host :rem-port rem-port :loc-port loc-port :loc-host loc-host})
  ;;    (req-wfinst-over-ssh-tunnel wfinst path rem-host rem-port loc-port loc-host const/DEFAULT-SERVER-HOST-REL-TO-REMOTE))
  ([req-type wfinst path rem-host rem-port loc-port loc-host server-host]
     (with-ssh-agent  [(create-ssh-agent true)]
       (let [session (session rem-host :strict-host-key-checking :no)]
         (with-port-forwarded-connection [session loc-port rem-port server-host]
           ;; (println (client/get "http://localhost:7777/" {:body wfinst-str}))
           (req-wfinst req-type wfinst path loc-host loc-port))))))

(defn- update-request
  "send an HTTP GET request to the server to get the status for a wf-instance, and the response message is returned"
  ([wfinst]
     (update-request wfinst const/DEFAULT-HOST const/DEFAULT-PORT))
  ([wfinst host port]
     (req-wfinst :get wfinst "/wfinstance" host port)))

(defn- update-request-over-ssh-tunnel
  "same as update-request, but over an ssh-tunnel"
  ([wfinst]
     (apply update-request-over-ssh-tunnel wfinst (default-ssh-tunnel-params)))
  ([wfinst rem-host rem-port loc-port loc-host server-host]
     (req-wfinst-over-ssh-tunnel :get wfinst "/wfinstance" rem-host rem-port loc-port loc-host server-host)))

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
  (let [
        ;; resp (update-request wfinst)
        resp (update-request-over-ssh-tunnel wfinst)
        ]
    (wfinst-from-response-msg resp)))

;; return the response WFInstance returned by the server (from an
;; update operation)
(defmulti response-wfinst :exec-domain)
(defmethod response-wfinst "SGE" [wfinst] (sge-response-wfinst wfinst))