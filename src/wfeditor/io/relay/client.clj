(ns wfeditor.io.relay.client
  (:use clj-ssh.ssh)
  (:require [clj-http.client :as client]
            [wfeditor.io.file.wfeformat :as fformat]
            [wfeditor.model.workflow :as wflow]
            [wfeditor.io.util.const :as const]
            [wfeditor.io.status.task-run :as task-status]))

;;
;; agents (declarations here, definitions below)
;;

;; this agent is meant to represent a queue for all SSH connections so
;; that they wait one-after-another and do not compete or conflict
;; with each others claims for local resources (local port bindings, etc.)
(declare ssh-agent)

;;
;; util functions
;;

(defn default-ssh-tunnel-params
  "return the parameters (local and/or remote hosts and/or ports) necessary for an SSH tunnel from the client to server, as needed by the functions in this namespace"
  []
  [const/DEFAULT-HOST const/DEFAULT-PORT const/DEFAULT-LOCAL-PORT const/DEFAULT-LOCAL-HOST const/DEFAULT-SERVER-HOST-REL-TO-REMOTE])

(defn- response-msg
  "retrieve the body of the HTTP response message from the server"
  [resp]
  (:body resp))

(defn clj-agent-ssh
  "take a fn that runs an SSH command, gets it to run in a clojure agent (so as to prevent simultaneous SSH commands), and returns the result to the calling fn"
  [f]
  (let [result (promise)
        agent-fn (fn [clj-agent-val]
                   (try
                     (let [output (f)]
                       (deliver result output))
                     (catch Throwable e
                       (println "error in SSH connection: " (.getMessage e) ".  Check your SSH connection properties (client config, server status,...)."))))]
    (send-off ssh-agent agent-fn)
    @result))

;;
;; functions
;;

;;
;; WFInstance functions
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
  ([req-type wfinst path rem-host rem-port loc-port loc-host server-host]
     (let [ssh-fn (fn []
                    (with-ssh-agent  []
                      (let [session (session rem-host :strict-host-key-checking :no)]
                        (with-connection session
                          (with-local-port-forward [session loc-port rem-port]
                            (req-wfinst req-type wfinst path loc-host loc-port))))))]
       (clj-agent-ssh ssh-fn))))

(defn- update-request
  "send an HTTP GET request to the server to get the status for a wf-instance, and the response message is returned"
  ([wfinst]
     (update-request wfinst const/DEFAULT-HOST const/DEFAULT-PORT))
  ([wfinst host port]
     (req-wfinst :get wfinst "/wfinstance" host port)))

(defn- create-request
  "send an HTTP POST request to the server to instantiate the execution of a wf-instance on the server, and the response message is returned"
  ([wfinst]
     (create-request wfinst const/DEFAULT-HOST const/DEFAULT-PORT))
  ([wfinst host port]
     (req-wfinst :post wfinst "/wfinstance" host port)))

(defn- update-request-over-ssh-tunnel
  "same as update-request, but over an ssh-tunnel"
  ([wfinst]
     (apply update-request-over-ssh-tunnel wfinst (default-ssh-tunnel-params)))
  ([wfinst rem-host rem-port loc-port loc-host server-host]
     (req-wfinst-over-ssh-tunnel :get wfinst "/wfinstance" rem-host rem-port loc-port loc-host server-host)))

(defn- create-request-over-ssh-tunnel
  "same as create-request, but over an ssh-tunnel"
  ([wfinst]
     (apply create-request-over-ssh-tunnel wfinst (default-ssh-tunnel-params)))
  ([wfinst rem-host rem-port loc-port loc-host server-host]
     (req-wfinst-over-ssh-tunnel :post wfinst "/wfinstance" rem-host rem-port loc-port loc-host server-host)))

(defn- wfinst-from-response-msg
  "extract the WFInstance object encoded in the HTTP response message sent back from the server"
  [resp]
  (let [wfinst-str (response-msg resp)
        wfinst (fformat/wfinstance-from-string wfinst-str)]
    wfinst))

(defn- update-sge-response-wfinst
  "take the WFInstance input, send it to the server running SGE, and return the WFInstance returned containing an updated state"
  [wfinst & conn-args]
  (let [resp (apply update-request-over-ssh-tunnel wfinst conn-args)]
    (wfinst-from-response-msg resp)))

(defn- create-sge-response-wfinst
  "take the WFInstance input, send it to the server running SGE, and return the WFInstance returned containing the state after enqueuing the jobs"
  [wfinst & conn-args]
  (let [resp (apply create-request-over-ssh-tunnel wfinst conn-args)]
    (wfinst-from-response-msg resp)))

;; return the response WFInstance returned by the server (from an
;; update operation)
;; op is the operation being performed on the WFInstance.  The possible values for op are
;;  #{:create :update}
(defmulti response-wfinst (juxt (comp :exec-domain first vector) (comp second vector)))
(defmethod response-wfinst ["SGE" :create] [wfinst op & conn-args] (apply create-sge-response-wfinst wfinst conn-args))
(defmethod response-wfinst ["rem-piped-shell" :create] [wfinst op & conn-args] (apply create-sge-response-wfinst wfinst conn-args))
(defmethod response-wfinst ["SGE" :update] [wfinst op & conn-args] (apply update-sge-response-wfinst wfinst conn-args))
(defmethod response-wfinst ["rem-piped-shell" :update] [wfinst op & conn-args] (apply update-sge-response-wfinst wfinst conn-args))

;;
;; job status functions
;;
;; at the moment, no multi-methods b/c exec-domain is assumed to be
;; SGE for temporary convenience
;;

(defn- req-status
  "similar to req-wfinst function, but for job execution statuses"
  [req-type exec-domain username path host port]
  (let [req-fn (condp = req-type
                 :get client/get
                 :post client/post)
        ;; path "/status"
        url (str "http://" host ":" port path)]
    ;; TODO: update this to use JSON (or XML, but in this case, JSON
    ;; is better?)
    ;; TODO: include info of the exec-domain at least, esp. if calls
    ;; are being made from a remote client
    ;; (req-fn url {:body "body not looked at, at least, not yet"})
    (req-fn url {:body username})))

(defn- req-status-over-ssh-tunnel
  "same as req-status, but over an ssh-tunnel"
  [req-type exec-domain username path rem-host rem-port loc-port loc-host server-host]
  (let [ssh-fn (fn []
                 (with-ssh-agent  []
                   (let [session (session rem-host :strict-host-key-checking :no)]
                     (with-connection session
                       (with-local-port-forward [session loc-port rem-port]
                         (req-status req-type exec-domain username path loc-host loc-port))))))]
    (clj-agent-ssh ssh-fn)))

(defn- status-update-request
  "same as update-request, but for job execution statuses. body of response is JSON-encoded statuses from server"
  ([exec-domain username]
     (status-update-request exec-domain username const/DEFAULT-HOST const/DEFAULT-PORT))
  ([exec-domain username host port]
     (req-status :get exec-domain username "/status" host port)))

(defn- status-force-server-update-request
  "send an HTTP POST request to get the server to update its cache of job execution statuses"
  ([exec-domain username]
     (status-update-request exec-domain username const/DEFAULT-HOST const/DEFAULT-PORT))
  ([exec-domain username host port]
     (req-status :post exec-domain username "/status" host port)))

(defn- status-update-request-over-ssh-tunnel
  "same as status-update-request, but over an ssh-tunnel"
  ([exec-domain username]
     (apply status-update-request-over-ssh-tunnel (default-ssh-tunnel-params)))
  ([exec-domain username rem-host rem-port loc-port loc-host server-host]
     (req-status-over-ssh-tunnel :get exec-domain username "/status" rem-host rem-port loc-port loc-host server-host)))

(defn status-force-server-update-request-over-ssh-tunnel
  "same as status-force-server-update-request, but over an ssh-tunnel"
  ([exec-domain username]
     (apply status-force-server-update-request-over-ssh-tunnel (default-ssh-tunnel-params)))
  ([exec-domain username rem-host rem-port loc-port loc-host server-host]
     (req-status-over-ssh-tunnel :post exec-domain username "/status" rem-host rem-port loc-port loc-host server-host)))

(defn- statuses-map-from-response-msg
  "extract the job statuses map encoded in the HTTP response message sent back from the server"
  [resp]
  (let [json-str (response-msg resp)
        statuses-map (task-status/json-to-statuses-map json-str)]
    statuses-map))

(defn update-sge-response-statuses
  "take the latest JSON-encoded job statuses as returned by the server running SGE, and return the Clojure nested-map data structure containing an updated state"
  [exec-domain username & conn-args]
  (let [resp (apply status-update-request-over-ssh-tunnel exec-domain username conn-args)]
    (statuses-map-from-response-msg resp)))

;;
;; agents (definitions)
;;

(def ssh-agent (agent nil))