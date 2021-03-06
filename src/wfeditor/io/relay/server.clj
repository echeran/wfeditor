(ns wfeditor.io.relay.server
  (:require [noir.server :as server]
            [wfeditor.io.util.const :as const]
            wfeditor.io.relay.server-views.welcome
            wfeditor.io.relay.server-views.common
            [wfeditor.io.util.thread-control :as thread-control]
            [wfeditor.io.status.task-run :as task-status]))

;;
;; refs
;;

(def s (ref nil))

;;
;; functions
;;

(defn common-load-views
  "Load the Noir server view files for all server types (view = V of MVC, files = Clojure code that use Noir to define the behavior of the server)"
  []
  ;; remember that a ns uses dashes but the corresponding path uses

  ;; substituting load-views-ns for load-views so that when the
  ;; uberjar is compiled, a compiled version can be used instead of
  ;; having to include the source *.clj files.  this will make it
  ;; slightly less convenient, but oh well
  ;; (server/load-views "src/wfeditor/io/relay/server_views/")
  (server/load-views-ns 'wfeditor.io.relay.server-views.common
                        'wfeditor.io.relay.server-views.welcome)
  )

(defn get-server
  "Get the current value of the server"
  []
  @s)

(defn set-server
  "Set the current state of the server"
  [server]
  (dosync
   (ref-set s server)))

(defn new-running-server
  "Create a new Noir server, start it, and return it.  config map can contain any of the keys [:mode :port]"
  [& [config-map]]
  (let [mode (get config-map :mode :dev)
        port (let [user-port-val (get config-map :port)
                   env-port-str (get (System/getenv) "PORT")
                   env-port-val (when env-port-str (Integer. env-port-str))]
               (cond
                user-port-val user-port-val
                env-port-val env-port-val
                true const/DEFAULT-PORT))]
    (common-load-views)
    (server/start port {:mode mode
                        :ns 'wfeditor.io.relay.server})))

(defn initialize-server-process
  "initialize everything that the server process needs in addition to the embedded [HTTP] server from new-running-server"
  []
  (task-status/initialize-task-status-file-ops)
  (thread-control/start-all-bg-threads-server))