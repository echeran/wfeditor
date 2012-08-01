(ns wfeditor.io.util.thread-control
  (:require
   [wfeditor.io.status.task-run-thread :as task-status-thread]))

(defn start-all-bg-threads-server
  "start all background threads necessary in the server process"
  []
  (task-status-thread/init-bg-thread-status-updater-thread)
  (task-status-thread/init-bg-thread-status-output-thread))

(defn stop-all-bg-threads-server
  "stop all background threads that are running in the server process"
  []
  (task-status-thread/stop-bg-thread-status-updater-thread)
  (task-status-thread/stop-bg-thread-status-output-thread)
  ;; reason for shutdown-agents
  ;; http://tech.puredanger.com/2010/06/08/clojure-agent-thread-pools/
  (shutdown-agents))

(defn start-all-bg-threads-client
  "start all background threads necessary in the client process"
  []
  (task-status-thread/init-bg-thread-status-output-thread)
  (task-status-thread/init-bg-thread-status-from-server-updater-thread))

(defn stop-all-bg-threads-client
  "stop all background threads that are running in the client progress"
  []
  (task-status-thread/stop-bg-thread-status-output-thread)
  (task-status-thread/stop-bg-thread-status-from-server-updater-thread)
  ;; reason for shutdown-agents
  ;; http://tech.puredanger.com/2010/06/08/clojure-agent-thread-pools/
  (shutdown-agents))