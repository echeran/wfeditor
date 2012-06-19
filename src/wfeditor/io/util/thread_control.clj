(ns wfeditor.io.util.thread-control
  (:require
   [wfeditor.io.execution :as exec]
   [wfeditor.io.status.task-run :as task-status]))

(defn start-all-bg-threads-server
  "start all background threads necessary in the server process"
  []
  (task-status/init-bg-thread-status-updater-thread))

(defn stop-all-bg-threads-server
  "stop all background threads that are running in the program"
  []
  (task-status/stop-bg-thread-status-updater-thread))