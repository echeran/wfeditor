(ns wfeditor.io.util.thread-control
  (:require
   [wfeditor.io.execution :as exec]))

(defn start-all-bg-threads-server
  "start all background threads necessary in the server process"
  []
  (exec/init-bg-thread-status-updater-thread))

(defn stop-all-bg-threads-server
  "stop all background threads that are running in the program"
  []
  (exec/stop-bg-thread-status-updater-thread))