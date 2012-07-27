(ns wfeditor.io.util.dir
  (:require [wfeditor.io.util.const :as io-const]
            [clj-commons-exec :as commons-exec]
            [clojure.string :as string]
            [fs.core :as fs]))

;;
;; general dir functions
;;

(defn cmd-result
  "given the vector of command parts as taken by executing clj-commons-exec/sh, execute and return the result map"
  [cmd-parts]
  (let [cmd-prom (commons-exec/sh cmd-parts)
        result @cmd-prom]
    result))

(defn bash-cmd
  "take the list/vector of command parts that are passed to clj-commons-exec/sh and tack on the bash command to the beginning to ensure that it is run through bash"
  [cmd-parts]
  (concat ["bash" "-c"] [(string/join " " cmd-parts)]))

(defn sudo-cmd
  "take the list/vector of command parts that are passed to clj-commons-exec/sh and tack on the sudo command to the beginning to ensure that it is run as another user"
  [username cmd-parts]
  (concat ["sudo" "-u" username "-i"] cmd-parts))

(defn result-first-line
  "take the result map from clj-commons-exec/sh and give the first line of output"
  [result]
  (when-let [result-out (:out result)]
    (first (string/split-lines result-out))))

(defn bash-expansion
  "given an expression in bash, give the bash interpreter's expansion of that expression"
  [expr]
  ;; bash expansion form taken from getting users' home dirs from this
  ;; SO: http://stackoverflow.com/questions/7358611/bash-get-users-home-directory-when-they-run-a-script-as-root
  (let [expr-eval-cmd-parts [(str "echo $(eval echo " expr ")")]
        cmd-parts (bash-cmd expr-eval-cmd-parts)
        result (cmd-result cmd-parts)
        first-line (result-first-line result)]
    first-line))

(defn bash-user-home
  "give the bash interpreter's result of a user's home"
  ([]
     (bash-user-home nil))
  ([user]
     (let [user (or user "")
           bash-expr (str "~" user)
           home-dir (bash-expansion bash-expr)]
       (when (not= bash-expr home-dir)
         home-dir))))

(defn bash-mkdir
  "create a directory(ies) using bash. optional username is the username under which to perform the mkdir operation"
  ([dir]
     (bash-mkdir nil dir))
  ([username dir]
     (let [dir (str (fs/normalized-path (fs/file dir)))
           mkdir-cmd-parts ["mkdir" "-p" dir]
           bash-mkdir-cmd-parts (bash-cmd mkdir-cmd-parts)
           cmd-parts (if username
                       (sudo-cmd username bash-mkdir-cmd-parts)
                       bash-mkdir-cmd-parts)]
       (cmd-result cmd-parts))))

;;
;; props dir - functions
;;

(defn- props-dir
  "return the full path of the directory in which all the properties info of the program (running as this user) is stored as a Java File object. this directory should also be specific to whether the program is being run as a client or a server (i.e., server mode should have a separate configuration from client mode). ('properties' includes job output files, configuration, etc., and by 'properties info directory' I basically mean the dot-directory in the home directory)"
  ([]
     (props-dir nil))
  ([user]
     (let [home-dir-str (bash-user-home user)
           home-dir (fs/file home-dir-str)
           relay-type-name (name @io-const/relay-type)
           props-dir (fs/file home-dir io-const/PROPS-DIR-NAME relay-type-name)]
       props-dir)))

(defn config-dir
  "return the full path of the directory in which the configuration info of the program is stored. similar to props-dir, and should be within the props-dir"
  [& [user :as params]]
  (fs/file (apply props-dir params) io-const/CONFIG-DIR-NAME))

(defn data-dir
  "return the full path of the directory in which the data of the program are stored. similar to props-dir, and should be within the props-dir"
  [& [user :as params]]
  (fs/file (apply props-dir params) io-const/DATA-DIR-NAME))

(defn task-run-file
  "a File object representing the file where the jobs' tasks' statuses are stored"
  [& [user :as params]]
  (fs/file (apply config-dir params) io-const/TASK-RUN-FILE-NAME))
