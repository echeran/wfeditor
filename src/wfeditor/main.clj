(ns wfeditor.main
  ;; tells Clojure to generate a Java class out of this namespace --
  ;; this allows a standalone uberjar to be run by Java
  (:gen-class)
  (:require clojure.tools.cli)
  (:require wfeditor.ui.gui.core
            [wfeditor.model.workflow :as wflow]
            [wfeditor.io.relay.server :as server]
            [wfeditor.ui.util.swt-dispose :as swt-dispose]
            [wfeditor.io.util.thread-control :as thread-control])
  (:import
   org.eclipse.swt.widgets.Display))

(defn handle-common-args
  "handle arguments and options that are common for the entire program, whether the program is eventually run in command-line mode, gui mode, or etc."
  [options args]
  ;; set the workflow if the user has inputed one
  (if (:graph options)
    (let [new-graph (:graph options)
          new-job-dep-map (wflow/job-dep-map (:graph options))]
      (wflow/set-depends-upon new-job-dep-map))))

(defn ui-create
  "The entry point to building the entire UI.  Uses a JFace idiom to do this, so UI code comes from an extended (proxied) ApplicationWindow.
TODO: handle options and args coming in from the CLI"
  [options args]
  ;; The JFace idiomatic way of displaying a window.  As it seems, using
  ;; the "plain SWT" idiom for displaying a window doesn't work for
  ;; situations like a simple Label attached to the Shell, in the
  ;; following way: no Label widgets are painted on the Shell until the
  ;; user resizes the Shell.  The JFace style of doing things works
  ;; properly as expected.
  ;; If there is any code that should've gone in what would've been a
  ;; subclass constructor, that should go somewhere here, where the
  ;; instance is actually being returned and manipulated
  (let [app-win (wfeditor.ui.gui.core/app-win-proxy)]

    ;; TODO: use CLI options and args to modify the UI here, which is
    ;; after its instantiation 

    
    (. app-win setBlockOnOpen true)
    (. app-win open)
    (swt-dispose/dispose-all)
    (when-let [display (. Display getCurrent)]
        (.dispose display))))

(defn cli-execute
  "The entry point for executing the CLI version of the program"
  [options args]
  ;; TODO: create functionality of program!
  )

(defn server-execute
  "The entry point for creating the server version of the program"
  [options args]
  (let [port (:port options)
        new-server (server/new-running-server {:port port})]
    (wfeditor.io.util.thread-control/start-all-bg-threads-server)
    (server/set-server new-server)))

(defn parse-args
  "parse the command-line arguments and, as the clojure.tools.cli provides, returns a vector containing 3 elements: 1) the parsed options, 2) remaining arguments, and 3) a help banner. works well with user-args as a list of the command-line arguments"
  [user-args]
  (let [[options arguments banner] (clojure.tools.cli/cli user-args
                                        ["-g" "--[no-]gui" "Run the GUI frontend with the program" :default true :flag true]
                                        ["-h" "--help" "Display the command-line help statement" :default false :flag true]
                                        ["--graph" "An initial job dependency graph as a map of keywords -> nested vector of keywords (Ex:  {:1 [:0 :2] :3 [:1] :4 [:0]})" :default nil :parse-fn load-string]
                                        ["-S"  "--server" "Run in server mode, i.e., run the agent polling the computation server (when on the server main node)" :default false :flag true])]
    (when (:help options)
      (println banner)
      (System/exit 0))
    [options arguments banner]))

(defn -main
  "main method (i.e., entry point) for the entire WFE"
  [ & args]
  (let [[options parsed-args banner] (parse-args args)]
    (handle-common-args options args)
    (cond
     (true? (:server options)) (server-execute options parse-args)
     (false? (:gui options))  (cli-execute options parsed-args)
     :else (ui-create options parsed-args))))
