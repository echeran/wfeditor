(ns wfeditor.ui.gui.core
  ;; putting the import statement in the ns form is the preferred way
  ;; of importing Java classes in Clojure (see
  ;; http://pragprog.com/magazines/2010-11/getting-clojure)
  (:require wfeditor.ui.gui.zest.canvas
            [wfeditor.ui.gui.editor-left :as editor-left]
            [wfeditor.io.util.thread-control :as thread-control]
            [wfeditor.io.status.task-run :as task-status]

            [wfeditor.model.workflow :as wflow]
            [wfeditor.io.file.wfeformat :as fformat])
  (:use [wfeditor.ui.util.swt :as swt-util])
  (:import
   org.eclipse.jface.window.ApplicationWindow
   org.eclipse.swt.SWT
   (org.eclipse.swt.layout FormLayout FormData FormAttachment GridLayout FillLayout)
   (org.eclipse.swt.widgets Display Shell Sash Composite)
   (org.eclipse.swt.events SelectionEvent SelectionAdapter)
   org.eclipse.jface.action.MenuManager
   org.eclipse.jface.action.Action


   org.eclipse.swt.widgets.FileDialog))

;;
;; functions
;;

(defn initialize-gui
  "initialize everything that the GUI needs before creating the Shell, etc."
  []
  (. Display setAppName "WFEditor")
  (task-status/initialize-task-status-file-ops)
  (thread-control/start-all-bg-threads-client))

(defn cleanup-gui
  "perform cleanup operations after the Shell has been closed"
  []
  (thread-control/stop-all-bg-threads-client)
  (task-status/statuses-to-file))

(defn ui-editor-right-create [parent] 
  (wfeditor.ui.gui.zest.canvas/graph-viewer-create parent) 
  (.setLayout parent (GridLayout.)))

;; assume FormLayout of the parent widget to which the returned sash
;; will be attached
(defn ui-editor-sash [parent]
  (let [sash (Sash. parent SWT/VERTICAL)
        data (FormData.)]
    (do
      (set! (. data top) (FormAttachment. 0 0))
      (set! (. data bottom) (FormAttachment. 100 0))
      (set! (. data left) (FormAttachment. 50 0))
      (.setLayoutData sash data))
    sash))

(defn ui-editor-create [parent]
  (let [sash (Sash. parent (bit-or SWT/VERTICAL SWT/BORDER SWT/SMOOTH))
        ;; have to make the style of the elements next to Sash have
        ;; BORDER so that Sash is drawn identifiably
        comp-left (Composite. parent SWT/BORDER)
        comp-right (Composite. parent SWT/BORDER)
        sash-fdata (FormData.)
        comp-left-fdata (FormData.)
        comp-right-fdata (FormData.)]
    (do
      (let [shell (get-ancestor-shell parent)]
        (.setText shell "WFEditor")))
    (do
      (.setLayout parent (FormLayout.))
      (editor-left/ui-editor-left-create comp-left)
      (ui-editor-right-create comp-right))
    (do
      (set! (. sash-fdata top) (FormAttachment. 0 0))
      (set! (. sash-fdata bottom) (FormAttachment. 100 0))
      (set! (. sash-fdata left) (FormAttachment. 25 0))
      (.setLayoutData sash sash-fdata)
      (set! (. comp-left-fdata top) (FormAttachment. 0 0))
      (set! (. comp-left-fdata bottom) (FormAttachment. 100 0))
      (set! (. comp-left-fdata left) (FormAttachment. 0 0))
      (set! (. comp-left-fdata right) (FormAttachment. sash 0))
      (.setLayoutData comp-left comp-left-fdata)
      (set! (. comp-right-fdata top) (FormAttachment. 0 0))
      (set! (. comp-right-fdata bottom) (FormAttachment. 100 0))
      (set! (. comp-right-fdata left) (FormAttachment. sash 0))
      (set! (. comp-right-fdata right) (FormAttachment. 100 0))
      (.setLayoutData comp-right comp-right-fdata))
    (do
      (.addSelectionListener sash (proxy [SelectionAdapter]
                                      [] ;; do not call the
                                    ;; super-class constructor w/o
                                    ;; reason to, but provide it for
                                    ;; proxy's syntax's sake
                                    (widgetSelected [event]
                                      (set! (. (^FormData . sash getLayoutData) left) (FormAttachment. 0 (. event x)))
                                      (dorun
                                       (.. sash getParent layout))))))))

(defn ui-menu-bar
  "create a menu bar for the ApplicationWindow using JFace"
  []
  (let [menu-mgr (MenuManager.)
        file-menu (MenuManager. "File")
        active-shell-fn (fn [] (.. Display getCurrent getActiveShell))
        open-actn (proxy [Action] ["Open"]
                    (run []
                      (let [fd (new FileDialog (get-ancestor-shell (active-shell-fn)) SWT/OPEN)]
                        (when-let [in-file-name (.open fd)]
                          (fformat/set-workflow-from-file in-file-name)))))
        save-as-actn (proxy [Action] ["Save As"]
                       (run []
                         (let [fd (FileDialog. (get-ancestor-shell (active-shell-fn)) SWT/SAVE)]
                           (when-let [out-file-name (.open fd)]
                             (fformat/save-workflow-to-file (wflow/workflow) out-file-name)))))]
    (doto file-menu
      (.add open-actn)
      (.add save-as-actn))
    (doto menu-mgr
      (.add file-menu))
    menu-mgr))

;; JFace way of creating a window is to subclass ApplicationWindow and
;; override createContents
;; This is the Clojure way of "subclassing", i.e., extending a class
;; via the proxy macro.  This function just returns the instance of
;; the anonymous class generated by proxy

(defn app-win-proxy []
  (proxy [ApplicationWindow]
      [nil]  ;; calling the ApplicationWindow constructor
    ;; note, should not implement any subclass constructor, as
    ;; explained in
    ;; http://pragprog.com/magazines/2010-11/getting-clojure
    ;; Instead, any code that would go in the subclass construtor
    ;; should be applied to the instance of the extended class
    ;; returned by proxy
    ;;
    ;; the addMenuBar call in the constructor is required by JFace for the
    ;; ApplicationWindow subclass to call createMenuManger
    (createContents [parent]
      (ui-editor-create parent))
    (getInitialSize []
      ;; override default implementation (documented in Javadoc) with
      ;; one that ensures the window is "maximized"
      ;; note: a display can encompass multiple monitors, so get just
      ;; primary monitor in order not to be a screen-hog on a
      ;; multi-display setup
      ;; I wouldn't be doing this if I could get
      ;; shell.setMaximized(true) to work in Clojure easily, but I
      ;; can't.  I haven't tested doing an all-out gen-class for a
      ;; subclass of ApplicationWindow (instead of this proxy), but
      ;; that would be the next step.  For now, at least, this
      ;; workaround of setMaximized(true) is fine by me.
      ;;
      ;; after adding the JFace menu bar, this maximization code
      ;; doesn't seem to work
      ;; TODO: try either of
      ;; http://www.eclipsezone.com/eclipse/forums/t31806.html
      ;; http://stackoverflow.com/questions/9722911/java-swt-application-bring-to-front
      ;; for yet another workaround
      (let [shell (.getShell this)
            display (.getDisplay shell)
            prim-mon (.getPrimaryMonitor display)
            client-area (.getClientArea prim-mon)]
        (.computeSize shell (. client-area width) (. client-area height) true)))
    (createMenuManager []
      ;; method required if creating a menu through JFace
      ;; must return MenuManger object
      (ui-menu-bar))))