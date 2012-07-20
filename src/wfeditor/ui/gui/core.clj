(ns wfeditor.ui.gui.core
  ;; putting the import statement in the ns form is the preferred way
  ;; of importing Java classes in Clojure (see
  ;; http://pragprog.com/magazines/2010-11/getting-clojure)
  (:require wfeditor.ui.gui.zest.canvas
            [wfeditor.ui.gui.editor-left :as editor-left]
            [wfeditor.io.util.thread-control :as thread-control]
            [wfeditor.io.status.task-run :as task-status]
            [wfeditor.ui.util.const :as ui-const]
            [wfeditor.ui.state.gui :as gui-state])
  (:use [wfeditor.ui.util.swt :as swt-util])
  (:import
   org.eclipse.jface.window.ApplicationWindow
   org.eclipse.swt.SWT
   (org.eclipse.swt.layout GridLayout FillLayout RowLayout)
   (org.eclipse.swt.widgets Display Shell Composite)
   (org.eclipse.swt.events SelectionEvent SelectionAdapter)
   org.eclipse.jface.action.MenuManager
   org.eclipse.jface.action.Action))

;;
;; functions
;;

(defn initialize-gui
  "initialize everything that the GUI needs before creating the Shell, etc."
  []
  (. Display setAppName ui-const/WINDOW-APP-NAME)
  (task-status/initialize-task-status-file-ops)
  (thread-control/start-all-bg-threads-client))

(defn cleanup-gui
  "perform cleanup operations after the Shell has been closed"
  []
  (thread-control/stop-all-bg-threads-client)
  (task-status/statuses-to-file))

(defn ui-editor-right
  [parent]
  (let [comp-right (new-widget {:keyname :editor-right :widget-class Composite :parent parent :styles [SWT/BORDER]})]
    (do
      (wfeditor.ui.gui.zest.canvas/graph-viewer-create comp-right))
    (do
      (.setLayout comp-right (GridLayout.)))
    comp-right))

(defn ui-editor-create [parent]
  (let [comp-left (editor-left/ui-editor-left parent)
        comp-right (ui-editor-right parent)]
    (do
      (let [shell (get-ancestor-shell parent)]
        (.setText shell "WFEditor")))
    (swt-util/sash-ify parent comp-left comp-right (/ 1 4))))

;; (defn ui-toolbar
;;   "create a toolbar for the entire window"
;;   [parent]
;;   (let [toolbar (ToolBar. parent SWT/HORIZONTAL)
;;         push-button-names ["Open" "Save As"]]
;;     (doseq [name push-button-names]
;;       (doto (ToolItem. toolbar SWT/PUSH)
;;         (.setText name)))
;;     toolbar))

;; (defn ui-contents-create
;;   "create the entire contents of the window"
;;   [parent]
;;   (let [toolbar (ui-toolbar parent)
;;         comp-left-right (Composite. parent SWT/NONE)]
;;     (do
;;       (.setLayout parent (RowLayout. SWT/VERTICAL)))
;;     (do
;;       (ui-editor-create comp-left-right))))

(defn ui-menu-bar
  "create a menu bar for the ApplicationWindow using JFace, return the MenuManager that represents the menu bar"
  []
  (let [menu-mgr (MenuManager.)
        file-menu (MenuManager. "File")
        open-actn (proxy [Action] ["Open"]
                    (run []
                      (swt-util/file-dialog-open-wf (get-ancestor-shell (active-shell)))))
        save-as-actn (proxy [Action] ["Save As"]
                       (run []
                         (swt-util/file-dialog-save-as-wf (get-ancestor-shell (active-shell)))))]
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
      (gui-state/initialize-gui-map {:keyname :shell :obj (.getShell this) :class (class (.getShell this))})
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