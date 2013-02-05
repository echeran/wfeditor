;; ;; this ns is meant for compiling and running in an Emacs buffer that
;; ;; is running its own Clojure REPL and connecting to it via
;; ;; a swank-clojure instance via SLIME mode via Clojure mode
;; ;; this works in Linux. this most likely won't work in Mac OS X b/c of
;; ;; Cocoa SWT thread restrictions and weirdnesses - AWT and SWT share
;; ;; same thread.

;; This ns is meant for running the client GUI either from a REPL (Mac
;; OS X) or directly in emacs using SLIME-mode-style commands (Linux).
;; Getting the GUI to run in Mac OS X requires Leiningen 2 and using
;; the 'dumbrepl' alias in project.clj that uses trampoline run to
;; ensure the REPL is the first thread (to appease Carbon SWT
;; restrictions due to the threading model). Getting this to run in
;; Linux directly via nREPL's emacs mode

;; On Mac OS X, when running the REPL, you can start using this from
;; the default ns by doing the following
;; 1. (use 'wfeditor.ui.gui.debug)
;; 2. any time you edit a file
;;   a. run "git st" in a separate terminal window
;;   b. for each new .clj file reported modified by git st, copy those
;; files, quoted, into (add-mod-files ....) at the REPL. ex:
;;     (add-mod-files "src/wfeditor/ui/gui/left/edit_tab.clj" "src/some/other/file.clj")
;; 3. eval (start)
;; 4. to check on which files are updated during each (start), eval (print-mod-files)

(ns wfeditor.ui.gui.debug
  (:require wfeditor.ui.gui.core)
  (:import (org.eclipse.swt.widgets Display)))

(def app-win (atom nil))
(def mod-files (atom #{}))

(defn start
  "launch the GUI from a REPL"
  ([]
     (doseq [f @mod-files]
       (load-file f))
     (reset! app-win (wfeditor.ui.gui.core/app-win-proxy))
     (doto @app-win
       (.addMenuBar)
       (.setBlockOnOpen true)
       (.open))))

(defn add-mod-files
  "add a file that must be checked before every launch (this creates a non-nuanced, poor-man's emacs SLIME eval)
file must be specified from the src directory and include the .clj, ex: src/wfeditor/ui/gui/left/edit_tab.clj"
  [& file-names]
  (doseq [f file-names]
    (swap! mod-files conj f)))

(defn print-mod-files
  "print a list of all the mod files"
  []
  (doseq [f (sort @mod-files)]
    (println f)))