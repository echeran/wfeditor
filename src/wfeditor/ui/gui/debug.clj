(ns wfeditor.ui.gui.debug
  (:require wfeditor.ui.gui.core)
  (:import (org.eclipse.swt.widgets Display)))

;; this ns is meant for compiling and running in an Emacs buffer that
;; is running its own Clojure REPL and connecting to it via
;; a swank-clojure instance via SLIME mode via Clojure mode
;; this works in Linux. this most likely won't work in Mac OS X b/c of
;; Cocoa SWT thread restrictions and weirdnesses - AWT and SWT share
;; same thread.

(let [app-win (wfeditor.ui.gui.core/app-win-proxy)]
  (.addMenuBar app-win)
  (. app-win setBlockOnOpen true)
  (. app-win open)
  (.dispose (. Display getCurrent)))