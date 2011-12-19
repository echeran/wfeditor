(ns wfeditor.ui.gui.zest.types)

(defrecord MyNode [id name connected-to])

(defrecord MyConnection [id label source destination])