(ns wfeditor.ui.gui.d2dcanvas
  (:import
   org.eclipse.swt.SWT
   org.eclipse.swt.widgets.Canvas
   org.eclipse.swt.layout.GridData
   (org.eclipse.draw2d Figure XYLayout ColorConstants LightweightSystem RectangleFigure ToolbarLayout PolygonShape PolylineConnection ChopboxAnchor)
      (org.eclipse.draw2d.geometry Rectangle Point)))

(defn connect
  "create and return a line segment that connects two figures to each other on the line connecting their respective centers"
  [fig1 fig2]
  (let [conn (PolylineConnection.)]
    (doto conn
      (.setSourceAnchor (ChopboxAnchor. fig1))
      (.setTargetAnchor (ChopboxAnchor. fig2)))))

(defn create-marriage-figure
  "create and return a figure representing the marriage between two people and the relationship to a 3rd person, their child"
  []
  (let [r (Rectangle. 0 0 50 50)
        polygon-shape (PolygonShape.)]
    (.setStart polygon-shape (.getTop r))
    (dorun (map (fn [point] (.addPoint polygon-shape point)) [(.getTop r) (.getTop r) (.getLeft r) (.getBottom r) (.getRight r) (.getTop r)]))
    (.setEnd polygon-shape (.getTop r))
    (doto polygon-shape
      (.setFill true)
      (.setBackgroundColor ColorConstants/lightGray)
      (.setPreferredSize (.getSize r)))))

(defn create-person-figure
  "create a figure representing a person"
  [name]
  (let [rectangle-figure (RectangleFigure.)]
    (doto rectangle-figure
      (.setBackgroundColor ColorConstants/lightGray)
      (.setLayoutManager (ToolbarLayout.))
      (.setPreferredSize 100 100)
      (.add (org.eclipse.draw2d.Label. name)))))

(defn create-diagram
  "create a canvas with basic draw2d figures"
  [parent]
  (let [root (Figure.)
        layout (XYLayout.)
        canvas (Canvas. parent SWT/DOUBLE_BUFFERED)
        lws (LightweightSystem. canvas)
        andy (create-person-figure "Andy")
        betty (create-person-figure "Betty")
        carl (create-person-figure "Carl")
        marriage (create-marriage-figure)]
    (doto root
      (.setFont (.getFont parent))
      (.setLayoutManager layout)
      (.add andy)
      (.add betty)
      (.add carl)
      (.add marriage (Rectangle. (Point. 145 35) (.getPreferredSize marriage)))
      (.add (connect andy marriage))
      (.add (connect betty marriage))
      (.add (connect carl marriage)))
    (doto layout
      (.setConstraint andy (Rectangle. (Point. 10 10) (.getPreferredSize andy)))
      (.setConstraint betty (Rectangle. (Point. 230 10) (.getPreferredSize betty)))
      (.setConstraint carl (Rectangle. (Point. 120 120) (.getPreferredSize carl))))
    (doto canvas
      (.setBackground ColorConstants/white)
      (.setSize 365 280)
      (.setLayoutData (GridData. GridData/FILL_BOTH)))
    (.setContents lws root)))
