(ns image-measure.graphics-low
 "Low level functions and defs that relate to drawing and handling the state
  of points and lines.
  See state.clj for info on application state."
 (:require [clojure.string :as str]
           [clojure.set :as set]
           [clojure.tools.logging :as log]
           [seesaw.core :as sc]
           [seesaw.color :as sclr]
           [seesaw.graphics :as sg]
           [image-measure.geo :as geo]
           [image-measure.util :as util])
 (:import [java.awt.geom Line2D, Point2D]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Application State passed as param to some functions is a map
;; that is defined and discussed in image-measure.state
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; unit is pixels - a line of length less than this is still considered
;; a point when drawing polygons
(def point-line-tolerance 5)

(defn points-close-enough [^Point2D pt1 ^Point2D pt2]
  (and (>= point-line-tolerance (Math/abs (- (.getX pt1) (.getX pt2))))
       (>= point-line-tolerance (Math/abs (- (.getY pt1) (.getY pt2))))))

;; utilities for accessing lines and points

(defn get-points [^Line2D l]
  [(.getP1 l) (.getP2 l)])

(defn get-other-pt [^Line2D l ^Point2D pt]
  (if (= (.getP1 l) pt)
    (.getP2 l)
    (.getP1 l)))

(defn line2d-to-xy [^Line2D l]
  "Returns [x1 y1 x2 y2] from Line2D."
  (let [pts (get-points l)]
    (vec (flatten (map #(vec [(.getX %) (.getY %)]) pts)))))

(defn ^Line2D line-from-index [state index]
  (((state :lines) index) 0))

(defn lines-as-points [state]
  (vec (for [l (state :lines)]
         (get-points (l 0)))))

(defn lines-as-str [state]
  (str/join "\n" (for [l (state :lines)]
    (str/join "<->" (map #(format "(%s, %s)" (int (.getX %)) (int (.getY %)))
                         (get-points (l 0)))))))

(defn free-lines [state]
  "Returns vector of indices of lines not associated with any polygon.
   Always returns vector."
  (let [in-a-poly (set (concat (flatten (state :polygons))
                               (state :current-polygon)))
        all-lines (set (range (count (state :lines))))]
   (vec (set/difference all-lines in-a-poly))))

;; drawing utilities for lines and points

(defn draw-point
  ([x y r g style]
    (sg/draw g (sg/circle x y r) style)))

(defn draw-line-end-points [^Line2D line2d style g]
  (let [p1 (.getP1 line2d)
        p2 (.getP2 line2d)
        color (:foreground style)
        line-width (.getLineWidth (:stroke style))]
;    (prn "line-width: " line-width ", p1: " p1)
    (draw-point (.getX p1) (.getY p1) line-width g style)
    (draw-point (.getX p2) (.getY p2) line-width g style)))

(defn change-line-color [state index color]
  "Return new state with line at index having new color.
   color - color object or keyword accepted by seesaw (e.g. :green)"
  (let [old-style (get-in state [:lines index 1])
        new-style (sg/update-style old-style :foreground color)]
    (assoc-in state [:lines index 1] new-style)))

(defn update-line-style [state source]
  (let [style-key (sc/id-of source) new-value (sc/selection source)]
    (update-in state [:style] sg/update-style style-key new-value)))

(defn line-intersects [state index x y]
  "Returns true if the line with index index in state intersects with point
  (x, y).  This takes into account the line width based on the given line's
  stroke."
  (let [l (line-from-index state index)
        stroke (get-in (state :lines) [index 1 :stroke])
        width (.getLineWidth stroke)]
    (.intersects l (- x width) (- y width) (* 2 width) (* 2 width))))

(defn find-free-line [state x y]
  "If the point (x, y) is on a line not associated with a polygon in state,
   the index of the line is returned.  Otherwise nil.
   Tolerance is determined by the lines and points themselves (stroke, etc)."
  (let [free (free-lines state)
        found #(line-intersects state % x y)]
    (first (filter found free))))
