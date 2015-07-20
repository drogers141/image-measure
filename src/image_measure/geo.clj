(ns image-measure.geo
 "Geometry and math functionality - pure functions."
 (:require [clojure.math.numeric-tower :as math])
 (:import [java.awt.geom Line2D]))


(defn distance
  "Distance between two points."
  ([x1 y1 x2 y2]
    (java.lang.Math/sqrt
      (+ (math/expt (- x2 x1) 2) (math/expt (- y2 y1) 2)))))

(defn midpoint
  "Midpoint of line segment between two points."
  ([x1 y1 x2 y2]
    (let [x (/ (+ x1 x2) 2.0)
          y (/ (+ y1 y2) 2.0)]
      [x y])))

(defn area [polygon]
  "Area enclosed in a non-self-intersecting polygon.
   The Surveyor's Area, or Shoestring formula..

   polygon - [[x1 y1] [x2 y2] ..], where each pair is a point and the
       order defines a traversal in order around polygon in either
       direction"
  (let [wrapped (conj polygon (first polygon))]
    (loop [remaining wrapped sum 0.0]
;      (println (first remaining) ", " (second remaining))
      (if (not (second remaining))
        (Math/abs (* sum 0.5))
        (let [[x1 y1] (first remaining)
              [x2 y2] (second remaining)
              component (- (* x1 y2) (* y1 x2))]
;          (println x1 y1 ", " x2 y2)
          (recur (rest remaining) (+ sum component)))))))

(defn centroid [polygon]
  "Centroid of non-self-intersecting polygon.

   polygon - [[x1 y1] [x2 y2] ..], where each pair is a point and the
       order defines a traversal in order around polygon in either
       direction"
  (let [wrapped (conj polygon (first polygon))]
    (loop [remaining wrapped xsum 0.0 ysum 0.0]
      (if (not (second remaining))
        (vec (map #(* (/ 1.0 (* 6.0 (area polygon))) %) [xsum ysum]))
        (let [[x1 y1] (first remaining)
              [x2 y2] (second remaining)
              cross-diff (- (* x1 y2) (* y1 x2))
              xcomp (* (+ x1 x2) cross-diff)
              ycomp (* (+ y1 y2) cross-diff)]
;          (println (format "[%s %s] [%s %s] cross-diff = %s  xcomp = %s  ycomp = %s"
;                           x1 y1 x2 y2 cross-diff xcomp ycomp))
          (recur (rest remaining) (+ xsum xcomp) (+ ysum ycomp)))))))



(defn calibrate-scale [known-line length lines]
 "Given a line and a length for that line in some units, returns
   the length of each of a sequence of lines in those units.

   known-line - [x1 y1 x2 y2]
   lines - [[x1 y1 x2 y2] .. [xn yn xn+1 yn+1]]
   return - [length_1 .. length_n] - length of each line in lines"
 (let [pixel-len (apply distance known-line)
       scale-factor (/ length pixel-len)]
   (vec (map #(* scale-factor (apply distance %)) lines))))
