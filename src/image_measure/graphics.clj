(ns image-measure.graphics
  "Functions and defs that relate to drawing and handling the state of
   lines and polygons."
  (:require [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [clojure.tools.logging :as log]
            [seesaw.core :as sc]
            [seesaw.color :as sclr]
            [seesaw.graphics :as sg]
            [seesaw.behave :as behave])
  (:import [java.awt.geom Line2D]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Application State passed as param to these functions is a map
;; that is defined and discussed in image-measure.gui
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; unit is pixels - a line of length less than this is still considered
;; a point
(def point-line-tolerance 5)

(defn draw-point
  ([x y r g style]
    (sg/draw g (sg/circle x y r) style)))

(defn draw-line-end-points [line2d style g]
  (let [p1 (.getP1 line2d)
        p2 (.getP2 line2d)
        color (:foreground style)
        line-width (.getLineWidth (:stroke style))]
;    (prn "line-width: " line-width ", p1: " p1)
    (draw-point (.x p1) (.y p1) line-width g style)
    (draw-point (.x p2) (.y p2) line-width g style)))

;; good for sanity checks
(defn draw-a-red-x
  "Draw a red X on a widget with the given graphics context"
  [c g]
  (let [w          (sc/width c)
        h          (sc/height c)
        line-style (sg/style :foreground "#FF0000" :stroke 3 :cap :round)
        d 5]
    (sg/draw g
      (sg/line d d (- w d) (- h d)) line-style
      (sg/line (- w d) d d (- h d)) line-style)))

(defn start-new-polygon [state]
  "Returns new state with last line in state being first line in
   :current-polygon"
  (assoc state :current-polygon [(dec (count (state :lines)))]))

(defn add-latest-line-to-current-polygon [state]
  (assoc state :current-polygon
         (conj (state :current-polygon) (dec (count (state :lines))))))

(defn finish-polygon [state]
  (let [new-state (assoc state :polygons
                         (conj (state :polygons) (state :current-polygon)))]
    (assoc new-state :current-polygon nil)))

(defn ordered-polygon [state poly]
  "Given the app state and a polygon which is a vector of indices of :lines
   in the state map returns a vector of those indices representing the lines
   of the polygon in contiguous order.
   The polygon may be incomplete."
  nil)

(defn end-points [state partial-poly]
  "Given ordered partial polygon returns the 2 endpoints"
  nil)
;;****** after these funcs implemented need to use endpoints to
;;****** start drawing of any new line in polygon mode unless
;; starting or finishing the current polygon
;; don't worry about highlighting yet
;;*************


(defn start-new-line [state e]
  (let [p (.getPoint e)]
;    (log/info "state: " state)
    (assoc state
           :start-point [(.x p) (.y p)]
           :current-line [(sg/line (.x p) (.y p) (.x p) (.y p)) (:style state)])))

(defn drag-new-line [state e [dx dy]]
  (let [p (.getPoint e)
        [start-x start-y] (:start-point state)]
    (assoc state :current-line
           [(sg/line start-x start-y (.x p) (.y p)) (:style state)])))

(defn finish-new-line-orig [state e]
  (do
;    (log/info "state: " state)
    (-> state
      (update-in [:lines] conj (:current-line state))
      (assoc :current-line nil))))

(defn finish-new-line [state e]
  (do
;    (log/info "state: " state)
    (-> state
      (update-in [:lines] conj (:current-line state))
      (assoc :current-line nil))
    ))

(defn update-line-style
  [state source]
  (let [style-key (sc/id-of source) new-value (sc/selection source)]
    (update-in state [:style] sg/update-style style-key new-value)))

(defn delete-last-line [state]
  "Delete the last completed line in saved state.
   state - current state
   Returns new state."
  (update-in state [:lines] pop))
