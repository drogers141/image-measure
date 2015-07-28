(ns image-measure.graphics
 "Functions and defs that relate to drawing and handling the state of points
  and lines.  So lower level than polygons, as well as general higher level
  graphics functionality that doesn't seem to belong in any other namespace.
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

(defn start-new-line [state e]
  "Start a new line.
   If in polygon mode and there is a current polygon a new line can only
   be started from one of its endpoints."
  (let [p (.getPoint e)]
    (if (= (state :mode) :polygons)
      ;; polygon mode
      (do
        ;; if current poly - line must start from an endpoint
        (if (and (state :current-polygon) (pos? (count (state :current-polygon))))
            (let [endpts (end-points state (state :current-polygon))
                  close-pt (first (filter #(points-close-enough p %) endpts))]
              (log/debug "** start-new-line: current polygon **")
              (log/debug "endpts: " endpts "\nclose-pt: " close-pt)
              (if close-pt
                (assoc state
                       :start-point [(.getX close-pt) (.getY close-pt)]
                       :current-line [(sg/line (.getX close-pt) (.getY close-pt)
                                               (.getX close-pt) (.getY close-pt))
                                      (:style state)])
                state))
            (assoc state
               :start-point [(.getX p) (.getY p)]
               :current-line [(sg/line (.getX p) (.getY p) (.getX p) (.getY p)) (:style state)])))
      ;; lines mode
      (assoc state
         :start-point [(.getX p) (.getY p)]
         :current-line [(sg/line (.getX p) (.getY p) (.getX p) (.getY p)) (:style state)]))))

(defn drag-new-line [state e [dx dy]]
  "New line has been started - add to it."
  (if (state :current-line)
    (let [p (.getPoint e)
          [start-x start-y] (:start-point state)]
      (assoc state :current-line
             [(sg/line start-x start-y (.getX p) (.getY p)) (:style state)]))
    state))

(defn finish-new-line [state e]
  "Finish newly drawn line.
     * In polygon mode add it to the current polygon (possibly
        starting/finishing the polygon).
     * In lines mode change color to free-lines color"
  (let [new-state (-> state
                    (update-in [:lines] conj (:current-line state))
                    (assoc :current-line nil)
                    (assoc :start-point nil))]
    (if (state :current-line)
      (if (= (state :mode) :polygons)
        ;; handle polygon mode
        (if (state :current-polygon)
          (do
            (let [updated-state (add-latest-line-to-current-polygon new-state)
                  endpts (end-points updated-state (updated-state :current-polygon))]
              ;; close enough end points means completed polygon
              (log/debug "** finish-new-line: :current-polygon - endpts: " endpts)
              (cond
                (nil? endpts)
                (do
                  (log/debug "** finish-new-line: endpts nil")
                  new-state)
                ;; end points are close enough or exact - close polygon and finish it
                (or (zero? (count endpts)) (apply points-close-enough endpts))
                (do
                  (log/debug "** finish-new-line: finish poly - endpts: " endpts)
                  (finish-polygon (close-current-polygon updated-state)))
                :else
                (do
                  (log/debug "** finish-new-line: add line to poly")
                  updated-state))))
          ;; no :current-polygon so start it with this line
          (start-new-polygon new-state))
        ;; handle lines mode
        (let [c (state :free-lines-color)]
          (change-line-color new-state (dec (count (new-state :lines))) c)))
      ;; no current-line being drawn - no-op
      state)))

(defn update-line-style [state source]
  (let [style-key (sc/id-of source) new-value (sc/selection source)]
    (update-in state [:style] sg/update-style style-key new-value)))

(defn delete-last-line [state]
  "Delete the last completed line in saved state.
   Removes line from current-polygon if applicable.
   Does not delete line if in any finished polygons.
   state - current state
   Returns new state."
  ;; disregard if no lines
  (if (zero? (count (state :lines)))
    state
    (let [index (dec (count (state :lines)))
          inpoly #(not= -1 (.indexOf % index))]
      ;; don't delete if last line is in finished polygon
      (if (some inpoly (state :polygons))
        state
        (let [state2 (update-in state [:lines] pop)]
          (cond
            ;; if current polygon - delete line from it as well
            (and (get state2 :current-polygon)
                 (inpoly (state2 :current-polygon)))
            (assoc state2 :current-polygon
                   (vec (util/remove-first index (state :current-polygon))))
            ;; set selected free line to nil if deleted (doesn't handle label)
            (= index (state :selected-free-line))
            (assoc state2 :selected-free-line nil)
            :else
            state2))))))

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

(defn remove-line-label [state index]
  "Clear label on line with index index if there is one."
  (assoc-in state [:labels]
            (vec (filter #(not= (get % :line) index) (state :labels)))))

(defn remove-poly-center-label [state index]
  "Clear label at center of poly with index index if there is one."
  (assoc-in state [:labels]
            (vec (filter #(not= (get % :polygon) index) (state :labels)))))

(defn label-line [state ^java.awt.Graphics2D g index ^Label label]
  "Places label centered on midpoint of line with index index.
   Clears any previous label for line.
   g - graphics context for calculating geometry of label"
  (let [points (get-points (line-from-index state index))
        midpt (apply geo/midpoint (flatten (map #(vec [(.getX %) (.getY %)]) points)))
        l (centered-label g label (midpt 0) (midpt 1))
        state1 (remove-line-label state index)]
    (assoc-in state1 [:labels] (conj (state1 :labels) {:label l :line index}))))

(defn label-poly-center [state ^java.awt.Graphics2D g index ^Label label]
  "Places label centered on centroid of poly with index index.
   Clears any previous center label for polygon.
   g - graphics context for calculating geometry of label"
  (let [poly (get (:polygons state) index)
        points (ordered-poly-points state poly)
        [cx cy] (geo/centroid points)
        cl (centered-label g label cx cy)
        state1 (remove-poly-center-label state index)]
    (assoc-in state1 [:labels] (conj (state1 :labels) {:label cl :polygon index}))))

(defn label-poly-for-calculation [state ^java.awt.Graphics2D g index]
  "index - index of poly in state
   * label each line in polygon with its index
   * label center of polygon for area"
  (let [lines ((state :polygons) index)
        polylabel (label 0 0 18 "Area")
        state1 (label-poly-center state g index polylabel)]
    (loop [s state1
           rem-lines lines]
      (if (seq rem-lines)
        (let [i (first rem-lines)
              l (label 0 0 18 (str i))]
          (recur (label-line s g i l) (rest rem-lines)))
        s))))

(defn label-free-lines-for-calculation [state ^java.awt.Graphics2D g index]
  "index - index of selected line in state
   Clears all free-lines of labels and labels selected line."
  (let [free (free-lines state)
        l (label 0 0 18 "Selected")
        state1 (reduce remove-line-label state free)]
    (label-line state1 g index l)))

(defn label-poly-with-results [state ^java.awt.Graphics2D g index results]
  "Label polygon with results of calculating lengths and area.
   index - index of poly
   results - map with results of calculation in form
     {:area <poly-area> index1 <length-line1> index2 <length-line2>}
   ** Formatting with single digit precision for now **
   ** Defaulting to 18 as fontsize for now as well **"
  (let [lines ((state :polygons) index)
        make-label (fn [val] (label 0 0 18 (format "%.1f" val)))
        polylabel (make-label (results :area))
        state1 (label-poly-center state g index polylabel)]
    (loop [s state1
           rem-lines lines]
      (if (seq rem-lines)
        (let [i (first rem-lines)
              l (make-label (results i))]
          (recur (label-line s g i l) (rest rem-lines)))
        s))))

(defn label-free-lines-with-results [state ^java.awt.Graphics2D g results]
  "Label all free-lines with results of calculating lengths.
   results - map with results of calculation in form
     {index1 <length-line1> index2 <length-line2>}
   ** Formatting with single digit precision for now **
   ** Defaulting to 18 as fontsize for now as well **"
  (let [lines (free-lines state)
        make-label (fn [val] (label 0 0 18 (format "%.1f" val)))]
    (loop [s state
           rem-lines lines]
      (if (seq rem-lines)
        (let [i (first rem-lines)
              l (make-label (results i))]
          (recur (label-line s g i l) (rest rem-lines)))
        s))))
