(ns image-measure.polygon
  "Polygon utilities.
   Polygons are defined as a vector of line indices in the application state
   map.
   See graphics_low.clj for lower level graphics functionality and state.clj for an
   idea of the application state being passed around."
  (:require [clojure.tools.logging :as log]
           [seesaw.graphics :as sg]
           [image-measure.util :as util]
           [image-measure.graphics-low :as graphics]))

(defn start-new-polygon [state]
  "Returns new state with last line in state being first line in
   :current-polygon"
  (log/debug "** start-new-polygon **")
  (assoc state :current-polygon [(dec (count (state :lines)))]))

(defn add-latest-line-to-current-polygon [state]
  (assoc state :current-polygon
         (conj (state :current-polygon) (dec (count (state :lines))))))

(defn finish-polygon [state]
  "Finish current polygon:
      * move the vector of line indices from :current-polygon to :polygons
      * change the line colors for that polygon to :finished-polygon-color"
  (let [state1 (assoc state :polygons
                      (conj (state :polygons) (state :current-polygon)))
        state2 (assoc state1 :current-polygon nil)
        poly (get (state2 :polygons) (dec (count (state2 :polygons))))
        c (state :finished-polygon-color)]
    (reduce #(graphics/change-line-color %1 %2 c) state2 poly)))

(defn select-next [state forward ordered remaining]
  "Helper for ordered-polygon: select next line from remaining set to add to
   those previously ordered
   ordered, remaining - vector and set, respectively, of indices pointing to
   lines in @state :lines vector
   forward - boolean - if true adding next line to tail of ordered
      otherwise add to front
   return - updated [ordered remaining]"
  (let [select  ;; returns first line containing curr-pt in remaining
        (fn [curr-pt] (first (filter (fn [i] (some #(= curr-pt %) (graphics/get-points (graphics/line-from-index state i))))
                         remaining)))]
    (if forward
      (let [curr-pts (graphics/get-points (graphics/line-from-index state (last ordered)))
            selected (some identity (map select curr-pts))]
        (if selected
          [(conj ordered selected) (disj remaining selected)]
          [ordered remaining]))
      (let [curr-pts (graphics/get-points (graphics/line-from-index state (first ordered)))
            selected (some identity (map select curr-pts))]
        [(into [selected] ordered) (disj remaining selected)]))))

(defn ordered-polygon [state poly]
  "Given the app state and a polygon which is a vector of indices of :lines
   in the state map returns a vector of those indices representing the lines
   of the polygon in contiguous order.
   The polygon may be incomplete."
  (loop [ordered [(first poly)]
         remaining (set (rest poly))]
    (if (not (seq remaining))
      ordered
      (let [[new-ordered new-remaining] (select-next state true ordered remaining)]
        (if (not= (count new-ordered) (count ordered))
          (recur new-ordered new-remaining)
          (let [[new-o new-r] (select-next state false ordered remaining)]
            (recur new-o new-r)))))))

(defn ordered-poly-points [state poly]
  "Same input as ordered-polygon - returns the unique points in order
   as doubles in the form [[x1 y1] [x2 y2] ..]"
  (let [ordered (ordered-polygon state poly)
        all-pts (map #(graphics/get-points (graphics/line-from-index state %)) ordered)
        first-pt (if ((set (second all-pts)) (first (first all-pts)))
                    (second (first all-pts))
                    (first (first all-pts)))
         second-pt (graphics/get-other-pt (graphics/line-from-index state (first ordered))
                                   first-pt)]
    (loop [points [first-pt second-pt]
           remaining (rest all-pts)]
      (if (seq remaining)
        (let [line (first remaining)
              is-first (= -1 (.indexOf points (line 0)))]
          (if is-first
            (recur (conj points (line 0)) (rest remaining))
            (recur (conj points (line 1)) (rest remaining))))
        ;; last point is repeated - easier to just remove here than rework
        (let [v (vec (map #(vec [(.getX %) (.getY %)]) points))]
          (subvec v 0 (dec (count v))))))))

(defn end-points [state partial-poly]
  "Given partial polygon returns the 2 endpoints in a vector.
   Returns empty vector for complete polygon."
  (let [ordered (ordered-polygon state partial-poly)
         counts (frequencies (flatten (for [l (map #(graphics/line-from-index state %) ordered)]
                                        (graphics/get-points l))))]
     (vec (map first (filter (fn [[_ v]] (= 1 v)) counts)))))

(defn close-current-polygon [state]
  "Call this if you have graphics/points-close-enough with the end-points of the
   :current-polygon before calling finish-polygon.
   Replaces the last line with one that has as its last point the shared
   point in the polygon.
   Returns updated state."
  (let [endpts (end-points state (state :current-polygon))]
    ;; nothing to do if end points are already the same point
    (if (zero? (count endpts))
      state
      (let [last-line-index ((state :current-polygon) (dec (count (state :current-polygon))))
            last-line (graphics/line-from-index state last-line-index)
            last-line-pts (graphics/get-points last-line)
            keep-pt (first (filter #(not (graphics/points-close-enough (endpts 0) %)) last-line-pts))
            other-pt (first (filter #(not= (graphics/get-other-pt last-line keep-pt) %) endpts))
            new-line (sg/line (.getX keep-pt) (.getY keep-pt) (.getX other-pt) (.getY other-pt))
            new-state (assoc-in state [:lines last-line-index 0] new-line)]
        new-state))))

(defn find-polygon [state x y]
  "If the point (x, y) is on a line or point defining a polygon in state,
   the index of the polygon in :polygons is returned.  Otherwise nil.
   Tolerance is determined by the lines and points themselves (stroke, etc)."
  (when (< 0 (count (state :polygons)))
    (let [found #(graphics/line-intersects state % x y)
          in-poly #(some found %)]
      (first (util/indices in-poly (state :polygons))))))
