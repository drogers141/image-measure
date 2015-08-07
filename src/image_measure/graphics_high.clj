(ns image-measure.graphics-high
 "Higher level graphics functionality. See state.clj for info on application
  state."
 (:require [clojure.tools.logging :as log]
           [seesaw.graphics :as sg]
           [image-measure.geo :as geo]
           [image-measure.util :as util]
           [image-measure.graphics-low :as graphics-low]
           [image-measure.polygon :as polygon]
           [image-measure.label :as label])
 (:import [java.awt.geom Line2D, Point2D]))

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
            (let [endpts (polygon/end-points state (state :current-polygon))
                  close-pt (first (filter #(graphics-low/points-close-enough p %) endpts))]
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
            (let [updated-state (polygon/add-latest-line-to-current-polygon new-state)
                  endpts (polygon/end-points updated-state (updated-state :current-polygon))]
              ;; close enough end points means completed polygon
              (log/debug "** finish-new-line: :current-polygon - endpts: " endpts)
              (cond
                (nil? endpts)
                (do
                  (log/debug "** finish-new-line: endpts nil")
                  new-state)
                ;; end points are close enough or exact - close polygon and finish it
                (or (zero? (count endpts)) (apply graphics-low/points-close-enough endpts))
                (do
                  (log/debug "** finish-new-line: finish poly - endpts: " endpts)
                  (polygon/finish-polygon (polygon/close-current-polygon updated-state)))
                :else
                (do
                  (log/debug "** finish-new-line: add line to poly")
                  updated-state))))
          ;; no :current-polygon so start it with this line
          (polygon/start-new-polygon new-state))
        ;; handle lines mode
        (let [c (state :free-lines-color)]
          (graphics-low/change-line-color new-state (dec (count (new-state :lines))) c)))
      ;; no current-line being drawn - no-op
      state)))

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

(defn remove-line-label [state index]
  "Clear label on line with index index if there is one."
  (assoc-in state [:labels]
            (vec (filter #(not= (get % :line) index) (state :labels)))))

(defn remove-poly-center-label [state index]
  "Clear label at center of poly with index index if there is one."
  (assoc-in state [:labels]
            (vec (filter #(not= (get % :polygon) index) (state :labels)))))

(defn label-line [state ^java.awt.Graphics2D g index label]
  "Places label centered on midpoint of line with index index.
   Clears any previous label for line.
   g - graphics context for calculating geometry of label"
  (let [points (graphics-low/get-points (graphics-low/line-from-index state index))
        midpt (apply geo/midpoint (flatten (map #(vec [(.getX %) (.getY %)]) points)))
        l (label/centered-label g label (midpt 0) (midpt 1))
        state1 (remove-line-label state index)]
    (assoc-in state1 [:labels] (conj (state1 :labels) {:label l :line index}))))

(defn label-poly-center [state ^java.awt.Graphics2D g index label]
  "Places label centered on centroid of poly with index index.
   Clears any previous center label for polygon.
   g - graphics context for calculating geometry of label"
  (let [poly (get (:polygons state) index)
        points (polygon/ordered-poly-points state poly)
        [cx cy] (geo/centroid points)
        cl (label/centered-label g label cx cy)
        state1 (remove-poly-center-label state index)]
    (assoc-in state1 [:labels] (conj (state1 :labels) {:label cl :polygon index}))))

(defn label-poly-for-calculation [state ^java.awt.Graphics2D g index]
  "index - index of poly in state
   * label each line in polygon with its index
   * label center of polygon for area"
  (let [lines ((state :polygons) index)
        polylabel (label/label 0 0 18 "Area")
        state1 (label-poly-center state g index polylabel)]
    (loop [s state1
           rem-lines lines]
      (if (seq rem-lines)
        (let [i (first rem-lines)
              l (label/label 0 0 18 (str i))]
          (recur (label-line s g i l) (rest rem-lines)))
        s))))

(defn label-free-lines-for-calculation [state ^java.awt.Graphics2D g index]
  "index - index of selected line in state
   Clears all free-lines of labels and labels selected line."
  (let [free (graphics-low/free-lines state)
        l (label/label 0 0 18 "Selected")
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
        make-label (fn [val] (label/label 0 0 18 (format "%.1f" val)))
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
  (let [lines (graphics-low/free-lines state)
        make-label (fn [val] (label/label 0 0 18 (format "%.1f" val)))]
    (loop [s state
           rem-lines lines]
      (if (seq rem-lines)
        (let [i (first rem-lines)
              l (make-label (results i))]
          (recur (label-line s g i l) (rest rem-lines)))
        s))))
