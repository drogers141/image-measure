(ns image-measure.graphics
  "Functions and defs that relate to drawing and handling the state of
   lines and polygons."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]]
            [clojure.tools.logging :as log]
            [seesaw.core :as sc]
            [seesaw.color :as sclr]
            [seesaw.graphics :as sg]
            [seesaw.behave :as behave]
            [seesaw.font :as sfont])
  (:import [java.awt.geom Line2D, Point2D, Point2D$Double]))

;; general utilities, maybe collect into a namespace if we get more
(defn remove-first [item coll]
  "Returns collection coll with first element equal to item removed."
  (let [[n m] (split-with (partial not= item) coll)]
    (concat n (rest m))))

(defn indices [pred coll]
  "Returns indices in collection coll of elements that satisfy predicate pred."
  (keep-indexed #(when (pred %2) %1) coll))

;(defmacro print-debug [x & vars]
;  "prints name of vars with values"
;  `(println (format "%s: %s" `'x ~x)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Application State passed as param to these functions is a map
;; that is defined and discussed in image-measure.state
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; unit is pixels - a line of length less than this is still considered
;; a point when drawing polygons
(def point-line-tolerance 5)

(defn points-close-enough [^Point2D pt1 ^Point2D pt2]
  (and (>= point-line-tolerance (Math/abs (- (.x pt1) (.x pt2))))
       (>= point-line-tolerance (Math/abs (- (.y pt1) (.y pt2))))))

;; utilities for accessing lines and points

(defn get-points [^Line2D l]
  [(.getP1 l) (.getP2 l)])

(defn get-other-pt [^Line2D l ^Point2D pt]
  (if (= (.getP1 l) pt)
    (.getP2 l)
    (.getP1 l)))

(defn ^Line2D line-from-index [state index]
  (((state :lines) index) 0))

(defn lines-as-points [state]
  (vec (for [l (state :lines)]
         (get-points (l 0)))))

(defn lines-as-str [state]
  (str/join "\n" (for [l (state :lines)]
    (str/join "<->" (map #(format "(%s, %s)" (int (.getX %)) (int (.getY %)))
                         (get-points (l 0)))))))

;; drawing utilities for lines and points

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

(defn change-line-color [state index color]
  "Return new state with line at index having new color.
   color - color object or keyword accepted by seesaw (e.g. :green)"
  (let [old-style (get-in state [:lines index 1])
        new-style (sg/update-style old-style :foreground color)]
    (assoc-in state [:lines index 1] new-style)))

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

;; polygon utilities

(defn start-new-polygon [state]
  "Returns new state with last line in state being first line in
   :current-polygon"
  (log/info "** start-new-polygon **")
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
    (reduce #(change-line-color %1 %2 c) state2 poly)))

(defn select-next [state forward ordered remaining]
  "Helper for ordered-polygon: select next line from remaining set to add to
   those previously ordered
   ordered, remaining - vector and set, respectively, of indices pointing to
   lines in @state :lines vector
   forward - boolean - if true adding next line to tail of ordered
      otherwise add to front
   return - updated [ordered remaining]"
  (let [select  ;; returns first line containing curr-pt in remaining
        (fn [curr-pt] (first (filter (fn [i] (some #(= curr-pt %) (get-points (line-from-index state i))))
                         remaining)))]
    (if forward
      (let [curr-pts (get-points (line-from-index state (last ordered)))
            selected (some identity (map select curr-pts))]
        (if selected
          [(conj ordered selected) (disj remaining selected)]
          [ordered remaining]))
      (let [curr-pts (get-points (line-from-index state (first ordered)))
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
;        (println "ordered: " ordered "\nremaining: " remaining)
;        (if (not= (count new-ordered) (count ordered))
;          (println "forward")
;          (println "reverse"))
        (if (not= (count new-ordered) (count ordered))
          (recur new-ordered new-remaining)
          (let [[new-o new-r] (select-next state false ordered remaining)]
            (recur new-o new-r)))))))

(defn end-points [state partial-poly]
  "Given partial polygon returns the 2 endpoints in a vector.
   Returns empty vector for complete polygon."
  (let [ordered (ordered-polygon state partial-poly)
         counts (frequencies (flatten (for [l (map #(line-from-index state %) ordered)]
                                        (get-points l))))]
     (vec (map first (filter (fn [[_ v]] (= 1 v)) counts)))))

(defn start-new-line [state e]
  "Start a new line - in polygon mode if there is a current polygon
   a new line can only be started from one of its endpoints."
  (let [p (.getPoint e)]
;    (log/info "state: " state)
    (if (= (state :mode) :polygons)
      (do
;        (log/info "polygon mode")
        ;; if current poly - line must start from an endpoint
        (if (state :current-polygon)
          (let [endpts (end-points state (state :current-polygon))
                close-pt (first (filter #(points-close-enough p %) endpts))]
            (log/info "** start-new-line: current polygon **")
            (log/info "endpts: " endpts "\nclose-pt: " close-pt)
;            (println "endpts: " endpts "\nclose-pt: " close-pt)
            (if close-pt
              (assoc state
                     :start-point [(.x close-pt) (.y close-pt)]
                     :current-line [(sg/line (.x close-pt) (.y close-pt)
                                             (.x close-pt) (.y close-pt))
                                    (:style state)])
              state))
          (assoc state
             :start-point [(.x p) (.y p)]
             :current-line [(sg/line (.x p) (.y p) (.x p) (.y p)) (:style state)])))
      ;; not polygon mode, just start new line
      (assoc state
             :start-point [(.x p) (.y p)]
             :current-line [(sg/line (.x p) (.y p) (.x p) (.y p)) (:style state)]))))

(defn drag-new-line [state e [dx dy]]
  "New line has been started - add to it."
  (let [p (.getPoint e)
        [start-x start-y] (:start-point state)]
    (assoc state :current-line
           [(sg/line start-x start-y (.x p) (.y p)) (:style state)])))

(defn close-current-polygon [state]
  "Call this if you have points-close-enough with the end-points of the
   :current-polygon before calling finish-polygon.
   Replaces the last line with one that has as its last point the shared
   point in the polygon.
   Returns updated state."
  (let [endpts (end-points state (state :current-polygon))
        last-line-index ((state :current-polygon) (dec (count (state :current-polygon))))
        last-line (line-from-index state last-line-index)
        last-line-pts (get-points last-line)
        keep-pt (first (filter #(not (points-close-enough (endpts 0) %)) last-line-pts))
        other-pt (first (filter #(not= (get-other-pt last-line keep-pt) %) endpts))
        new-line (sg/line (.getX keep-pt) (.getY keep-pt) (.getX other-pt) (.getY other-pt))
        new-state (assoc-in state [:lines last-line-index 0] new-line)]
    ;; nothing to do if end points are already the same point
    (if (= (endpts 0) (endpts 1))
      state
      new-state)))

(defn finish-new-line [state e]
  "Finish newly drawn line.  In polygon mode add it to the current polygon
   (possibly starting/finishing the polygon)."
  (let [new-state (-> state
                    (update-in [:lines] conj (:current-line state))
                    (assoc :current-line nil))]
    (if (= (state :mode) :polygons)
      (if (state :current-polygon)
        (do
          (let [updated-state (add-latest-line-to-current-polygon new-state)
                endpts (end-points updated-state (updated-state :current-polygon))]
            ;; close enough end points means completed polygon
            (log/info "** finish-new-line: :current-polygon **")
            (log/info "endpts: " endpts)
            (cond
              (nil? endpts)
              (do
                (log/info "** finish-new-line: endpts nil")
                new-state)
              (apply points-close-enough endpts)
                ;; end points are close enough - close polygon and finish it
              (do
                (log/info "** finish-new-line: finish poly - endpts: " endpts)
                (finish-polygon (close-current-polygon updated-state)))
              :else
              (do
                (log/info "** finish-new-line: add line to poly")
                updated-state))))
        ;; no :current-polygon so start it with this line
        (start-new-polygon new-state))
      ;; not :polygons mode, just add the line
      new-state)))


(defn update-line-style [state source]
  (let [style-key (sc/id-of source) new-value (sc/selection source)]
    (update-in state [:style] sg/update-style style-key new-value)))

(defn delete-last-line [state]
  "Delete the last completed line in saved state.
   Removes line from current-polygon if applicable.

   Todo maybe:
      - deleting last line from :current-polygon leaves it as an empty vec
        - not nil - might want to change
      - deleting from a closed polygon is not handled as far as polygons go

   state - current state
   Returns new state."
  (if (< 0 (count (state :lines)))
    (let [index (dec (count (state :lines)))
  ;        new-current-poly (vec (remove-first index (state :current-polygon)))
          state2 (update-in state [:lines] pop)]
      (if (get state2 :current-polygon)
        (assoc state2 :current-polygon
               (vec (remove-first index (state :current-polygon))))
        state2))
    state))

(defn line-intersects [state index x y]
  "Returns true if the line with index index in state intersects with point
  (x, y).  This takes into account the line width based on the given line's
  stroke."
  (let [l (line-from-index state index)
        stroke (get-in (state :lines) [index 1 :stroke])
        width (.getLineWidth stroke)]
    (.intersects l (- x width) (- y width) (* 2 width) (* 2 width))))

(defn find-polygon [state x y]
  "If the point (x, y) is on a line or point defining a polygon in state,
   the index of the polygon in :polygons is returned.  Otherwise nil.
   Tolerance is determined by the lines and points themselves (stroke, etc)."
  (when (< 0 (count (state :polygons)))
    (let [found #(line-intersects state % x y)
          in-poly #(some found %)]
      (first (indices in-poly (state :polygons))))))

(defn label-geometry [g x y fontsize text]
  "Returns map of geometry of label created from string text at x, y
   g - graphics context
   x, y - top left of outside of label
   fontsize - size of font
   returns map { :x <x> :y <y>
      :width <full width> :height <full height>
      :text-height <height of string> :text-width <width of string>
      :string-x <x for drawString> :string-y <y for drawString> }

   Note - x + width and y + height appear to not take into account stroke
   of outside rectangle of label - ie 2-3 pixels shy"
  (let [font (sfont/font :name :sans-serif :size fontsize)
       metrics (.getFontMetrics g font)
       twidth (.stringWidth metrics text)
       theight (.getHeight metrics)
       wpad (* 0.25 theight)
       hpad (* 0.125 theight)]
    {:x x :y y :width (int (+ twidth (* 2 wpad))) :height (int (+ theight (* 2 hpad)))
     :text-width twidth :text-height theight
     :string-x (int (+ x wpad)) :string-y (int (+ y theight))}))

(defn paint-label [^java.awt.Graphics2D g x y text fg bg]
  (let [stroke (sg/stroke :width 2 :join :round)
       fontsize 18
       font (sfont/font :name :sans-serif :size fontsize)
       geo (label-geometry g x y fontsize text)]
    (doto g
      (.setFont font)
      (.setColor bg)
      (.fillRect x y (geo :width) (geo :height))
      (.setColor fg)
      (.drawString text (geo :string-x) (geo :string-y))
      (.setStroke stroke)
      (.drawRect x y (geo :width) (geo :height)))))
