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
           [seesaw.font :as sfont]
           [image-measure.geo :as geo])
 (:import [java.awt.geom Line2D, Point2D, Point2D$Double]
          [java.awt.Color]))

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

(defn ordered-poly-points [state poly]
  "Same input as ordered-polygon - returns the unique points in order
   as doubles in the form [[x1 y1] [x2 y2] ..]"
  (let [ordered (ordered-polygon state poly)
        all-pts (map #(get-points (line-from-index state %)) ordered)
        first-pt (if ((set (second all-pts)) (first (first all-pts)))
                    (second (first all-pts))
                    (first (first all-pts)))
         second-pt (get-other-pt (line-from-index state (first ordered))
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
        (if (and (state :current-polygon) (pos? (count (state :current-polygon))))
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
  (if (state :current-line)
    (let [p (.getPoint e)
          [start-x start-y] (:start-point state)]
      (assoc state :current-line
             [(sg/line start-x start-y (.x p) (.y p)) (:style state)]))
    state))

(defn close-current-polygon [state]
  "Call this if you have points-close-enough with the end-points of the
   :current-polygon before calling finish-polygon.
   Replaces the last line with one that has as its last point the shared
   point in the polygon.
   Returns updated state."
  (let [endpts (end-points state (state :current-polygon))]
    ;; nothing to do if end points are already the same point
    (if (zero? (count endpts))
      state
      (let [last-line-index ((state :current-polygon) (dec (count (state :current-polygon))))
            last-line (line-from-index state last-line-index)
            last-line-pts (get-points last-line)
            keep-pt (first (filter #(not (points-close-enough (endpts 0) %)) last-line-pts))
            other-pt (first (filter #(not= (get-other-pt last-line keep-pt) %) endpts))
            new-line (sg/line (.getX keep-pt) (.getY keep-pt) (.getX other-pt) (.getY other-pt))
            new-state (assoc-in state [:lines last-line-index 0] new-line)]
        new-state))))

(defn finish-new-line [state e]
  "Finish newly drawn line.  In polygon mode add it to the current polygon
   (possibly starting/finishing the polygon)."
  (let [new-state (-> state
                    (update-in [:lines] conj (:current-line state))
                    (assoc :current-line nil)
                    (assoc :start-point nil))]
    (if (state :current-line)
      (if (state :current-polygon)
        (do
          (let [updated-state (add-latest-line-to-current-polygon new-state)
                endpts (end-points updated-state (updated-state :current-polygon))]
            ;; close enough end points means completed polygon
            (log/info "** finish-new-line: :current-polygon - endpts: " endpts)
            (cond
              (nil? endpts)
              (do
                (log/info "** finish-new-line: endpts nil")
                new-state)
              ;; end points are close enough or exact - close polygon and finish it
              (or (zero? (count endpts)) (apply points-close-enough endpts))
              (do
                (log/info "** finish-new-line: finish poly - endpts: " endpts)
                (finish-polygon (close-current-polygon updated-state)))
              :else
              (do
                (log/info "** finish-new-line: add line to poly")
                updated-state))))
        ;; no :current-polygon so start it with this line
        (start-new-polygon new-state))
      ;; no current-line being drawn - no-op
      state)))



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

;; A label painted in java2d with a default font.
;; x, y - top left outside
;; fg, bg - foreground and background colors
(defrecord Label [x y fontsize ^String text
                  ^java.awt.Color fg ^java.awt.Color bg])

(defn label [x y fontsize text]
  "Convenience - black and white label."
  (Label. x y fontsize text (sclr/color :black) (sclr/color :white)))

(defn label-geometry [^java.awt.Graphics2D g ^Label l]
  "Returns map of geometry of label created from string text at x, y
   g - graphics context for calculating geometry of label
   x, y - top left of outside of label
   fontsize - size of font
   returns map { :x <x> :y <y>
      :width <full width> :height <full height>
      :text-height <height of string> :text-width <width of string>
      :string-x <x for drawString> :string-y <y for drawString> }

   Note - x + width and y + height appear to not take into account stroke
   of outside rectangle of label - ie 2-3 pixels shy"
  (let [font (sfont/font :name :sans-serif :size (:fontsize l))
       metrics (.getFontMetrics g font)
       twidth (.stringWidth metrics (:text l))
       theight (.getHeight metrics)
       wpad (* 0.25 theight)
       hpad (* 0.125 theight)]
    {:x (:x l) :y (:y l)
     :width (int (+ twidth (* 2 wpad))) :height (int (+ theight (* 2 hpad)))
     :text-width twidth :text-height theight
     :string-x (int (+ (:x l) wpad)) :string-y (int (+ (:y l) theight))}))

(defn paint-label [^java.awt.Graphics2D g ^Label l]
  "g - graphics context for calculating geometry of label "
  (let [stroke (sg/stroke :width 2 :join :round)
       font (sfont/font :name :sans-serif :size (:fontsize l))
       geo (label-geometry g l)]
    (doto g
      (.setFont font)
      (.setColor (:bg l))
      (.fillRect (:x l) (:y l) (geo :width) (geo :height))
      (.setColor (:fg l))
      (.drawString (:text l) (geo :string-x) (geo :string-y))
      (.setStroke stroke)
      (.drawRect (:x l) (:y l) (geo :width) (geo :height)))))

(defn centered-label [^java.awt.Graphics2D g ^Label l cx cy]
  "Returns copy of Label l with its x, y adjusted so it is centered
   on cx, cy
   g - graphics context for calculating geometry of label"
  (let [geo (label-geometry g l)
        x (int (- cx (* 0.5 (geo :width))))
        y (int (- cy (* 0.5 (geo :height))))]
    (Label. x y (:fontsize l) (:text l) (:fg l) (:bg l))))

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
