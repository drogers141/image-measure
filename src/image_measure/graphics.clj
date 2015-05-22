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
            [seesaw.behave :as behave])
  (:import [java.awt.geom Line2D, Point2D, Point2D$Double]))

;; general utility, maybe collect into a namespace if we get more
(defn remove-first [item coll]
  "Returns collection coll with first element equal to item removed."
  (let [[n m] (split-with (partial not= item) coll)]
    (concat n (rest m))))

;(defmacro print-debug [x & vars]
;  "prints name of vars with values"
;  `(println (format "%s: %s" `'x ~x)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Application State passed as param to these functions is a map
;; that is defined and discussed in image-measure.gui
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; unit is pixels - a line of length less than this is still considered
;; a point in certain contexts (polygon mode)
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
  (assoc state :current-polygon [(dec (count (state :lines)))]))

(defn add-latest-line-to-current-polygon [state]
  (assoc state :current-polygon
         (conj (state :current-polygon) (dec (count (state :lines))))))

(defn finish-polygon [state]
  (let [new-state (assoc state :polygons
                         (conj (state :polygons) (state :current-polygon)))]
    (assoc new-state :current-polygon nil)))

(defn select-next [state forward ordered remaining]
  "select next line from remaining to add to ordered
   ordered, remaining - vectors of indices pointing to lines in
     @state :lines vector
   forward - boolean - if true adding next line to tail of ordered
      otherwise add to front
   return - updated [ordered remaining]"
  (let [select  ;; returns first line containing curr-pt in remaining
        (fn [curr-pt] (first (filter (fn [i] (some #(= curr-pt %) (get-points (line-from-index state i))))
                         remaining)))]
    (if forward
      (let [current-pt ((get-points (line-from-index state (last ordered))) 1)
            selected (select current-pt)]
        (if selected
          [(conj ordered selected) (disj remaining selected)]
          [ordered remaining]))
      (let [current-pt ((get-points (line-from-index state (first ordered))) 0)
            selected (select current-pt)]
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

(defn start-new-line-orig [state e]
  (let [p (.getPoint e)]
;    (log/info "state: " state)
    (assoc state
       :start-point [(.x p) (.y p)]
       :current-line [(sg/line (.x p) (.y p) (.x p) (.y p)) (:style state)])))

(defn start-new-line [state e]
  "Start a new line - in polygon mode if there is a current polygon
   a new line can only be started from one of its endpoints."
  (let [p (.getPoint e)]
    (log/info "state: " state)
    (if (= (state :mode) :polygons)
      (do
        (println "polygon mode")
        ;; if current poly - line must start from an endpoint
        (if (state :current-polygon)
          (let [endpts (end-points state (state :current-polygon))
                close-pt (first (filter #(points-close-enough p %) endpts))]
            (println "** current polygon **")
            (println "endpts: " endpts "\nclose-pt: " close-pt)
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

(defn finish-new-line-orig [state e]
  (do
;    (log/info "state: " state)
    (-> state
      (update-in [:lines] conj (:current-line state))
      (assoc :current-line nil))))

(defn finish-new-line [state e]
  "Finish newly drawn line.  In polygon mode add it to the current polygon
   (possibly starting/finishing the polygon)."
  (let [new-state (-> state
                    (update-in [:lines] conj (:current-line state))
                    (assoc :current-line nil))]
    (if (= (state :mode) :polygons)
      (if (state :current-polygon)
        (let [updated-state (add-latest-line-to-current-polygon new-state)]
          ;; nil end-points means completed polygon
          (if (nil? (end-points updated-state (updated-state :current-polygon)))
            (finish-polygon updated-state)
            updated-state))
        ;; no :current-polygon so start it with this line
        (start-new-polygon new-state))
      ;; not :polygons mode, just add the line
      new-state)))


(defn update-line-style
  [state source]
  (let [style-key (sc/id-of source) new-value (sc/selection source)]
    (update-in state [:style] sg/update-style style-key new-value)))

(defn delete-last-line [state]
  "Delete the last completed line in saved state.
   state - current state
   Returns new state."
  (update-in state [:lines] pop))
