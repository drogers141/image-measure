(ns image-measure.state
  (:require [clojure.pprint :refer [pprint]]
            [seesaw.graphics :as sg]
            [seesaw.color :as color]))

;;;;;;;;;;; Application State ;;;;;;;;;;
;; modes - domain terminology
;; mode of drawing
;; affects how lines drawn on image are considered
;; Lines - lines drawn are considered unconnected and a set
;;       - lengths can be calculated based on a scale length for one line
;; Polygons - lines drawn are connected to active polygon
;;          - active polygon is finished when line closes it by connecting
;;              2 points
;;          - only 1 active polygon at a time - kept in state as
;;          :current-polygon
;;
(defn clean-state []
  {
  ;; lines [[Line2D graphics.Style] ..]
  ;; graphics.Style {:foreground <Color> :background <Color>
  ;;                 :stroke <Stroke> :font}
  :lines []
  ;; polygons are implemented as references to lines in the lines vector
  ;; vectors of indices
  :current-polygon nil
  :polygons []
  ;; one of :polygons
  :selected-polygon nil
  ;; custom rendered labels at midpoints of lines or in the centroid
  ;; of a polygon displaying numeric values
  ;; each label is described by a map with keys
  ;; {x  y  fontsize  text  polygon <index of poly>}
  :labels []
  ;; current style - graphics.Style
  :style  (sg/style :color :black :stroke nil)
  ;; color of lines while drawing the current polygon
  :current-polygon-color nil
  ;; color of lines in finished polygons that you can select for calculation
  :finished-polygon-color (color/color 0 216 0)
  ;; current mode - one of :lines :polygons
  :mode nil
  ;; click-mode - either :draw or :calculate
  ;; what happens when you click on the image
  ;; :draw - click and drag to create the :current-polygon
  ;;     once finished, :current-polygon goes to :polygons
  ;; :calculate - if there is at least one polygon in :polygons
  ;;     clicking on a line or point belonging to one will
  ;;     show empty labels for the lines and area calculations
  ;;     the user can then fill in whatever data and click "Calculate"
  ;;
  ;; will phase out :lines or :polygons values for :mode and may rename
  ;; this to :mode
  :click-mode :draw})

;; GLOBAL APPLICATION STATE
(def state (atom (clean-state)))
