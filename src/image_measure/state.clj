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
;;          - once polygons are finished they are stored as indices in
;;             :polygons
;; Once a line is associated with a polygon it is always associated with it
;; Lines and polygons may be in state at the same time - they will have
;;    different colors, and calculation occurs for either a selected polygon
;;    or the set of all free lines
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
  ;; each label entry contains the label and an index of either
  ;; a line if it is a line label, or a polygon if it is an area label
  ;; ie one of:
  ;; {:label <Label record> :line <index>}
  ;; {:label <Label record> :polygon <index>}
  :labels []
  ;; current style - graphics.Style
  :style  (sg/style :color :black :stroke nil)
  ;; color of lines while drawing the current polygon
  :current-polygon-color nil
  ;; color of lines in finished polygons that you can select for calculation
  :finished-polygon-color (color/color 0 216 0)
  ;; color of finished lines not associated with a polygon
  ;; (drawn in :lines mode)
  :free-lines-color (color/color 0 168 255)
  ;; blue 0 128 255, 0 168 255  purple 128 0 255
  ;; magenta 204 0 204
  ;; selected line to give length to calculate for in :lines mode
  :selected-free-line nil
  ;; scaled lengths of most recently calculated lines from polygons
  ;; - enables automatic calculation of free-lines
  ;; line index is key, scaled length is val
  :latest-calculated-lines {}

  ;; mode - either :lines or :polygons
  ;;    what happens when you draw or click the calculate button
  ;; :polygons - in :draw click-mode all lines drawn are part of the
  ;;     current polygon - see below
  ;;     - in :calculate click-mode, selecting a polygon by clicking on it
  ;;       enables calculation, which is restricted to that polygon
  ;; :lines - in :draw click-mode, lines are drawn independently of each other
  ;;        and have a different color from current or finished polygons
  ;;        - in :calculate mode a free line (not in a polygon) must be
  ;;          selected to enable calculation - which labels the other free lines
  :mode :polygons
  ;; click-mode - either :draw or :calculate
  ;;    what happens when you click on the image
  ;; :draw - click and drag to create the :current-polygon
  ;;     once finished, :current-polygon goes to :polygons
  ;; :calculate - if there is at least one polygon in :polygons
  ;;     clicking on a line or point belonging to one will
  ;;     show empty labels for the lines and area calculations
  ;;     the user can then fill in whatever data and click "Calculate"
  :click-mode :draw})

;; GLOBAL APPLICATION STATE
(def state (atom (clean-state)))
