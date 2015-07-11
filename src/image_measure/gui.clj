(ns image-measure.gui
  (:require [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [clojure.tools.logging :as log]
            [seesaw.core :as sc]
            [seesaw.color :as sclr]
            [seesaw.graphics :as sg]
            [seesaw.behave :as behave]
            [image-measure.graphics :as g])
  (:import [javax.swing JFrame JPanel ImageIcon]
           [java.awt.geom Line2D]))

(def colors [:black :white :red :blue :yellow :green :orange :purple nil])

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
  :polygons []
  ;; one of :polygons
  :selected-polygon nil
  ;; current style - graphics.Style
  :style  (sg/style :color :black :stroke nil)
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

(defn print-state
  "Pretty print parts or all of state.  Default prints lines out in
   compact string form."
;  ([] (pprint @state))
  ([] (do
        (pprint (for [k (keys @state) :when (not= k :lines)]
            {k (@state k)}))
        (println (format "lines:\n%s" (g/lines-as-str @state)))))
  ([k] (pprint (@state k))))


;(defn red-x-icon []
;  (let [icon (get-icon)]
;    (sc/config! icon :paint draw-a-red-x)
;    icon))


;; /Users/drogers/Desktop/images-craigslist/floorplan.jpg
;; /Users/drogers/Desktop/images-craigslist/cottage.jpg

;;**** TODO - REMOVE ALONG WITH :mode stuff ***
(defn switch-mode [state source]
  "Switch the application mode in state - ie Lines/Polygons
   state - current app state map
   source - widget"
  (let [selected? (sc/selection source)]
    (assoc state :mode (if selected? (sc/id-of source)))))

(defn dispatch  [handler]
  "Returns event handler based on param that manipulates app state.
   ** ACCESSES GLOBAL STATE: @state **
   Used for drawing lines: see *-new-line for input function examples.
   handler - function (f [state event]) -> new-state"
  (fn [event & args]
    (when (and handler (= (@state :click-mode) :draw))
;      (println "dispatch:")
;      (println "event: " event "\nargs: " args)
      (log/info "dispatch: handler: " handler "event: " event)
;                 "\nargs: " args "\nstate: " @state)
;      (prn "image-label: " (sc/select (sc/to-root event) [:#image-label]))
      (apply swap! state handler event args)
      (sc/repaint! (sc/select (sc/to-root event) [:#image-label])))))


(defn render [^javax.swing.JComponent c ^java.awt.Graphics2D g]
  "** ACCESSES GLOBAL STATE: @state **
   c - component
   g - graphics context
  "
  (let [{:keys [lines current-line]} @state]
    ;; draw line segments of full lines
    (apply sg/draw g (apply concat lines))
    ;; draw endpoints of full lines
;    (println "lines: " (count lines))
;    (pprint lines)
    (dorun
      (for [[l s] lines]
        (do
;          (println "draw endpoints - " l)
          (g/draw-line-end-points l s g))))
    ;; draw line segment of working line if dragging
    (when current-line
      (apply sg/draw g current-line))))

;(def del-last-line-action
;  (sc/action :name "delete-last-line"
;                   :handler (fn [actevent]
;                              (swap! state delete-last-line)
;                              (sc/repaint! actevent))
;                   :key "menu Z"))

(defn add-behaviors [root]
  (let [imgicon (sc/select root [:#image-label])
        modes (sc/button-group :buttons (sc/select root [:.mode]))
        styles (sc/select root [:.style])
        delete-last-button (sc/select root [:#delete-last-button])]
    (sc/listen modes :selection #(swap! state switch-mode %))
    (sc/listen delete-last-button
               :action (fn [actevent]
                          (swap! state g/delete-last-line)
                          (sc/repaint! imgicon)))
    (sc/listen styles :selection #(swap! state g/update-line-style %))
    ;; click and drag applies in :draw :click-mode
    (behave/when-mouse-dragged imgicon
      :start (dispatch g/start-new-line)
      :drag  (dispatch g/drag-new-line)
      :finish (dispatch g/finish-new-line))
    ;; simple click applies to selection of polys in :calculate :click-mode
    (sc/listen imgicon :mouse-clicked
               (fn [e]
                 (when (= :calculate (@state :click-mode))
                   (println "click: (" (.getX e) ", " (.getY e) ")"))))
    ;; add mouse coords to label in bottom panel
    (sc/listen imgicon :mouse-moved
               (fn [e]
                 (let [display (sc/select root [:#mouse-coords])]
                   (sc/config! display :text
                               (str "(" (.getX e) ", " (.getY e) ")")))))
    (doseq [s styles] #(swap! state g/update-line-style s)))
  root)

(defn init-selections! [root]
  "Set initial selections as needed - triggers state update.
   root - root component
   returns root"
  (let [width (sc/select root [:#stroke])
        color (sc/select root [:#foreground])
        mode (sc/select root [:#polygons])]
    (sc/selection! width 3)
    (sc/selection! color :red)
    (sc/selection! mode true))
  root)

;(def floorplan-img "/Users/drogers/Desktop/images-craigslist/floorplan.jpg")
(def floorplan-img "/Users/drogers/Desktop/images-craigslist/floorplan-w-1000.jpg")

(defn get-image-label []
  (let [icon (ImageIcon. (io/as-url (str "file://" floorplan-img)) "describe me")
        label (sc/label :id :image-label :paint render)]
    (.setIcon label icon)
    label))

;; cell renderer - for comboboxes that list colors paints the
;; background that color - taken from Dave Ray's scribble example
;; as he notes below - OS X does not show the color for the current
;; value
; TODO On OSX (at least) the renderer is not used for the currently
; displayed value, only when the combobox list is displayed
(defn color-cell [this {:keys [value selected?]}]
  (if value
    (sc/config! this :background value
                  :foreground (if (= :white value) :black :white))
    (sc/config! this :text "None")))

(defn make-gui []
  "Constructs and returns the jframe gui.
   ** MANIPULATES GLOBAL STATE: @state **"
  ;; Swing native look and feel
  (sc/native!)
  (let [click-mode-group (sc/button-group)
        gui (sc/frame
            :title "Calculate Distances or Area"
            :on-close :exit
            :width 800 :height 400
            :content
            (sc/border-panel :hgap 5 :vgap 5 :border 5
                         :north (sc/toolbar
                           :floatable? false
                           :items [(sc/toggle :id :lines     :class :mode :text "Lines")
                                   (sc/toggle :id :polygons  :class :mode :text "Polygons")
                                   :separator
                                   (sc/radio :text "Draw" :selected? true :group click-mode-group)
                                   (sc/radio :text "Calculate" :group click-mode-group)
                                   :separator
                                   "Width"
                                   (sc/combobox :id :stroke :class :style :model [1 2 3 5 8 13 21])
                                   "Color"
                                   (sc/combobox :id :foreground :class :style :model colors :renderer color-cell)
                                   ])
                          ; Create the drawing surface over an image held by an image label
                          :center (get-image-label)

                          :south (sc/horizontal-panel :id :south-panel
                                      :items ["Here's a label "
                                                "And another"
                                                (sc/button :text "Delete last"
                                                           :id :delete-last-button)
                                                (sc/label :id :mouse-coords)])))]

    (sc/listen click-mode-group :action
        (fn [e]
          (if (= "Draw" (sc/text (sc/selection click-mode-group)))
            (swap! state assoc :click-mode :draw)
            (swap! state assoc :click-mode :calculate))))
    gui))

(defn reset-state! [root]
  "Reset application state
   ** MANIPULATES GLOBAL STATE: @state **
   root - root component of gui as far as seesaw is concerned"
  (reset! state (clean-state))
  (init-selections! root)
  (sc/repaint! root))

(defn run-gui []
  "** ACCESSES GLOBAL STATE: @state **"
  (let [gui (make-gui)]
    (reset! state (clean-state))
    (sc/config! gui :on-close :dispose)
    (sc/invoke-later
      (-> gui add-behaviors init-selections! sc/pack! sc/show!))
    gui))

(defn -main []
  (sc/invoke-later
    (-> (make-gui) add-behaviors init-selections! sc/pack! sc/show!)))
