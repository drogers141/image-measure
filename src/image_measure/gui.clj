(ns image-measure.gui
  (:require [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [clojure.tools.logging :as log]
            [seesaw.core :as sc]
            [seesaw.color :as sclr]
            [seesaw.graphics :as sg]
            [seesaw.behave :as behave]
            [image-measure.graphics :as g]
            [image-measure.state :as state])
  (:import [javax.swing JFrame JPanel ImageIcon]
           [java.awt.geom Line2D]))

(def colors [:black :white :red :blue :yellow :green :orange :purple nil])



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
   ** ACCESSES GLOBAL STATE: @state/state **
   Used for drawing lines: see *-new-line for input function examples.
   handler - function (f [state event]) -> new-state"
  (fn [event & args]
    (when (and handler (= (@state/state :click-mode) :draw))
;      (println "dispatch:")
;      (println "event: " event "\nargs: " args)
;      (log/info "dispatch: handler: " handler "event: " event)
      (apply swap! state/state handler event args)
      (sc/repaint! (sc/select (sc/to-root event) [:#image-label])))))


(defn render [^javax.swing.JComponent c ^java.awt.Graphics2D g]
  "** ACCESSES GLOBAL STATE: @state/state **
   c - component
   g - graphics context
  "
  (let [{:keys [lines current-line]} @state/state
        label (g/->Label 445 195 18 "3.14159" (sclr/color :black) (sclr/color :white))]
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
      (apply sg/draw g current-line))

;    (g/paint-label g 680 150 "HelloWorld.0" (sclr/color :black) (sclr/color :white))
;    (println (g/label-geometry g 445 195 18 "3.14159"))
    (println (g/label-geometry g label))
    (g/paint-label g label)
    (g/paint-label g (g/centered-label g label 445.1 195.1))
;    (g/paint-label g 445 195 "3.14159" (sclr/color :black) (sclr/color :white))
    ))

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
    (sc/listen modes :selection #(swap! state/state switch-mode %))
    (sc/listen delete-last-button
               :action (fn [actevent]
                          (swap! state/state g/delete-last-line)
                          (sc/repaint! imgicon)))
    (sc/listen styles :selection #(swap! state/state g/update-line-style %))
    ;; click and drag applies in :draw :click-mode
    (behave/when-mouse-dragged imgicon
      :start (dispatch g/start-new-line)
      :drag  (dispatch g/drag-new-line)
      :finish (dispatch g/finish-new-line))
    ;; simple click selects polygon in :calculate :click-mode
    (sc/listen imgicon :mouse-clicked
               (fn [e]
                 (when (= :calculate (@state/state :click-mode))
                   (do
                     (let [selected-poly (g/find-polygon @state/state (.getX e) (.getY e))]
                     (println "click: (" (.getX e) ", " (.getY e) ")")
                     (println "polygon: " selected-poly)
                     (swap! state/state assoc :selected-polygon selected-poly))))))
    ;; add mouse coords to label in bottom panel
    (sc/listen imgicon :mouse-moved
               (fn [e]
                 (let [display (sc/select root [:#mouse-coords])]
                   (sc/config! display :text
                               (str "(" (.getX e) ", " (.getY e) ")")))))
    (doseq [s styles] #(swap! state/state g/update-line-style s)))
  root)

(defn init-selections! [root]
  "Set initial selections as needed - triggers state update.
   root - root component
   returns root"
  (let [width (sc/select root [:#stroke])
        color (sc/select root [:#foreground])
        mode (sc/select root [:#polygons])]
    (sc/selection! width 5)
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

(defn set-poly-for-calculation [state index]
  "Setup polygon for calculation:
     * enable line and area selection gui elements
     * enable calculate button
     * label each line with index
     * populate dropdown gui with indices so user can enter length of a
         particular line
     "
  (let [lines ((state :polygons) index)]
    nil))


(defn make-gui []
  "Constructs and returns the jframe gui.
   ** MANIPULATES GLOBAL STATE: @state/state **"
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
                                   "Area"
                                   (sc/text :id :area-input)
                                   "Line"
                                   (sc/combobox :id :line-select :model [])
                                   (sc/text :id :line-length-input)
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
            (swap! state/state assoc :click-mode :draw)
            (swap! state/state assoc :click-mode :calculate))))
    gui))

(defn print-state
  "Pretty print parts or all of state.  Default prints lines out in
   compact string form."
;  ([] (pprint @state/state))
  ([] (do
        (pprint (for [k (keys @state/state) :when (not= k :lines)]
            {k (@state/state k)}))
        (println (format "lines:\n%s" (g/lines-as-str @state/state)))))
  ([k] (pprint (@state/state k))))

(defn reset-state! [root]
  "Reset application state
   ** MANIPULATES GLOBAL STATE: @state/state **
   root - root component of gui as far as seesaw is concerned"
  (reset! state/state (state/clean-state))
  (init-selections! root)
  (sc/repaint! root))

(defn run-gui []
  "** ACCESSES GLOBAL STATE: @state/state **"
  (let [gui (make-gui)]
    (reset! state/state (state/clean-state))
    (sc/config! gui :on-close :dispose)
    (sc/invoke-later
      (-> gui add-behaviors init-selections! sc/pack! sc/show!))
    gui))

(defn -main []
  (sc/invoke-later
    (-> (make-gui) add-behaviors init-selections! sc/pack! sc/show!)))
