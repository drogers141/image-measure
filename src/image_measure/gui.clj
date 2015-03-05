(ns image-measure.gui
  (:require [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [clojure.tools.logging :as log]
            [seesaw.core :as sc]
            [seesaw.color :as sclr]
            [seesaw.graphics :as sg]
            [seesaw.behave :as behave])
  (:import [javax.swing JFrame JPanel ImageIcon]))

(def colors [:black :white :red :blue :yellow :green :orange :purple nil])

(def state (atom {
  ;; can have lines or polygons that are sequences of lines 
  ;; so any line in a poly is also in lines
  :lines []
  :polygons []
  :style  (sg/style :color :black :stroke nil) }))

(defn print-state
  ([] (pprint @state))
  ([k] (pprint (@state k))))

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

;(defn red-x-icon []
;  (let [icon (get-icon)]
;    (sc/config! icon :paint draw-a-red-x)
;    icon))


;; /Users/drogers/Desktop/images-craigslist/floorplan.jpg
;; /Users/drogers/Desktop/images-craigslist/cottage.jpg

(defn start-new-line [state e]
  (let [p (.getPoint e)]
    (log/info "state: " state)
    (assoc state
           :start-point [(.x p) (.y p)]
           :current-line [(sg/line (.x p) (.y p) (.x p) (.y p)) (:style state)])))

(defn drag-new-line [state e [dx dy]]
  (let [p (.getPoint e)
        [start-x start-y] (:start-point state)]
    (assoc state :current-line 
           [(sg/line start-x start-y (.x p) (.y p)) (:style state)])))

(defn finish-new-line [state e]
  (do
    (log/info "state: " state)
    (-> state
      (update-in [:lines] conj (:current-line state))
      (assoc :current-line nil))))

(defn update-line-style
  [state source]
  (let [style-key (sc/id-of source) new-value (sc/selection source)]
    (update-in state [:style] sg/update-style style-key new-value)))

(defn dispatch  [handler]
  (fn [event & args]
    (when handler
;      (println "dispatch:")
;      (println "event: " event "\nargs: " args)
      (log/debug "dispatch: handler: " handler "event: " event 
                 "\nargs: " args "\nstate: " @state)
;      (prn "image-label: " (sc/select (sc/to-root event) [:#image-label]))
      (apply swap! state handler event args)
      (sc/repaint! (sc/select (sc/to-root event) [:#image-label])))))


(defn render [c g]
  "c - component
   g - graphics context
  "
  (let [{:keys [lines current-line]} @state]
    (apply sg/draw g (apply concat lines))
    (when current-line
      (apply sg/draw g current-line))))
    
(defn add-behaviors [root]
  (let [imgicon (sc/select root [:#image-label])
        styles (sc/select root [:.style])]
    (sc/listen styles :selection #(swap! state update-line-style %))
    (behave/when-mouse-dragged imgicon
      :start (dispatch start-new-line)
      :drag  (dispatch drag-new-line)
      :finish (dispatch finish-new-line))
    (doseq [s styles] #(swap! state update-line-style s)))
  root)

(defn init-selections! [root]
  "Set initial selections as needed - triggers state update.
   root - root component
   returns root"
  (let [width (sc/select root [:#stroke])
        color (sc/select root [:#foreground])]
    (sc/selection! width 3)
    (sc/selection! color :red))
  root)

(def floorplan-img "/Users/drogers/Desktop/images-craigslist/floorplan.jpg")

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
  ;; Swing native look and feel
  (sc/native!)
  (sc/frame 
    :title "Calculate Distances or Area" 
    :on-close :dispose
    :width 800 :height 400
    :content 
    (sc/border-panel :hgap 5 :vgap 5 :border 5
                 :north (sc/toolbar
                   :floatable? false
                   :items [(sc/toggle :id :lines     :class :tool :text "Line")
                           (sc/toggle :id :polygons  :class :tool :text "Polygons")
                           :separator
                           "Width"
                           (sc/combobox :id :stroke :class :style :model [1 2 3 5 8 13 21])
                           "Color"
                           (sc/combobox :id :foreground :class :style :model colors :renderer color-cell)
;                           (log/info "color selection: " (sc/selection (sc/select 
;                           (sc/selection! (sc/combobox :id :background :class :style :model colors :renderer color-cell) 
;                                       nil)
                           ])
                  ; Create the drawing surface over an image held by an image label
                  :center (get-image-label)
                  
                  ; Some buttons to swap the paint function
                  :south (sc/horizontal-panel :items ["Here's a label "
                                                        "And another"]))))

(defn run-gui []
  (let [gui (make-gui)]
    (sc/invoke-later
      (-> gui add-behaviors init-selections! sc/pack! sc/show!))
    gui))

(defn -main []
  (sc/invoke-later
    (-> (make-gui) add-behaviors sc/pack! sc/show!)))
