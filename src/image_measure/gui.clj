(ns image-measure.gui
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]]
            [clojure.tools.logging :as log]
            [seesaw.core :as sc]
            [seesaw.color :as sclr]
            [seesaw.graphics :as sg]
            [seesaw.behave :as behave]
            [seesaw.bind :as bind]
            [seesaw.chooser :as chooser]
            [image-measure.graphics :as g]
            [image-measure.state :as state]
            [image-measure.geo :as geo])
  (:import [javax.swing JFrame JPanel ImageIcon]
           [java.awt.geom Line2D]
           [java.awt.image BufferedImage]
           [javax.imageio ImageIO]))

(def colors [:black :white :red :blue :yellow :green :orange :purple nil])

(defn dispatch  [handler]
  "Returns event handler based on param that manipulates app state.
   ** ACCESSES GLOBAL STATE: @state/state **
   Used for drawing lines: see *-new-line for input function examples.
   handler - function (f [state event]) -> new-state"
  (fn [event & args]
    (when (and handler (= (@state/state :click-mode) :draw))
;      (log/debug "dispatch: handler: " handler "event: " event)
      (apply swap! state/state handler event args)
      (sc/repaint! (sc/select (sc/to-root event) [:#image-label])))))


(defn render [^javax.swing.JComponent c ^java.awt.Graphics2D g]
  "** ACCESSES GLOBAL STATE: @state/state **
   c - component
   g - graphics context
  "
  (let [{:keys [lines current-line labels]} @state/state]
    ;; draw line segments of full lines
    (apply sg/draw g (apply concat lines))
    ;; draw endpoints of full lines
    (dorun
      (for [[l s] lines]
        (do
          (g/draw-line-end-points l s g))))
    ;; draw all labels over finished lines
    (dorun
      (for [{l :label} labels]
        (do
          (g/paint-label g l))))
    ;; draw line segment of working line if dragging
    (when current-line
      (apply sg/draw g current-line))))

(defn set-poly-for-calculation! [root index]
  "Setup polygon for calculation:
     * enable line and area selection gui elements
     * enable calculate button
     * label each line with index
     * populate dropdown gui with indices so user can enter length of a
         particular line
   root - gui root
   index - poly index
   ** Mutates global state: state/state **"
  (let [g (.getGraphics root)
        lines ((@state/state :polygons) index)
        area-txt (sc/select root [:#area-input])
        line-select (sc/select root [:#line-select])
        line-txt (sc/select root [:#line-length-input])
        calculate-btn (sc/select root [:#calculate-btn])]
    (sc/config! area-txt :enabled? true)
    (sc/config! line-select :enabled? true)
    (sc/config! line-txt :enabled? true)
    (sc/config! calculate-btn :enabled? true)
    (sc/config! line-select :model lines)
    (swap! state/state g/label-poly-for-calculation g index)
    (sc/repaint! root)))

(defn set-free-lines-for-calculation [root index]
  "Setup free-lines for calculation
     * disable area and line selection
     * enable line length textbox
     * label selected free line with Selected
   index - index of selected free line in state"
  (let [g (.getGraphics root)
        area-txt (sc/select root [:#area-input])
        line-select (sc/select root [:#line-select])
        line-txt (sc/select root [:#line-length-input])
        calculate-btn (sc/select root [:#calculate-btn])]
    (sc/config! area-txt :enabled? false)
    (sc/config! line-select :enabled? false)
    (sc/config! line-txt :enabled? true)
    (sc/config! calculate-btn :enabled? true)
    (swap! state/state g/label-free-lines-for-calculation g index)
    (sc/repaint! root)))

(defn calculate-line-lengths [indices line length]
  "indices - line indices
   line - index of known line
   length - length of known line
   Returns map with indices as keys and lengths as values (doubles).
   ** Access global state: state/state **"
  (let [index-to-xy #(g/line2d-to-xy (g/line-from-index @state/state %))
        known (index-to-xy line)
        xy-lines (vec (map index-to-xy indices))
        lengths (geo/calibrate-scale known length xy-lines)]
    (zipmap indices lengths)))

(defn calculate-scaled-area [indices line length]
  "indices - line indices
   line - index of known line
   length - length of known line
   Returns area of polygon scaled to correspond to line's length.
   ** Access global state: state/state **"
  (let [points (g/ordered-poly-points @state/state indices)
        area (geo/area points)
        line-pts (g/get-points (g/line-from-index @state/state line))
        pixel-len (.distance (line-pts 0) (line-pts 1))
        scale (/ length pixel-len)]
    (* area (* scale scale))))

(defn calculate-free-line-lengths [line-index length]
  "Calculates length of all free-lines in state/state given the length
   of one line.
   Returns map {0 1 ..} with the length of each line keyed to its index.
   ** Access global state: state/state **"
  (let [indices (g/free-lines @state/state)]
    (calculate-line-lengths indices line-index length)))

(defn calculate-from-length [poly-index line-index length]
  "Calculates area of polygon and length of all lines given the length
   of one line.
   poly-index - polygon index to work with
   line-index - line with known length
   length - length of line with line-index
   Returns map {:area 0 1 ..} with the area of the polygon and length of
   each line keyed to its index.
   ** Access global state: state/state **"
  (let [indices (get (@state/state :polygons) poly-index)
        lengths (calculate-line-lengths indices line-index length)
        area (calculate-scaled-area indices line-index length)]
    (assoc lengths :area area)))

(defn calculate-from-area [poly-index area]
  "Calculates length of all lines of polygon given the area.
   poly-index - index of polygon to work with
   area - given scaled area of polygon
   Returns map {:area 0 1 ..} with the area of the polygon and length of
   each line keyed to its index.
   ** Access global state: state/state **"
  ;; iterative method used to approximate within a tolerance
  (let [tolerance 0.01
        indices (get (@state/state :polygons) poly-index)
        line-index (first indices)]
    (loop [len-i (Math/sqrt area)
           len-lo nil
           len-hi nil]
      (let [area-i (calculate-scaled-area indices line-index len-i)]
        (cond
          ;; if within tolerance return results using line and length
          (> tolerance (Math/abs (- area-i area)))
          (calculate-from-length poly-index line-index len-i)
          ;; otherwise bisect current approximation of length with
          ;; prior high and low values or double or half current val if n/a
          (< area-i area)
          (let [len-lo len-i]
;            (log/info "lower - len-i: " len-i ", area-i: " area-i)
            (if (nil? len-hi)
              (recur (* 2 len-i) len-lo nil)
              (recur (/ (+ len-lo len-hi) 2) len-lo len-hi)))
          :else
          (let [len-hi len-i]
;            (log/info "higher - len-i: " len-i ", area-i: " area-i)
            (if (nil? len-lo)
              (recur (/ len-i 2.0) nil len-hi)
              (recur (/ (+ len-lo len-hi) 2) len-lo len-hi))))))))

(defn numeric-input? [root val]
  "Returns val as double if it can be parsed as double.
   If non-numeric alerts user with dialog before returning false.
   root - root component of gui to be dialog parent"
  (try
    (Double/parseDouble val)
    (catch NumberFormatException e
      (sc/alert root (format "\"%s\": input needs to be numeric." val))
      false)
    (finally false)))

(defn print-state
  "Pretty print parts or all of state.  Default prints lines out in
   compact string form."
  ([] (do
        (pprint (for [k (keys @state/state) :when (not= k :lines)]
            {k (@state/state k)}))
        (println (format "lines:\n%s" (g/lines-as-str @state/state)))))
  ([k] (pprint (@state/state k))))

(defn init-selections! [root]
  "Set initial selections as needed - triggers state update.
   root - root component
   returns root"
  (let [polygons-radio (sc/select root [:#polygons-radio])
        draw-radio (sc/select root [:#draw-radio])
        width (sc/select root [:#stroke])
        area-txt (sc/select root [:#area-input])
        line-select (sc/select root [:#line-select])
        line-txt (sc/select root [:#line-length-input])
        ;; leaving for now - see commented out section in create-gui
;        color (sc/select root [:#foreground])
        ]
    (sc/config! polygons-radio :selected? true)
    (sc/config! draw-radio :selected? true)
    (sc/selection! width 5)
    (sc/config! area-txt :text "" :enabled? false)
    (sc/config! line-txt :text "" :enabled? false)
    (sc/config! line-select :enabled? false)
;    (sc/selection! color :red)
    ;; hard set foreground drawing color to red
    ;; replace if we go back to giving color choice
    (swap! state/state assoc-in [:style :foreground] (sclr/color :red))
  root))

(defn reset-state! [root]
  "Reset application state
   ** MANIPULATES GLOBAL STATE: @state/state **
   root - root component of gui as far as seesaw is concerned"
  (reset! state/state (state/clean-state))
  (init-selections! root)
  (sc/repaint! root))

(defn get-image-label [image-url]
  "Returns image component of gui.
   image-url - java.net.URL of image to work with"
  (let [icon (ImageIcon. image-url "Main image")
        label (sc/label :id :image-label :paint render)]
    (.setIcon label icon)
    label))

(defn save-image [root outfile]
  "Save image to File outfile.  Can only handle png so path must be *.png.
   Does nothing and returns nil if not.  Returns true if successful.
   root - root component of gui
   outfile - File instance"
  (when (= "png" (last (str/split (.getName outfile) #"\.")))
    (let [label (sc/select root [:#image-label])
          image (BufferedImage. (.getWidth label) (.getHeight label)
                                BufferedImage/TYPE_INT_ARGB)
          g (.getGraphics image)]
      (.paint label g)
      (ImageIO/write image "png" outfile))))

(defn handle-calculate-btn [root actevent]
  "Calculate button handler
   ** MANIPULATES GLOBAL STATE: @state/state **"
 (let [area-txt (sc/select root [:#area-input])
       line-select (sc/select root [:#line-select])
       line-txt (sc/select root [:#line-length-input])
       area (sc/config area-txt :text)
       line (sc/selection line-select)
       line-len (sc/config line-txt :text)
       poly-index (@state/state :selected-polygon)
       imgicon (sc/select root [:#image-label])
       g (.getGraphics root)]
   (if (= :polygons (@state/state :mode))
     (cond
       (and (pos? (count area)) (pos? (count line-len)))
       (sc/alert root "Can only calculate based on area or the length of a line. Not both.")
       (pos? (count line-len))
       (when (numeric-input? root line-len)
         (let [results (calculate-from-length poly-index line
                                              (Double/parseDouble line-len))]
           (swap! state/state g/label-poly-with-results g poly-index results)
           (swap! state/state assoc :latest-calculated-lines
                  (dissoc results :area))
           (sc/config! line-txt :text "")
           (sc/repaint! imgicon)))
       (pos? (count area))
       (when (numeric-input? root area)
         (let [results (calculate-from-area poly-index
                                            (Double/parseDouble area))]
           (swap! state/state g/label-poly-with-results g poly-index results)
           (swap! state/state assoc :latest-calculated-lines
                  (dissoc results :area))
           (sc/config! area-txt :text "")
           (sc/repaint! imgicon))))
     ;; lines mode
     (let [handle-lines (fn [line-index length]
                          (let [results (calculate-free-line-lengths
                                          line-index length)]
                             (swap! state/state
                                    g/label-free-lines-with-results g results)
                             (sc/config! line-txt :text "")
                             (sc/repaint! imgicon)))]
       (cond
         ;; no length given, but have polygons calculated
         ;; use max length from calculated lines in state
         (and (zero? (count line-len))
              (pos? (count (@state/state :latest-calculated-lines))))
         (let [calculated (@state/state :latest-calculated-lines)
               maxlen (apply max (vals calculated))
               line (first (for [[k v] calculated :when (= v maxlen)] k))]
           (handle-lines line maxlen))
         ;; line length given for selected free line
         (numeric-input? root line-len)
         (handle-lines (@state/state :selected-free-line)
                       (Double/parseDouble line-len))
         :else
         (sc/alert root (str "No scaled lengths calculated from polygons"
                             "or selected line length given.")))))))

;; simple click selects polygon in :polygons mode, :calculate click-mode
;; selects a free line in :lines mode, :calculate click-mode
;; note - have the assignment of selected-polygon or selected-free-line
;; within the when clauses below - this way clicking something else or
;; nothing does not deselect the polygon or free-line -
;; user has to select another free-line or polygon to change selection
;; may want to change, but seems safer this way given current setup
(defn handle-mouse-clicked [root e]
  "Mouse click handler - see add-behaviors for click/drag
   ** ACCESSES GLOBAL STATE: @state/state **"
 (when (= :calculate (@state/state :click-mode))
   (if (= :polygons (@state/state :mode))
     (let [selected-poly (g/find-polygon @state/state (.getX e) (.getY e))]
       (log/debug "click: (" (.getX e) ", " (.getY e) ")")
       (log/debug "selecting polygon: " selected-poly)
       (when selected-poly
         (swap! state/state assoc :selected-polygon selected-poly)
         (set-poly-for-calculation! root selected-poly)))
     ;; is :lines mode
     (let [selected-line (g/find-free-line @state/state (.getX e) (.getY e))]
       (log/debug "click: (" (.getX e) ", " (.getY e) ")")
       (log/debug "selecting free line: " selected-line)
       (when selected-line
         (swap! state/state assoc :selected-free-line selected-line)
         (set-free-lines-for-calculation root selected-line))))))

(defn add-behaviors [root]
  "Add functionality to widgets using Seesaw's selection.
   root - root widget of gui
   Returns root."
  (let [imgicon (sc/select root [:#image-label])
        styles (sc/select root [:.style])
        save-image-btn (sc/select root [:#save-image-btn])
        clear-all-btn (sc/select root [:#clear-all-btn])
        clear-labels-btn (sc/select root [:#clear-labels-btn])
        delete-last-btn (sc/select root [:#delete-last-btn])
        calculate-btn (sc/select root [:#calculate-btn])
        ]
    (sc/listen save-image-btn
               :action (fn [actevent]
                         (let [outfile (chooser/choose-file root :type :save
                                          :filters [["PNG files" ["png"]]])]
                           (if (= "png" (last (str/split (.getName outfile) #"\.")))
                             (save-image root outfile)
                             (sc/alert root "Must be *.png file to save image.")))))
    (sc/listen clear-all-btn
               :action (fn [actevent]
                         (reset-state! root)))
    (sc/listen clear-labels-btn
               :action (fn [actevent]
                         (swap! state/state assoc :labels [])
                         (sc/repaint! imgicon)))
    (sc/listen delete-last-btn
               :action (fn [actevent]
                          (swap! state/state g/delete-last-line)
                          (sc/repaint! imgicon)))
    (sc/listen calculate-btn
               :action (partial handle-calculate-btn root))
    (sc/listen styles :selection #(swap! state/state g/update-line-style %))
    ;; click and drag applies in :draw :click-mode
    (behave/when-mouse-dragged imgicon
      :start (dispatch g/start-new-line)
      :drag  (dispatch g/drag-new-line)
      :finish (dispatch g/finish-new-line))

    (sc/listen imgicon :mouse-clicked (partial handle-mouse-clicked root))

    ;; add mouse coords to label in bottom panel
    (sc/listen imgicon :mouse-moved
               (fn [e]
                 (let [display (sc/select root [:#mouse-coords])]
                   (sc/config! display :text
                               (str "(" (.getX e) ", " (.getY e) ")")))))
    (doseq [s styles] #(swap! state/state g/update-line-style s)))
  root)

;; (currently unused but keeping around for now)
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

(defn make-gui [image-url]
  "Constructs and returns the jframe gui.
   image-url - java.net.URL of image to work with
   ** MANIPULATES GLOBAL STATE: @state/state **"
  ;; Swing native look and feel
  (sc/native!)
  (let [mode-group (sc/button-group)
        click-mode-group (sc/button-group)
        gui (sc/frame
            :title "Calculate Distances or Area"
            :on-close :exit
            :width 800 :height 400
            :content
            (sc/border-panel :hgap 5 :vgap 5 :border 5
                         :north (sc/toolbar
                           :floatable? false
                           :items [(sc/radio :text "Lines" :group mode-group)
                                   (sc/radio :id :polygons-radio :text "Polygons" :selected? true :group mode-group)
                                   :separator
                                   (sc/radio :id :draw-radio :text "Draw" :selected? true :group click-mode-group)
                                   (sc/radio :text "Calculate" :group click-mode-group)
                                   :separator
                                   "Area"
                                   (sc/text :id :area-input :columns 12 :enabled? false)
                                   "Line"
                                   (sc/combobox :id :line-select :model [] :enabled? false)
                                   "Line Length"
                                   (sc/text :id :line-length-input :columns 12 :enabled? false)
                                   (sc/button :id :calculate-btn :text "Calculate" :enabled? false)
                                   :separator
                                   "Line Width"
                                   (sc/combobox :id :stroke :class :style :model [1 2 3 5 8 13 21])
                                   ;; leaving this here if we want to allow different line colors
                                   ;; again - would probably have to add current-poly and finished
                                   ;; poly colors - this is just for current-poly drawing
;                                   "Color"
;                                   (sc/combobox :id :foreground :class :style :model colors :renderer color-cell)
                                   ])
                          ; Create the drawing surface over an image held by an image label
                          :center (get-image-label image-url)

                          :south (sc/horizontal-panel :id :south-panel
                                      :items [(sc/button :id :save-image-btn
                                                         :text "Save Image")
                                              (sc/button :id :clear-all-btn
                                                         :text "Clear All")
                                              (sc/button :id :clear-labels-btn
                                                         :text "Clear Labels")
                                              (sc/button :text "Delete Last Line"
                                                         :id :delete-last-btn)
                                              (sc/label :id :mouse-coords)])))]

    (sc/listen click-mode-group :action
        (fn [e]
          (if (= "Draw" (sc/text (sc/selection click-mode-group)))
            (swap! state/state assoc :click-mode :draw)
            (swap! state/state assoc :click-mode :calculate))))
    (sc/listen mode-group :action
        (fn [e]
          (if (= "Polygons" (sc/text (sc/selection mode-group)))
            (swap! state/state assoc :mode :polygons)
            (swap! state/state assoc :mode :lines))))
    gui))

(defn run-gui [image-url]
  "Entry point that returns gui reference.
   image-url - java.net.URL of image to work with
   ** ACCESSES GLOBAL STATE: @state/state **"
  (let [gui (make-gui image-url)]
    (reset! state/state (state/clean-state))
    (sc/config! gui :on-close :dispose)
    (sc/invoke-later
      (-> gui add-behaviors init-selections! sc/pack! sc/show!))
    gui))

(defn -main [image-url]
  "image-url - java.net.URL of image to work with"
  (sc/invoke-later
    (-> (make-gui image-url) add-behaviors init-selections! sc/pack! sc/show!)))
