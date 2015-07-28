(ns image-measure.label
  "Defines a custom painted label in Java2d and offers some utilities."
  (:require [clojure.tools.logging :as log]
           [seesaw.core :as sc]
           [seesaw.color :as sclr]
           [seesaw.graphics :as sg]
           [seesaw.font :as sfont]))

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
