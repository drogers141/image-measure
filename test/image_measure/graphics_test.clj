(ns image-measure.graphics-test
  (:require [clojure.test :refer :all]
            [image-measure.graphics :refer :all]))

(def state0
  {:lines [:line1 :line2 :line3]
   :polygons []})

(def state1
  {:lines [:line1 :line2 :line3]
   :polygons []
   :current-polygon [2]})

(def state2
  {:lines [:line1 :line2 :line3 :line4]
   :polygons []
   :current-polygon [2]})

(def state3
  {:lines [:line1 :line2 :line3 :line4]
   :polygons []
   :current-polygon [2 3]})

(def state4
  {:lines [:line1 :line2 :line3 :line4 :line5]
   :polygons []
   :current-polygon [2 3 4]})

(def state5
  {:lines [:line1 :line2 :line3 :line4 :line5]
   :polygons [[2 3 4]]
   :current-polygon nil})

(deftest test-start-new-polygon
  (is (= (start-new-polygon state0) state1)))

(deftest test-add-latest-line-to-polygon
  (is (= (add-latest-line-to-current-polygon state2) state3)))

(deftest test-finish-polygon
  (is (= (finish-polygon state4) state5)))

(deftest test-remove-first
  (let [next-line [[411.0 14.0] [411.0 185.0]]
         remaining '([[268.0 185.0] [268.0 14.0]]
                      [[411.0 14.0] [411.0 185.0]]
                      [[411.0 185.0] [268.0 185.0]])
         expected '([[268.0 185.0] [268.0 14.0]]
                     [[411.0 185.0] [268.0 185.0]])]
    (is (= (remove-first next-line remaining)
           expected))))
