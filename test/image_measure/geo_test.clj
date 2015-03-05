(ns image-measure.geo-test
  (:require [clojure.test :refer :all]
            [image-measure.geo :refer :all]))

(deftest distance-test
  (are [input dist] (= dist (apply distance input))
       [3 0 0 4] 5.0
       [1 0 0 -1] (Math/sqrt 2.0)))

(deftest midpoint-test
  (are [input mid] (= mid (apply midpoint input))
       [4 0 0 4]  [2.0 2.0]
       [4 0 0 -4]  [2.0 -2.0]))

(deftest area-test
  (are [poly a] (= a (area poly))
       [[0 0] [2 0] [2 2] [0 2]]
       4.0
       ;;
       [[2 2] [4 10] [9 7] [11 2]]
       45.5
       ;;
       [[-2 0] [0 4] [2 0]]
       8.0
       ;;
       [] 0.0))

(deftest calibrate-scale-test
  (are [line length lines scaled-lengths]
       (= scaled-lengths (calibrate-scale line length lines))
       ;; scale to 2/5 = 0.4
       [0 3 4 0]
       2.0
       [[0 0 10 0] [0 0 -10 0] [0 0 1 0] [0 0 1 1]]
       [4.0 4.0 0.4 (* 0.4 (Math/sqrt 2.0))]
       ;;
       ))

(deftest centroid-test
  (are [polygon c] (= c (centroid polygon))
       [[3 0] [0 3] [-3 0] [0 -3]]
       [0.0 0.0]
       ;;
       [[-3 0] [0 3] [3 0] [0 -3]]
       [0.0 0.0]
       ;;
       ))