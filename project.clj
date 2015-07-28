(defproject image-measure "0.1.0-SNAPSHOT"
  :description "Calculate distance or area on images using java2d drawing."
  :url "https://github.com/drogers141/image-measure"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [seesaw "1.4.5"]
                 [org.clojure/tools.logging "0.3.1"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [org.clojure/tools.cli "0.3.1"]]
  :main image-measure.core
  :aot [image-measure.core])
