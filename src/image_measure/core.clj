(ns image-measure.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.tools.cli :refer [parse-opts]]
            [image-measure.gui :as gui])
  (:gen-class))

(def cli-options
  [["-h" "--help" "Print this help"
        :flag true]])

(defn usage [options-summary]
  (->> ["Calculate distances/lengths and area in images using drawn lines and"
        "polygons."
        ""
        "Usage: image-measure [options] [image-file]"
        ""
        "Options:"
        options-summary
        ""
        "image-file"
        "Path to image file to work with."
        "Note gui cannot resize image.  Recommended to work with images at"
        "least 800 px wide for clarity of labeling, etc."
        "If image-file is not provided, program is run with image used in the"
        "Readme examples."]
       (str/join \newline)))

(defn error-msg [errors]
  (str "The following errors occurred while parsing your command:\n\n"
       (str/join \newline errors)))

(defn exit [status msg]
  (println msg)
  (System/exit status))

(defn -main [& args]
  (let [{:keys [options arguments errors summary]} (parse-opts args cli-options)]
    (cond
      (:help options) (exit 0 (usage summary))

      (not= 1 (count arguments))
      (gui/-main (io/resource "floorplan.jpg"))

      errors (exit 1 (error-msg errors))

      :else
      (let [f (io/file (arguments 0))]
        (if (.isFile f)
          (gui/-main (io/as-url (.toURI f)))
          (println "File not found: " (.getName f)))))))
