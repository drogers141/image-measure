(ns image-measure.util
  "Lowest level general utility functions.")

(defn remove-first [item coll]
  "Returns collection coll with first element equal to item removed."
  (let [[n m] (split-with (partial not= item) coll)]
    (concat n (rest m))))

(defn indices [pred coll]
  "Returns indices in collection coll of elements that satisfy predicate pred."
  (keep-indexed #(when (pred %2) %1) coll))
