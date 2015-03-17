(ns spinner.math)


(defn cartesian-product-indices
  "Calculates each succesive index, to reach nth item: target-index.
  Returns nil if index is out of bounds.
  Example:
    (cartesian-product-indices 75638, [(range 100) (range 100) (range 100)]) => [7 56 38]
  "
  [target-index vectors]
  (let [counts (map count vectors)]
    (when (< -1 target-index (reduce * 1 counts))
      (loop [indices   (transient [])
             vectors   vectors
             s-counts  (next counts)
             remainder target-index]
        (let [subcount (reduce * 1 s-counts)
              q        (quot remainder subcount)
              indices  (conj! indices q)]
          (if-let [nvecs (next vectors)]
            (recur indices nvecs (next s-counts) (- remainder (* q subcount)))
          ;else
            (persistent! indices)))))))
