(defn roll
  [n]
  (inc (math/floor (* n (math/random)))))

(defn get-table
  [t res]
  (var outcome nil)
  (loop [v :in (reverse (partition 2 t))]
    (when (>= res (v 0))
      (set outcome (v 1))
      (break)))
  outcome)

(defn roll-table
  [t die]
  (def res (roll die))
  (get-table t res))