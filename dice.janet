(defn roll
  [n]
  (inc (math/floor (* n (math/random)))))

(defn roll-many
  [nof n]
  (seq [_ :range [0 nof]]
    (roll n)))

(defn roll-sum
  [nof n]
  (var r 0)
  (for _ 0 nof
    (+= r (roll n)))
  r)

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

(comment
  (roll 6)
  (roll-many 2 6)
  (roll-sum 2 6)
  )