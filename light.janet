(import ./state :as s)
(import ./dice :fresh true)
(import ./tile)

(defn light
  ``
  Returns brightness for position x y.
  ``
  [o]
  (def [x y] (tile/->pos o))
  (var brightness 0)

  (loop [l :in s/lights
         :let [lx (in l :light/x)
               ly (in l :light/y)
               r (in l :radius)
               dist
               (+ (math/pow
                    (math/abs (- x lx)) 2)
                  (math/pow
                    (math/abs (- y ly)) 2))
               dist (- r (math/sqrt dist))]
         :when (pos? dist)]
    (+= brightness (/ dist r)))

  brightness)

(def light-table
  [0 -2
   1 -1
   4 0
   6 1
   10 2])

(defn modifier
  [o]
  (dice/get-table
    light-table
    (math/floor
      (* 10 (light o)))))

(defn light-sources
  [x y]
  (var brightness 0)

  (seq [l :in s/lights
        :let [lx (in l :light/x)
              ly (in l :light/y)
              r (in l :radius)
              dx (- x lx)
              dy (- y ly)
              dist (+ (math/pow
                        (math/abs dx) 2)
                      (math/pow
                        (math/abs dy) 2))
              dist (- r (math/sqrt dist))]
        :when (pos? dist)]
    [l (/ dist r) dx dy]))
 