(import freja/vector-math :as v)
(import ./state :as s)
(import ./tile)

(defn ->screen-pos
  [o]
  (def [x y] (tile/->pos o))  
  (-> [(* x s/w) (* y s/h)]
      (v/v+ s/offset)
      (v/v+ [(* s/w 0.5)
             (* s/h 0.5)])))

(comment
  (->screen-pos @{:pos [0 1]})
  #=> [16 72]

  (->screen-pos [0 1])
  #=> [16 72]
  )

(defn on-ui?
  []
  (>= (s/mouse-pos 1) s/ui-top))

(defn mouse-dir
  []
  (let [dir (->> (->screen-pos s/player)
                 (v/v- s/mouse-pos)
                 v/normalize
                 # if an axis is big enough, it is set to +/-1
                 (map |(if (> (math/abs $) 0.3333)
                         (/ $ (math/abs $))
                         0)))
        mt (tile/mouse-tile)

        tile-dir (v/v- mt (s/player :pos))]

    # if the cursor is on an adjacent tile, or on the character
    (if (> 2 (v/mag tile-dir))
      (tuple ;(v/v+ tile-dir (s/player :pos)))
      (tuple ;(v/v+ dir (s/player :pos))))))