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
 