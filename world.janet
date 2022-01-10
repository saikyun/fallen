(import ./state :as s)
(import ./objects :as o)
(import ./tile)


(def X o/inner-wall)
(def x o/inner-wall-down)

(def . o/ground)

(def p @[(table/clone o/ground) s/player])


(def z @[o/ground o/zombie])

(def world-map
  (let [l @[o/ground o/locked-door]
        L @[o/ground (o/light-source 4)]
        c (o/chest :lockpick)
        w (o/chest :artifact)
        r (o/chest :random)]

    (def measure
      @[1 2 3 4 5 6 7 8 9 0 1 2 3])

    (def old-world
      @[X x x x x x x x x x x X X X X X >
        X c . . . . . l . . . x x x X X >
        X . L p . X X X X X . . . . X X >
        X . . . x x x x x x . X X . X X >
        X X . . . . . z . L . X X . X X >
        X X X X X X X X X X X X X . X X >
        X X X X X X X X X X X X X . X X >
        X X X X X X X X X X X X X . X X >
        X X X X X X X X X x x x x . X X >
        X X X X x x x x x . . . . . X X >
        X X X X . . z . . . . . . r X X >
        X X X X . L . . . . . . X X X X >
        X X X X w . . . . . . . X X X X >
        X X X X X X X X X . z X X X X X >
        X X X X X X X X X . . X X X X X >
        X X X X X X X X X . . X X X X X >
        X X X X X X X X X X X X X X X X >
        #
        ])

    (def world
      @[X x x x x x x x x x x X X X X X >
        X . . p . . . . . . . . . . X X >
        X . . . . . . . . . . . . . X X >
        X . . . . . . . . . . . . . X X >
        X . z . . . . . . . . . . . X X >
        X . . . . . . . . . . . . . X X >
        X . . . . . . . . . . . . . X X >
        X . . . . . . . z . . . . . X X >
        X . . . . . . . . . . . . . X X >
        X . . . . . . . . . . . . . X X >
        X X X X x x x x x x x x x . X X >
        #
        ])
    world))


(defn new-world
  []
  (var acc 0)

  (var ww 0)
  
  {:world
   (seq [i :range [0 (length world-map)]
         :let [cell (in world-map i)
               _ (if (= > cell)
                   (set acc 0)
                   (do
                     (++ acc)
                     (set ww (max ww acc))))]
         :when (not= > cell)]
     (cond (indexed? cell)
       (map |(if (= s/player $)
               $
               #(table/clone $)
               (table/setproto @{} $)
               )
            cell)
       @[(table/clone cell)]))
   :ww ww})



(defn init-world
  []

  (def world (new-world))

  (set s/world-list (world :world))

  (set s/lights @[])

  (set s/ww (world :ww))

  (tile/loop-world
    s/world-list
    (put o :pos [x y])
    (when (o :light)
      (put o :light/x x)
      (put o :light/y y)
      (array/push s/lights o)))

  world)