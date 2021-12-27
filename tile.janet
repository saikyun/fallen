(import freja/vector-math :as v)
(import ./state :as s)

(defn mouse-tile
  []
  (let [[x y] (v/v- s/mouse-pos s/offset)]
    [(math/floor (/ x s/w))
     (math/floor (/ y s/h))]))

(defn ->i
  [o]
  (match o
    {:pos [x y]} (+ x (* y s/ww))

    [x y] (+ x (* y s/ww))
    
    (_ (number? o)) o

    (errorf "No screen pos for: %p" o)))

(defn ->tile
  [o]
  (def i (->i o))
  (in (dyn :world) i))

(defn ->pos
  [o]
  (def [x y]
    (case (type o)
      :table (in o :pos)

      :array o

      :tuple o

      :number [(mod o s/ww) (math/floor (/ o s/ww))]

      (errorf "No screen pos for: %p" o))))

(defmacro loop-world-tile
  [world & body]
  ~(loop [i :range [0 (length ,world)]
          :let [tile (in ,world i)
                x (mod i s/ww)
                y (math/floor (/ i s/ww))]]
     ,;body))

(defmacro loop-world
  [world & body]
  ~(loop [i :range [0 (length ,world)]
          :let [tile (in ,world i)
                x (mod i s/ww)
                y (math/floor (/ i s/ww))]
          o :in tile]
     ,;body))

(defn points-between-line
  [o1 o2]

  (def [emx emy] (->pos o1))
  (def [pmx pmy] (->pos o2))

  (var points @[])

  (let [emx (+ 0.5 emx)
        emy (+ 0.5 emy)
        pmx (+ 0.5 pmx)
        pmy (+ 0.5 pmy)

        start-x emx
        stop-x pmx
        start-y emy
        stop-y pmy

        dir (v/v-
              [stop-x stop-y]
              [start-x start-y])

        divver (if (> (math/abs (dir 0)) (math/abs (dir 1)))
                 (dir 0)
                 (dir 1))

        dir2 (v/v* dir (/ 1 (math/abs divver)))]

    (var x start-x)
    (var y start-y)

    #                # determine direction of loop
    (while (or ((if (< start-x stop-x) < >)
                 x
                 stop-x)
               ((if (< start-y stop-y) < >)
                 y
                 stop-y))

      # draw line points
      #     (comment
      (comment draw-circle (math/floor (* 10 (math/floor x)))
        (math/floor (* 10 (math/floor y)))
        5
        :yellow)
      #
      #)

      (array/push points [x y])

      (+= x (dir2 0))
      (+= y (dir2 1))))

  (array/push points [(+ 0.5 pmx) (+ 0.5 pmy)])

  points)