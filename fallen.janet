(use freja/flow)

(var spacing 0)
(var w 32)
(var h 48)

# world width
(def ww 10)

(defn rec
  [{:color color} x y]
  (draw-rectangle
    (* x w)
    (* y h)
    (- w spacing)
    (- h spacing)
    color))

(def inner-wall
  @{:blocking true
    :color 0x111111ff
    :render rec})

(def X inner-wall)

(def ground
  @{:blocking false
    :color 0x666666ff
    :render rec})

(def . ground)


(defn blocking?
  [i]
  (find |($ :blocking) (in (dyn :world) i)))

(defn interactive?
  [i]
  (find |($ :interact) (in (dyn :world) i)))


(defn circle
  [{:color color
    :hp hp} x y]
  (when (pos? hp)
    (let [r (* 0.5 (min (- w spacing) (- h spacing)))]
      (draw-circle (math/floor (+ (* x w)
                                  (* 0.5 (- w spacing))))
                   (math/floor (+ (* y h)
                                  (* 0.5 (- h spacing))))
                   r
                   color))))

(def player
  @{:name "Saikyun"
    :hp 30
    :max-hp 42
    :damage 3
    :blocking true
    :color 0x003333ff
    :render circle})

(def p @[ground player])

(defn pos
  [o]
  (let [i (find-index |(index-of o $) (dyn :world))]
    [(mod i ww)
     (math/floor (/ i ww))]))

(defn fight
  [defender attacker]
  (print (attacker :name) " dealt " (attacker :damage) " damace to " (defender :name) ".")
  (update defender :hp - (attacker :damage))

  (unless (pos? (defender :hp))
    (print (defender :name) " died in a fight against " (attacker :name) ".")
    (put defender :dead true)))

(defn living
  [tile-i]
  (find |(-?> ($ :hp) pos?) (in (dyn :world) tile-i)))

(defn xy->i
  [x y]
  (+ x (* y ww)))

(defn interact
  [o tile-i]
  (let [tile (in (dyn :world) tile-i)]
    (seq [other :in tile
          :when (other :interact)]
      (:interact other o)
      other)))

(defn fight-neighbour
  [self]
  (var fought nil)
  (let [[x y] (pos self)]
    (loop [ox :range-to [(dec x) (inc x)]
           oy :range-to [(dec y) (inc y)]
           :when (not fought)
           :when (not (and (= x ox) (= y oy)))
           :let [l (living (xy->i ox oy))]
           :when l]
      (fight l self)
      (set fought l)))
  fought)

(defn move-i
  [o target-i]
  (let [i (find-index |(index-of o $) (dyn :world))]
    (cond
      (interactive? target-i)
      (interact o target-i)

      (not (blocking? target-i))
      (-> (dyn :world)
          (update i |(filter |(not= $ o) $))
          (update target-i array/push o)))))

(defn move-dir
  [o x y]
  (let [i (find-index |(index-of o $) (dyn :world))]
    (move-i o
            (+ (+ x (mod i ww))
               (* ww (+ y (math/floor (/ i ww))))))))


(defn move-randomly
  [self]
  (let [[x y] (pos self)
        empties (seq [ox :range-to [(dec x) (inc x)]
                      oy :range-to [(dec y) (inc y)]
                      :let [i (xy->i ox oy)]
                      :when (and (not (and (= x ox) (= y oy)))
                                 (not (blocking? i)))]
                  i)
        target (math/floor
                 (* (inc (length empties))
                    (math/random)))]
    (if (zero? target)
      (print (self :name) " is just standing there.")
      (do
        (print (self :name) " shambles about.")
        (move-i self (get empties (dec target)))))))

(def zombie
  @{:name "Zombie"
    :hp 12
    :max-hp 12
    :damage 3
    :blocking true
    :color 0x552255ff
    :interact fight
    :act |(unless (fight-neighbour $)
            (move-randomly $))
    :render circle})

(def z @[ground (table/clone zombie)])

(var world
  @[X X X X X X X X X X
    X . . . . X X X X X
    X . . p . X X X X X
    X X . . X X X X X X
    X X . . . . . z X X
    X X X X X X X X X X])

(set world (seq [cell :in world]
             (if (indexed? cell)
               cell
               @[cell])))

(var offset @[#(* (+ w spacing) 0.5 (length (first world)))
              #(* (+ h spacing) 0.5 (length world))
              0 0])

(var npc-delay 40)
(var npc-turn false)


(var rw 0)
(var rh 0)

(defn render-player-ui
  [{:max-hp max-hp :hp hp}]
  (draw-rectangle 16 16 (+ max-hp 4) 20 0x444444ff)
  (draw-rectangle 18 18 max-hp 16 0x222222ff)
  (draw-rectangle 20 20 hp 12 0xaa3333ff)

  (when (number? npc-turn)
    (let [p (- 1 (/ npc-turn npc-delay))
          a (if (< p 0.5)
              (math/sin (* math/pi p))
              1)]
      (draw-rectangle 16
                      (- rh 32)
                      (math/floor (* (math/sin
                                       (* math/pi 0.5 p))
                                     (- rw 32)))
                      16
                      [0.95 0.95 0.95 a]))))

(defn on-event
  [ev]
  (with-dyns [:world world]
    (match ev
      [:key-down k]
      (when (not npc-turn)
        # (pp ev)
        (when
          (match k
            :w
            (move-dir player 0 -1)
            :a
            (move-dir player -1 0)
            :s
            (move-dir player 0 1)
            :d
            (move-dir player 1 0))
          (set npc-turn npc-delay))))))

(defn render
  [el]
  (with-dyns [:world world]
    (draw-rectangle 0 0 (el :width) (el :height) :black)
    (set rw (el :width))
    (set rh (el :height))

    (defer (rl-pop-matrix)
      (rl-push-matrix)

      # center camera on screen center
      (rl-translatef (* 0.5 (el :width)) (* 0.5 (el :height)) 0)

      # center camera on cell center
      (rl-translatef (- (* 0.5 w))
                     (- (* 0.5 h))
                     0)

      (set offset (-> (pos player)
                      (v/v* [w h])
                      (v/v* -1)))

      # then the camera offset
      (rl-translatef ;offset 0)

      (when (number? npc-turn)
        (-- npc-turn)

        (when (zero? npc-turn)
          (set npc-turn true)))

      (when (true? npc-turn)
        (loop [i :range [0 (length world)]
               :let [tile (in world i)
                     x (mod i ww)
                     y (math/floor (/ i ww))]
               o :in tile
               :when (o :act)]
          (:act o)))

      (loop [i :range [0 (length world)]
             :let [tile (in world i)
                   x (mod i ww)
                   y (math/floor (/ i ww))]
             o :in tile]

        (:render o x y)))

    (when (true? npc-turn)
      (set npc-turn false))

    (render-player-ui player)))

(start-game {:render render
             :on-event on-event})
