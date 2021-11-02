(use freja/flow)

(var spacing 0)
(var w 32)
(var h 48)

# world width, is set later
(var ww nil)

# camera offset
(var offset @[#(* (+ w spacing) 0.5 (length (first world)))
              #(* (+ h spacing) 0.5 (length world))
              0 0])

(var npc-delay 40)
(var npc-turn false)

(var rx 0)
(var ry 0)
(var rw 0)
(var rh 0)

(var mouse-pos @[0 0])


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
    :render circle
    :inventory @{}})

(var logs @[])
(var named-logs @{})

(defn render-debug-ui
  []
  (var i 0)

  (comptime
    (def draw-log
      (let [size 24
            spacing 4]
        (fn draw-log
          [i l]
          (draw-text l
                     [(- rw
                         ((measure-text l
                                        :size size
                                        :font :monospace) 0)
                         16)
                      (+ 16 (* (+ spacing size) i))]
                     :font :monospace
                     :color 0xeeeeee99
                     :size size)))))

  (loop [[k l] :pairs named-logs]
    (draw-log i (string/format "%p %p" k l))
    (++ i))

  (loop [l :in logs]
    (draw-log i l)
    (++ i))

  (array/clear logs))

(defn log
  [& args]
  (match args
    [k v]
    (do
      (put named-logs k v)
      v)

    [v]
    (do
      (array/push logs v)
      v)))


(defn pos
  [o]
  (let [i (find-index |(index-of o $) (dyn :world))]
    [(mod i ww)
     (math/floor (/ i ww))]))

(defn mouse-tile
  []
  (let [[x y] (v/v- mouse-pos offset)]
    [(math/floor (/ x w))
     (math/floor (/ y h))]))

(defn screen-pos
  [pos]
  (-> (v/v* pos [w h])
      (v/v+ offset)
      (v/v+ [(* w 0.5) (* h 0.5)])))

(defn mouse-dir
  []
  (let [dir (->> (screen-pos (pos player))
                 (v/v- mouse-pos)
                 v/normalize
                 # if an axis is big enough, it is set to +/-1
                 (map |(if (> (math/abs $) 0.3333)
                         (/ $ (math/abs $))
                         0)))
        mt (mouse-tile)

        tile-dir (v/v- mt (pos player))]

    # if the cursor is on an adjacent tile, or on the character
    (if (> 2 (v/mag tile-dir))
      (tuple ;(v/v+ tile-dir (pos player)))
      (tuple ;(v/v+ dir (pos player))))))

(def hover-delay 5)

(defn rec
  [self x y]
  (let [{:color color
         :hover-color hover-color} self
        hc (when hover-color
             (var deccing true)

             (when (= [x y] [3 2])
               (= (mouse-dir)
                  [x y]))

             (update self :hover-time
                     (if (= (mouse-dir)
                            [x y])
                       inc
                       (do
                         (set deccing true)
                         dec)))

             (update self :hover-time
                     |(min hover-delay (max 0 $)))

             (let [p (/ (self :hover-time)
                        hover-delay)]
               [;hover-color
                (if (not deccing)
                  (math/pow 2 (* -10 p))
                  (- 1 (math/pow 2 (* -5 p))))]))
        c color]

    (draw-rectangle
      (* x w)
      (* y h)
      (- w spacing)
      (- h spacing)
      c)

    (when hc
      (draw-rectangle
        (* x w)
        (* y h)
        (- w spacing)
        (- h spacing)
        hc))

    (comment
      (draw-text
        (string x "/" y)
        [(* x w)
         (* y h)])
      #
)))

(def inner-wall
  @{:blocking true
    :color 0x111111ff
    :render rec})

(def X inner-wall)

(def ground
  @{:blocking false
    :color 0x666666ff
    :hover-color [0.7 0.7 0.7]
    :hover-time 0
    :render rec})

(def . ground)

(defn blocking?
  [i]
  (find |(and (not ($ :dead))
              ($ :blocking))
        (in (dyn :world) i)))

(defn interactive?
  [i]
  (find |(and (not ($ :dead))
              ($ :interact))
        (in (dyn :world) i)))


(def p @[(table/clone ground) player])

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

(defn move
  [o x y]
  (move-i o (+ x (* ww y))))


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
                 (* (+ 10 (length empties))
                    (math/random)))]
    (if (< target 10)
      (print (self :name) " is just standing there.")
      (do
        (print (self :name) " shambles about.")
        (move-i self (get empties (- target 10)))))))

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

(defn zero->nil
  [v]
  (if (zero? v)
    nil
    v))

(defn use-item
  [user item]
  (if-not (get-in user [:inventory item])
    (print (user :name) " does not have a " item ".")
    (do
      (print (user :name) " used a " item ".")
      (update-in user [:inventory item] |(-> $ dec zero->nil)))))

(defn pick-lock
  [door picker]
  (when (use-item picker :lockpick)
    (print "You unlocked the " (door :name) ".")
    (put door :blocking false)
    (put door :interact nil)
    (put-in door [:color 3] 0.1)))

(def locked-door
  @{:name "Locked Door"
    :hp 12
    :max-hp 12
    :blocking true
    :color @[0.2 0.2 0.2 1]
    :interact pick-lock
    :render rec})

(defn nil-safe-inc
  [v]
  (if (nil? v)
    1
    (inc v)))

(defn take-items
  [container taker]
  (if-let [is (container :items)]
    (do (put container :items nil)
      (put-in container [:color 3] 0.5)
      (print (taker :name) " took all items inside " (container :name) ".")
      (loop [i :in is]
        (update-in taker [:inventory i] nil-safe-inc)))
    (print (container :name) " is empty.")))

(defn chest
  [& items]
  @{:name "Chest"
    :hp 12
    :max-hp 12
    :blocking true
    :color @[0.8 0.8 0 1]
    :interact take-items
    :items items
    :render rec})


(var world
  (let [l @[ground (table/clone locked-door)]
        c (chest :lockpick)
        w (chest :artifact)]

    (def measure
      @[1 2 3 4 5 6 7 8 9 0 1 2 3])

    (set ww (length measure))

    (def world
      @[X X X X X X X X X X X X X
        X c . . . . . l . . . X X
        X . . p . X X X X X . . w
        X X . . X X X X X X . X X
        X X . . . . . z . . . X X
        X X X X X X X X X X X X X])

    world))

(set world (seq [cell :in world]
             (if (indexed? cell)
               cell
               @[(table/clone cell)])))

(defn render-player-ui
  [{:max-hp max-hp
    :hp hp
    :inventory inv}]
  (draw-rectangle 16 16 (+ max-hp 4) 20 0x444444ff)
  (draw-rectangle 18 18 max-hp 16 0x222222ff)
  (draw-rectangle 20 20 hp 12 0xaa3333ff)

  (let [size 26
        size2 20
        c [1 1 1 0.9]
        c2 [1 1 1 0.8]]
    (var y 46)
    (draw-text "Inventory"
               [16 y]
               :size size2
               :color c2)
    (+= y size2)

    (loop [[k v] :pairs inv]
      (draw-text (string
                   v
                   " "
                   (string/ascii-upper (string/slice k 0 1))
                   (string/slice k 1))
                 [16 y]
                 :size size
                 :color c)
      (+= y size)))

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
      [:mouse-move pos]
      (set mouse-pos (v/v- pos [rx ry]))

      [:mouse-drag pos]
      (set mouse-pos (v/v- pos [rx ry]))

      [:press pos]
      (when (and (not npc-turn)
                 (move player ;(mouse-dir)))
        (set npc-turn npc-delay))

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
    (set rx (el :render-x))
    (set ry (el :render-y))

    (defer (rl-pop-matrix)
      (rl-push-matrix)

      (comment
        # center camera on screen center
        (rl-translatef 0)

        # center camera on cell center
        (rl-translatef (- (* 0.5 w))
                       (- (* 0.5 h))
                       0))

      (set offset (-> (pos player)
                      (v/v* [w h])
                      (v/v* -1)
                      (v/v+ [(* 0.5 (el :width))
                             (* 0.5 (el :height))])
                      (v/v+ [(- (* 0.5 w))
                             (- (* 0.5 h))])))

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
               :when (and (not (o :dead))
                          (o :act))]
          (:act o)))

      (loop [i :range [0 (length world)]
             :let [tile (in world i)
                   x (mod i ww)
                   y (math/floor (/ i ww))]
             o :in tile]

        (:render o x y)))

    (when (true? npc-turn)
      (set npc-turn false))

    (render-player-ui player)

    (render-debug-ui)))

(start-game {:render render
             :on-event on-event})
