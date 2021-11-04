(use freja/flow)

(var ui-top 0)

(var spacing 0)
(var w 32)
(var h 48)

# world width, is set later
(var ww nil)

# camera offset
(var offset @[#(* (+ w spacing) 0.5 (length (first world)))
              #(* (+ h spacing) 0.5 (length world))
              0 0])

(var npc-delay 10)
(var npc-turn false)

(var rx 0)
(var ry 0)
(var rw 0)
(var rh 0)

(var ui-press false)

(var mouse-pos @[0 0])


(var action-logs (array ;(range 0 6)))

(array/fill action-logs @[0 nil])

(var action-i 0)
(var action-delay 700)

(defn action-log
  [& args]
  (put action-logs action-i @[action-delay (string ;args)])
  (++ action-i)
  (when (>= action-i (length action-logs))
    (set action-i 0)))

(defn capitalize
  [s]
  (string
    (string/ascii-upper (string/slice s 0 1))
    (string/slice s 1)))

(defn circle
  [{:color color
    :color2 color2
    :hp hp} x y]
  (when (pos? hp)
    (let [r (* 0.5 (min (- w spacing) (- h spacing)))]
      (draw-circle (math/floor (+ (* x w)
                                  (* 0.5 (- w spacing))))
                   (math/floor (+ (* y h)
                                  (* 0.5 (- h spacing))))
                   r
                   color2)

      (draw-rectangle
        (* x w)
        (math/floor (+ (* y h)))
        w

        (-
          (math/floor (+ (* y h)
                         (* 0.5 (- h spacing))))
          (math/floor (+ (* y h))))

        color2)

      (draw-circle (math/floor (+ (* x w)
                                  (* 0.5 (- w spacing))))
                   (math/floor (+ (* y h)))
                   r
                   color))))

(def player
  @{:name "Saikyun"
    :dice @[1 4 3 2 5 6]
    :hp 30
    :max-hp 42
    :damage 3
    :blocking true
    :difficulty 4
    :color 0x003333ff
    :color2 0x002222ff
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

(defn on-ui?
  []
  (>= (mouse-pos 1) ui-top))

(defn rec
  [self x y]
  (let [{:color color
         :hover-color hover-color} self
        hc (when (and hover-color
                      (not (on-ui?)))
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


(defn rec2
  [self x y]
  (let [{:color color
         :color2 color2
         :offset offset
         :hover-color hover-color} self]

    (draw-rectangle
      (* x w)
      (* y h)
      (- w spacing)
      (- h spacing)
      color)

    (draw-rectangle
      (* x w)
      (+ (* y h) offset)
      (- w spacing)
      (- h spacing offset)
      color2)

    (comment
      (draw-text
        (string x "/" y)
        [(* x w)
         (* y h)])
      #
)))

(defn door-rec
  [self x y]
  (let [{:color color
         :color2 color2
         :color3 color3
         :offset offset
         :hover-color hover-color} self]

    (draw-rectangle
      (* x w)
      (math/floor (- (* y h) (* h 0.7)))
      (- w spacing)
      (- h spacing)
      color2)

    (draw-rectangle
      (* x w)
      (math/floor (+ (* h 0.3) (- (* y h) (* h 0.7))))
      (- w spacing)
      (math/floor (- h spacing (* h 0.3)))
      color3)

    (draw-rectangle
      (+ (* x w) offset)
      (math/floor (- (* y h) (* h 0.2)))
      (- w spacing (* 2 offset))
      (math/floor (+ (- h spacing) (* h 0.2)))
      color)

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

(def inner-wall-down
  @{:blocking true
    :color 0x111111ff
    :color2 0x333333ff
    :offset 15
    :render rec2})

(def X inner-wall)
(def x inner-wall-down)

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
  [defender attacker &keys {:difficulty difficulty
                            :total total}]
  (def dmg (+ (attacker :damage)
              (- total difficulty)))
  (action-log (attacker :name) " dealt " dmg " damage to " (defender :name) ".")
  (update defender :hp - dmg)

  (unless (pos? (defender :hp))
    (action-log (defender :name) " died in a fight against " (attacker :name) ".")
    (put defender :dead true)))

(defn living
  [tile-i]
  (find |(-?> ($ :hp) pos?) (in (dyn :world) tile-i)))

(defn xy->i
  [x y]
  (+ x (* y ww)))

(defn interact
  [o tile-i]
  (let [tile (in (dyn :world) tile-i)
        any-difficult (find |($ :difficulty) tile)]

    (if (and any-difficult (not (o :selected-die)))
      (action-log (o :name) " does not have a die selected.")

      (let [total (if-not any-difficult
                    0
                    (let [roll (inc (math/floor (* 6 (math/random))))
                          die-i (o :selected-die)
                          selected-die (get-in o [:dice die-i])
                          total (+ roll selected-die)]
                      (action-log (string (o :name) " used a " selected-die ", and rolled a " roll " for a total of " total "..."))
                      (put-in o [:dice die-i] 0)
                      (put o :selected-die nil)
                      total))]

        (seq [other :in tile
              :let [difficulty (other :difficulty)
                    _ (cond
                        (other :hp)
                        (action-log (other :name) " has armour rank " (other :difficulty) ".")
                        difficulty
                        (action-log (other :name) " has difficulty " (other :difficulty) "."))]
              :when (and (other :interact)
                         (or (not difficulty)
                             (>= total difficulty)))]
          (when difficulty
            (action-log "Success!"))
          (:interact other o
                     :total total
                     :difficulty difficulty)
          other)))))

(defn roll-die
  [n]
  (inc (math/floor (* n (math/random)))))

(defn fight-neighbour
  [self]
  (var fought nil)
  (let [[x y] (pos self)]
    (loop [ox :range-to [(dec x) (inc x)]
           oy :range-to [(dec y) (inc y)]
           :when (not fought)
           :when (not (and (= x ox) (= y oy)))
           :let [l (living (xy->i ox oy))
                 difficulty (get l :difficulty)]
           :when l]
      (def res (roll-die 6))
      (if (>= res difficulty)
        (fight l self :difficulty difficulty
               :total res)
        (action-log (self :name) " glanced on " (l :name) "s armour."))

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
      #(action-log (self :name) " is just standing there.")
      123
      (do
        # (action-log (self :name) " shambles about.")
        (move-i self (get empties (- target 10)))))))

(def zombie
  @{:name "Zombie"
    :hp 12
    :max-hp 12
    :damage 3
    :difficulty 5
    :blocking true
    :color 0x552255ff
    :color2 0x441144ff
    :interact fight
    :selected-dice 0
    :dice @[0]
    :act |(unless
            (fight-neighbour $)
            (move-randomly $))
    :render circle})

(def z @[ground zombie])

(defn zero->nil
  [v]
  (if (zero? v)
    nil
    v))

(defn use-item
  [user item]
  (if-not (get-in user [:inventory item])
    (action-log (user :name) " does not have a " item ".")
    (do
      (action-log (user :name) " used a " item ".")
      (update-in user [:inventory item] |(-> $ dec zero->nil)))))

(defn pick-lock
  [door picker &keys {:difficulty difficulty
                      :total total}]
  (when (use-item picker :lockpick)
    (action-log "Success! " (picker :name) " unlocked the " (door :name) ".")
    (put door :blocking false)
    (put door :interact nil)
    (put-in door [:color 3] 0.0)))

(def locked-door
  @{:name "Locked Door"
    :hp 12
    :max-hp 12
    :blocking true
    :color @[0.5 0.5 0.5 1]
    :color2 @[0.1 0.1 0.1 1]
    :color3 @[0.3 0.3 0.3 1]
    :offset 10
    :difficulty 7
    :interact pick-lock
    :render door-rec})

(defn nil-safe-inc
  [v]
  (if (nil? v)
    1
    (inc v)))

(defn take-items
  [container taker &keys {:difficulty difficulty
                          :total total}]
  (if-let [is (container :items)]
    (do (put container :items nil)
      (put-in container [:color 3] 0.5)
      (action-log (taker :name) " found "
                  (string/join
                    (map capitalize is)
                    ", ")
                  " inside " (container :name) ".")
      (put container :difficulty nil)
      (loop [i :in is]
        (update-in taker [:inventory i] nil-safe-inc)))
    (action-log (container :name) " is empty.")))

(defn chest
  [& items]
  @{:name "Chest"
    #:hp 12
    #:max-hp 12
    :blocking true
    :color @[0.8 0.8 0 1]
    :color2 @[0.6 0.6 0 1]
    :offset 30
    :interact take-items
    :items items
    :render rec2})


(var world
  (let [l @[ground locked-door]
        c (chest :lockpick)
        w (chest :artifact)]

    (def measure
      @[1 2 3 4 5 6 7 8 9 0 1 2 3])

    (def world
      @[X x x x x x x x x x x X X X X X >
        X c . . . . . l . . . x x x X X >
        X . . p . X X X X X . . . . X X >
        X X . . x x x x x x . X X . X X >
        X X . . . . . z . . . X X . X X >
        X X X X X X X X X X X X X . X X >
        X X X X X X X X X X X X X . X X >
        X X X X X X X X X X X X X . X X >
        X X X X X X X X X x x x x . X X >
        X X X X x x x x x . . . . . X X >
        X X X X . . . . . . . . . . X X >
        X X X X . z . . . . z . X X X X >
        X X X X c . . . . . . . X X X X >
        X X X X X X X X X . . X X X X X >
        X X X X X X X X X . . X X X X X >
        X X X X X X X X X . . X X X X X >
        X X X X X X X X X X X X X X X X >
        #
])

    world))

(set ww 0)
(var acc 0)

(set world (seq [cell :in world
                 :let [_ (if (= > cell)
                           (set acc 0)
                           (do
                             (++ acc)
                             (set ww (max ww acc))))]
                 :when (not= > cell)]
             (cond (indexed? cell)
               (map |(if (= player $)
                       $
                       (table/clone $))
                    cell)
               @[(table/clone cell)])))

(defn render-inventory
  [{:inventory inv}]
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
                   (capitalize k))
                 [16 y]
                 :size size
                 :color c)
      (+= y size))))


(defn render-action-log
  [_]
  (let [size 20
        spacing 6
        size2 16
        spacing2 4
        c [0.9 0.9 0.9]
        c2 [0.8 0.8 0.8]]
    (var y (- ui-top (+ size2 spacing2
                        (* 6 (+ size spacing)))))

    (draw-rectangle
      16
      (- y 16)
      400
      300
      [0 0 0 0.8])

    (draw-text "Log"
               [16 y]
               :size size2
               :color c2)
    (+= y size2)
    (+= y spacing2)

    (loop [i :range [0 (length action-logs)]
           :let [arr-i
                 (mod
                   (+ i action-i)
                   (length action-logs))
                 v (in action-logs arr-i)
                 _ (update v 0 dec)
                 [delay l] v]
           :when (and (pos? delay)
                      l)]

      (def show-time 20)
      (def hide-time 40)

      (def alpha
        (if (> delay (* 0.5 action-delay))
          # showing
          (math/sin
            (* math/pi
               0.5
               (/ (min (- action-delay delay)
                       show-time)
                  show-time)))

          # hiding
          (math/sin
            (* math/pi
               0.5
               (/ (min delay
                       hide-time)
                  hide-time)))))

      (draw-text l
                 [16 y]
                 :size size
                 :color [;c alpha])

      (+= y (* (min 1 (* 2 alpha)) (+ spacing size))))))

(defn render-player-ui
  [player]
  (def {:max-hp max-hp
        :hp hp
        :inventory inv} player)
  (draw-rectangle 16 16 (+ max-hp 4) 20 0x444444ff)
  (draw-rectangle 18 18 max-hp 16 0x222222ff)
  (draw-rectangle 20 20 hp 12 0xaa3333ff)

  (render-inventory player)
  (render-action-log player)

  (when (number? npc-turn)
    (let [p (- 1 (/ npc-turn npc-delay))
          a (if (< p 0.5)
              (math/sin (* math/pi p))
              1)]
      (draw-rectangle 24
                      (- rh 168)
                      (math/floor (* (math/sin
                                       (* math/pi 0.5 p))
                                     (- rw 48)))
                      16
                      [0.95 0.95 0.95 a])))

  (let [dice (player :dice)
        padding 24
        nof (length dice)
        spacing 16
        total-w (- rw (* 2 padding)
                   (* spacing (dec nof)))
        w (/ total-w nof)
        y (math/floor (- rh w (* 1.5 padding)))]

    (set ui-top y)

    (draw-rectangle (math/floor (* 0.5 padding))
                    y
                    (math/floor (- rw padding))
                    (math/floor (+ w padding))
                    [0.2 0.2 0.2 1])

    (loop [i :range [0 nof]
           :let [die (in dice i)
                 x (math/floor (+ padding
                                  (* i (+ w spacing))))
                 y (math/floor (- rh padding w))
                 w (math/floor w)
                 h (math/floor w)

                 selected (= i (in player :selected-die))

                 [mx my] mouse-pos
                 hit (and (> my y)
                          (> mx x)
                          (< mx (+ x w)))
                 c
                 (if (or selected hit)
                   [0.81 0.85 0.6 1]
                   [0.81 0.85 0.6 0.5])]]

      (when (and hit ui-press)
        (if selected
          (put player :selected-die nil)
          (put player :selected-die i)))

      (if selected
        (draw-rectangle
          (- x 8)
          (- y 8)
          (+ w 16)
          (+ h 16)
          [0.6 0.2 0.6 0.8])
        (draw-rectangle
          (+ x 2)
          (+ y 2)
          (+ w 2)
          (+ h 2)
          [0 0 0 0.5]))

      (draw-rectangle
        x y w h
        c)

      (let [die-col [0.01 0.15 0.2 1]
            r (math/floor (* 0.1 w))
            mid-x (math/floor (+ x (* 0.5 w)))
            mid-y (math/floor (+ y (* 0.5 w)))
            l-x (math/floor (+ x (* 0.2 w)))
            t-y (math/floor (+ y (* 0.2 w)))
            r-x (math/floor (+ x (* 0.8 w)))
            b-y (math/floor (+ y (* 0.8 w)))]

        (case die
          1 (draw-circle mid-x mid-y r die-col)
          2 (do (draw-circle l-x t-y r die-col)
              (draw-circle r-x b-y r die-col))
          3 (do (draw-circle r-x t-y r die-col)
              (draw-circle mid-x mid-y r die-col)
              (draw-circle l-x b-y r die-col))
          4 (do (draw-circle r-x t-y r die-col)
              (draw-circle r-x b-y r die-col)
              (draw-circle l-x b-y r die-col)
              (draw-circle l-x t-y r die-col))
          5 (do (draw-circle r-x t-y r die-col)
              (draw-circle r-x b-y r die-col)
              (draw-circle l-x b-y r die-col)
              (draw-circle l-x t-y r die-col)
              (draw-circle mid-x mid-y r die-col))
          6 (do (draw-circle r-x t-y r die-col)
              (draw-circle r-x b-y r die-col)
              (draw-circle l-x b-y r die-col)
              (draw-circle l-x t-y r die-col)
              (draw-circle l-x mid-y r die-col)
              (draw-circle r-x mid-y r die-col))))))

  (set ui-press false))

(defn on-event
  [ev]
  (with-dyns [:world world]
    (match ev
      [:mouse-move pos]
      (set mouse-pos (v/v- pos [rx ry]))

      [:mouse-drag pos]
      (set mouse-pos (v/v- pos [rx ry]))

      [:press pos]
      (if (on-ui?)
        (set ui-press true)
        (when (and (not npc-turn)
                   (move player ;(mouse-dir)))
          (set npc-turn npc-delay)))

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
                             (* 0.5 ui-top)])
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
