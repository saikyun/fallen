(import freja/vector-math :as v)
(import ./state :as s)
(import ./tile)
(import ./dice)
(import ./log)
(import ./light)
# should these be here?
(import ./animations :as anims)
(import ./color)

(defn living?
  [o]
  (find |(-?> ($ :hp) pos?) (tile/->tile o)))

(defn blocking?
  [o]
  (find |(and (not ($ :dead))
              ($ :blocking)) (tile/->tile o)))

(defn interactive?
  [o]
  (find |(and (not ($ :dead))
              ($ :interact))
        (tile/->tile o)))

(defn move
  [o target]
  (-> (dyn :world)
      (update (tile/->i o) |(filter |(not= $ o) $))
      (update (tile/->i target) array/push o))
  (put o :pos (tile/->pos target)))



(defn interact-with-tile
  [o tile total]
  (seq [other :in tile
        :let [difficulty (other :difficulty)]
        :when (other :interact)]
    # if the roll fails
    (if (and difficulty
             # this means failed roll
             (< total difficulty))

      (if (pos? (o :faith))
        (let [faith-dmg (dice/roll 6)]
          (log/action-log (o :name) "'s faith is being tested...")
          #          (when-let [t (other :fail-quotes-table)]
          #            (flash-text-above o (get-table t faith-dmg)))
          (update o :faith - faith-dmg)
          (anims/dmg o faith-dmg :color color/faith-dmg))
        (let [insanity-dmg 1]
          (log/action-log (o :name) " is losing it...")
          (update o :insanity + insanity-dmg)
          (anims/dmg o insanity-dmg :color color/insanity-dmg)))

      (do # roll succeded!
        (when difficulty
          (log/action-log "Success!"))
        (:interact other o
                   :total total
                   :difficulty difficulty)))
    other))

(defn interact
  [o target]
  (let [tile (tile/->tile target)
        any-difficult (find |($ :difficulty) tile)
        die-i (o :selected-die)
        selected-die (get-in o [:dice die-i])]
    # if one must choose a die, use the first branch
    (if (and false # disabled branch
             any-difficult
             (not (o :selected-die)))
      (do
        (log/action-log (o :name) "'s faith is lacking.")
        (anims/flash-die-bar any-difficult)
        nil)

      (if-not any-difficult
        (interact-with-tile o tile 0)

        (let [roll (dice/roll 6)
              bonus (light/modifier any-difficult)
              diff (any-difficult :difficulty)]
          (when selected-die
            (put o :selected-die nil)
            (put-in o [:render-dice die-i] @{:changed s/tick
                                             :die 0}))

          (anims/die-roll
            roll
            :extra
            selected-die

            :difficulty-label
            (string/format
              ``
              Base: %d
              Light: %d
              Difficulty: %d
              ``
              diff
              bonus
              (- diff bonus))

            :target
            (- diff bonus)

            :after
            (fn []
              (let [total # with selected die
                    (if selected-die
                      (let [total (+ roll selected-die)]
                        (put-in o [:dice die-i] 0)
                        (put o :selected-die nil)
                        total)

                      (do # no die selected
                        roll))
                    total (+ total bonus)]

                (interact-with-tile o tile total)

                (set s/npc-turn s/npc-delay)))))))))


(defn fight
  [defender attacker &keys {:difficulty difficulty
                            :total total}]
  (def dmg (+ (attacker :damage)
              (- total difficulty)))

  (print "huh?")
  (anims/dmg defender dmg :color color/dmg)
  (log/action-log (attacker :name) " dealt " dmg " damage to " (defender :name) ".")
  (update defender :hp - dmg)

  (unless (pos? (defender :hp))
    (log/action-log (defender :name) " died in a fight against " (attacker :name) ".")
    (put defender :dead true)))


(defn fight-neighbour
  [self]
  (var fought nil)

  (let [[x y] (self :pos)]
    (loop [ox :range-to [(dec x) (inc x)]
           oy :range-to [(dec y) (inc y)]
           :when (not fought)
           :when (not (and (= x ox) (= y oy)))
           :let [l (living? [ox oy])
                 difficulty (get l :difficulty)]
           :when l]
      (def res (dice/roll 6))
      (if (>= res difficulty)
        (fight l self :difficulty difficulty
               :total res)
        (log/action-log (self :name) " glanced on " (l :name) "s armour."))

      (set fought l)))
  fought)

(defn try-move
  [o target]
  (cond
    (interactive? target)
    (interact o target)

    (not (blocking? target))
    (do
      (move o target)
      
      (when (= o s/player)
        (set s/npc-turn s/npc-delay)))))

(defn try-move-dir
  [o dir]
  (try-move o (v/v+ (o :pos) dir)))

(defn chase-target
  [self]
  (var blocked nil)
  (def points (tile/points-between-line self s/player))

  (loop [[x y] :in points
         :let [x (math/floor x)
               y (math/floor y)
               blocking (blocking? [x y])
               ]

         :when (and blocking
                    (not= self blocking))]
    (set blocked [x y])
    #(break)
    )

  (when blocked
    (put self :target nil))

  (cond
    (self :target)
    (do
      (print "moving to target")
      (move self ;(map math/floor (points 1)))
      (put self :last-known-pos (get-in self [:target :pos]))
      true)

    (self :last-known-pos)
    (when-let [p (get (tile/points-between-line
                        self
                        (self :last-known-pos)) 1)]
      (def target (map math/floor p))
      (do (anims/flash-text-above self "?")
        (move self target)
        true))))

(defn find-target
  [self]

  (var blocked nil)

  (def points (tile/points-between-line
                self
                s/player))

  (loop [[x y] :in points
         :let [x (math/floor x)
               y (math/floor y)
               pos [x y]
               blocking (blocking? pos)
               ]

         :when (and blocking
                    (not= self blocking)
                    (not= s/player blocking))]
    (set blocked pos)
    #(break)
    )

  (when blocked
    (put self :target nil))

  (cond (= (self :pos) (self :last-known-pos))
    (do (put self :last-known-pos nil)
      (anims/flash-text-above self ":("))

    (not blocked)
    (do (put self :target s/player)
      (anims/flash-text-above self "!!")
      (put self :last-known-pos (get-in self [:target :pos]))
      true)))









(defn move-randomly
  [self]
  (let [pos (self :pos)
        [x y] pos
        empties (seq [ox :range-to [(dec x) (inc x)]
                      oy :range-to [(dec y) (inc y)]
                      :let [o-pos [ox oy]]
                      :when (and (not (and (= x ox) (= y oy)))
                                 (not (blocking? o-pos)))]
                  o-pos)
        target (math/floor
                 (* (+ 0 (length empties))
                    (math/random)))]
    (if (< target 0)
      #(action-log (self :name) " is just standing there.")
      123
      (do
        # (action-log (self :name) " shambles about.")
        (move self (get empties (- target 0)))))))



 