(use freja/flow)
(import ./state :as s)
(import ./items)
(import ./tile :fresh true)
(import ./light)
(import ./color)
(import ./dice)
(import ./animations :as anims)
(import ./screen)
(import ./text)

(defn render-inventory
  [{:inventory inv}]
  (let [size 26
        size2 20
        c [1 1 1 0.9]
        hover-c [0.7 0.7 0 1]
        c2 [1 1 1 0.8]
        x (math/floor (+ 16 (* s/rw 0.7)))
        [mx my] s/mouse-pos]

    (var y (- s/ui-top 48 s/log-h))

    (draw-text "Inventory"
               [x y]
               :size size2
               :color c2)

    (+= y size2)

    (def desc-y y)

    (loop [[k v] :pairs inv]
      (def hover (and (> mx x)
                      (> my y)
                      (< my (+ y size))))
      (when hover
        (def desc (get-in items/items [k :description]))
        (def [w h] (measure-text desc :size size))
        (draw-text desc
                   [(- x s/w 16) desc-y]
                   :size size
                   :color c)

        (when-let [desc (get-in items/items [k :flavour])]
          (def [w2 h2] (measure-text desc :size (- size 6)))
          (draw-text desc
                     [(- x w2 16) (+ h desc-y 2)]
                     :size (- size 6)
                     :color c2)))

      (draw-text (string
                   v
                   " "
                   (text/capitalize k))
                 [x y]
                 :size size
                 :color (if hover hover-c c))

      (+= y size))))

(defn render-action-log
  [_]
  (let [size 20
        spacing 6
        size2 20
        spacing2 4
        c [0.9 0.9 0.9]
        c2 [0.8 0.8 0.7]
        h (+ size2 spacing2
             (* 6 (+ size spacing)))]
    (set s/log-h h)
    (var y (- s/ui-top 48 h))

    (draw-rectangle
      0
      (- y 16)
      (* 32 s/rw)
      s/rh
      [0 0 0 1])

    (draw-text "Log"
               [16 y]
               :size size2
               :color c2)
    (+= y size2)
    (+= y spacing2)

    (loop [i :range [0 (length s/action-logs)]
           :let [arr-i
                 (mod
                   (+ i s/action-i)
                   (length s/action-logs))
                 v (in s/action-logs arr-i)
                 _ (update v 0 dec)
                 [delay l] v]
           :when (and (pos? delay)
                      l)]

      (def show-time 20)
      (def hide-time 40)

      (def alpha
        (if (> delay (* 0.5 s/action-delay))
          # showing
          (math/sin
            (* math/pi
               0.5
               (/ (min (- s/action-delay delay)
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


(defn draw-bar
  ``
  Takes a label, a value (e.g. hp), the upper bound (e.g. max-hp),
  y-offset, scale and color.
  Renders an outlined bar with the label.
  ``
  [label curr-v max-v [x y] scale color]
  (draw-text label
             [(+ x 4) y]
             :color [1 1 1 0.7]
             :size 16)
  (draw-rectangle x (+ y 16)
                  (+ 4 (math/floor (+ (* scale max-v) 4)))
                  20 0x444444ff)
  (draw-rectangle (+ x 2) (+ y 18)
                  (+ 4 (math/floor (* scale max-v)))
                  16 0x222222ff)
  (draw-rectangle (+ x 4) (+ y 20)
                  (math/floor (* scale curr-v))
                  12 color))

(var mind-shake @[0 0])

(defn render-player-ui
  [player]
  (def {:hp hp
        :max-hp max-hp
        :faith faith
        :max-faith max-faith
        :insanity insanity
        :max-insanity max-insanity
        :inventory inv} player)

  (when (mouse-button-down? 1)
    (let [mt (tile/mouse-tile)
          pos (map math/floor
                   (update
                     (screen/->screen-pos mt) 1 - s/h))
          light-mod (light/modifier mt)]

      (draw-rectangle
        (- (pos 0) 30)
        (- (pos 1) 20)
        60
        70
        [0 0 0 0.8])

      (draw-text
        (string (when (pos? light-mod) "+") light-mod)
        pos
        :color :yellow
        :center true)

      (comment draw-text
        (first (tile/->tile mt))
        [(pos 0) (+ 30 (pos 1))]
        :color :yellow
        :center true)))

  (when (not (pos? hp))
    (draw-rectangle 0 0 (inc s/rw) (inc s/rh) :black)
    (draw-text "You died"
               (v/v+ mind-shake
                     [(/ s/rw 2) (/ s/rh 2)])
               :center true
               :size 64
               :color color/dmg)
    (break))

  (when (>= insanity 4)
    (when (< 4 (dice/roll 6))
      (put mind-shake 0 (- (dice/roll 6) 6)))
    (when (< 4 (dice/roll 6))
      (put mind-shake 1 (- (dice/roll 6) 6)))
    (draw-rectangle 0 0 (inc s/rw) (inc s/rh) :black)
    (draw-text "You lost your mind"
               (v/v+ mind-shake
                     [(/ s/rw 2) (/ s/rh 2)])
               :center true
               :size 64
               :color color/insanity-dmg)
    (let [scale 1
          w (math/floor (* 0.5 s/rw))
          h (math/floor (* 0.5 s/rh))]
      (loop [x :range [(- w) w 5]
             y :range [(- h) h 5]]
        (when (< (* (- (* 2.5 w) (math/abs x))
                    (- (* 2.5 h) (math/abs y))
                    #(* (+ 60 x) (+ 60 y))
                    )
                 (dice/roll (math/pow (+ h w) 2.015)))
          (draw-pixel
            (+ 5
               (math/floor
                 (+ (* 10 (- (* 2 (math/random)) 1))
                    (*
                      scale
                      (+ w x)))))

            (+ 5
               (math/floor
                 (+ (* 10 (- (* 2 (math/random)) 1))
                    (*
                      scale
                      (+ h y)))))
            [0.5 0.5 0.5 0.5]))))
    (break))

  (draw-bar "Health" hp max-hp [16 16] 2 #0xaa3333ff
            0xff0000ff)

  (draw-bar "Faith" faith max-faith [16 62] 2 0x33aaffff)

  (loop [i :range [0 4]]
    (draw-bar (if (zero? i)
                "Insanity"
                "")
              (if (> insanity i)
                22
                0)
              22
              [(+ 16 (* i 32))
               110]
              1
              color/insanity-dmg))

  (render-action-log player)
  (render-inventory player)

  (when (number? s/npc-turn)
    (let [p (- 1 (/ s/npc-turn s/npc-delay))
          a (if (< p 0.5)
              (math/sin (* math/pi p))
              1)]
      (draw-rectangle 24
                      (- s/rh 148)
                      (math/floor (* (math/sin
                                       (* math/pi 0.5 p))
                                     (- s/rw 48)))
                      16
                      [0.95 0.95 0.95 a])))

  (let [dice (player :render-dice)
        padding 24
        nof (length dice)
        spacing 16
        total-w (- s/rw (* 2 padding)
                   (* spacing (max 5 (dec nof))))
        w (/ total-w (max nof 6))
        y (math/floor (- s/rh w (* 1.5 padding)))]

    (set s/ui-top y)

    (draw-rectangle (math/floor (* 0.5 padding))
                    y
                    (math/floor (- s/rw padding))
                    (math/floor (+ w padding))
                    (s/ui :die-bar-color))

    (loop [i :range [0 nof]
           :let [{:die die
                  :changed t} (in dice i)
                 x (math/floor (+ padding
                                  (* i (+ w spacing))))
                 y (math/floor (- s/rh padding w))
                 w (math/floor w)
                 h (math/floor w)

                 selected (= i (in player :selected-die))

                 [mx my] s/mouse-pos
                 hit (and (> my y)
                          (> mx x)
                          (< mx (+ x w)))

                 timer (- s/tick t)

                 c
                 (if (or selected hit)
                   color/highlight-die
                   [0.81 0.85 0.6
                    (+ 0.0 (if (< timer 20)
                             (+ 0.2
                                (* 0.3
                                   (math/sin (* math/pi 0.5 (/ timer 20)))))
                             0.5))])]]

      (when (and hit s/ui-press)
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

      (anims/draw-die
        x
        y
        w
        h
        die
        c)))

  (set s/ui-press false))


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
                     [(- s/rw
                         ((measure-text l
                                        :size size
                                        :font :monospace) 0)
                         16)
                      (+ 16 (* (+ spacing size) i))]
                     :font :monospace
                     :color 0xeeeeee99
                     :size size)))))

  (loop [[k l] :pairs s/named-logs]
    (draw-log i (string/format "%p %p" k l))
    (++ i))

  (loop [l :in s/logs]
    (draw-log i l)
    (++ i))

  (array/clear s/logs))

(defn log
  [& args]
  (match args
    [k v]
    (do
      (put s/named-logs k v)
      v)

    [v]
    (do
      (array/push s/logs v)
      v)))
 