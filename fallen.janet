(use freja/flow)
(import freja/frp)
(import freja/assets)

(defn points-between-line
  [[emx emy] [pmx pmy]]

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

(defn roll-die
  [n]
  (inc (math/floor (* n (math/random)))))

(var tick 0)


(var ui-top 0)

(var world-list nil)

(def highlight-die-color [0.81 0.85 0.6])
(def success-die-color [0.5 0.95 0.3])
(def fail-die-color [0.91 0.5 0.3])

(var spacing 0)
(var w 32)
(var h 48)

# world width, is set later
(var ww nil)

# camera offset
(var offset @[#(* (+ w spacing) 0.5 (length (first world)))
              #(* (+ h spacing) 0.5 (length world))
              0 0])

(def die-bar-color [0.2 0.2 0.2 1])
(def ui @{:die-bar-color die-bar-color})

(var npc-delay 10)
(var npc-turn false)

(var rx 0)
(var ry 0)
(var rw 0)
(var rh 0)

(var ui-press false)

(var mouse-pos @[0 0])

(var action-logs (range 0 6))

(array/fill action-logs @[0 nil])

(var action-i 0)
(var action-delay 700)


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


(var animations @[])

(defn run-animations
  [anims]
  (var i 0)
  (while (< i (length animations))
    (let [a (in animations i)]
      (if-not (fiber/can-resume? a)
        (array/remove animations i)
        (do
          (try (resume a)
            ([err fib]
              (debug/stacktrace fib err)))
          (++ i))))))

(defmacro anim
  [& body]
  ~(array/push animations
               (fiber/new (fn []
                            ,;body))))

(defn ease-out-elastic
  [p]
  (let [c4 (/ (* 2 math/pi) 3)]
    (case p
      0 0

      1 1

      (inc
        (* (math/pow 2 (* -10 p))
           (math/sin (* (- (* p 10) 0.75)
                        c4))))
      #
)))

(defn ease-out-bounce
  [p]
  (let [n1 7.5625
        d1 2.75]
    (cond
      (< p (/ 1 d1))
      (* n1 p p)

      (< p (/ 2 d1))
      (+ (* n1
            (math/pow (- p (/ 1.5 d1)) 2))
         0.75)

      #(log :2 (* n1
      #          (- p (/ 1.5 d1))
      #         (+ p
      #           0.75)))

      (< p (/ 2.5 d1))
      (+ (* n1
            (math/pow (- p (/ 2.25 d1)) 2))
         0.9375)

      # else
      (+ (* n1
            (math/pow (- p (/ 2.625 d1)) 2))
         0.984375)
      #
)))


(defn draw-die
  [x y w h die c]

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
          (draw-circle r-x mid-y r die-col)))))

(defn ease-in-expo
  [p]
  (if (zero? p)
    0
    (math/pow 2 (- (* 10 p) 10))))

(defn die-roll-anim
  [final-res &keys {:x x
                    :difficulty-label diff-label
                    :extra extra
                    :target target
                    :after after}]

  (defn draw-extra
    [x y col]
    (when extra
      (draw-die
        (math/floor (- x 100))
        (math/floor (+ 10 y))
        60
        60
        extra
        col)))

  (defn draw-target
    []
    (when target
      (draw-text (string diff-label target)
                 [(/ rw 2) (* rh 0.4)]
                 :color :white
                 :size 32
                 :center true)))

  (default x 0)
  (anim
    (def dur-scale 1)

    (def dur (math/floor (* dur-scale 40)))
    (var res nil)
    (var delay 0)

    (loop [i :range [0 (inc dur)]
           :let [p (/ i dur)
                 px (math/sin (* math/pi 0.5 p))
                 py (- 1 (ease-out-bounce p))]]
      (when (<= delay 0)
        (set res (roll-die 6))
        (set delay (* 10 p))
        (when (>= (+ i delay) dur)
          (set res final-res)))
      (-- delay)
      (draw-target)

      (draw-extra (+ x (* 0.5 rw))
                  (* rh 0.5)
                  highlight-die-color)
      (yield (draw-die
               (- (math/floor (+ x (- rw (* 0.5 rw px)))) 30)
               (math/floor (- (* rh 0.5) (* rh 0.5 py)))
               60
               60
               res
               highlight-die-color)))

    (def dur2 (math/floor (* dur-scale 5)))
    (loop [i :range [0 (inc dur2)]
           :let [scale (* 0.00206
                          (math/pow 2
                                    (* 10 (math/sin (* math/pi
                                                       0.5
                                                       (/ i dur2))))))
                 scale (* 0.2 scale)
                 col (cond
                       (and target (>= (+ (or extra 0) final-res) target))
                       success-die-color

                       target
                       fail-die-color

                       highlight-die-color)]]
      (draw-target)
      (rl-push-matrix)
      (rl-translatef
        (math/floor (- (+ x (- (* 0.5 rw) (* 0.5 60 scale))) 100 30))
        (math/floor (- (* rh 0.5) (* 1 60 scale)))
        0)
      (rl-scalef (inc scale) (inc scale) 1)

      (when extra
        (draw-die
          0
          0
          60
          60
          extra
          col))

      (rl-pop-matrix)

      (rl-push-matrix)
      (rl-translatef
        (math/floor (+ -30 x (- (* 0.5 rw) (* 0.5 60 scale))))
        (math/floor (- (* rh 0.5) (* 1 60 scale)))
        0)
      (rl-scalef (inc scale) (inc scale) 1)

      (draw-die
        0
        0
        60
        60
        res
        col)
      (yield (rl-pop-matrix)))

    (def dur3 (math/floor (* dur-scale 20)))
    (loop [i :range [0 (inc dur3)]
           :let [p (/ i dur3)
                 scale 0.4
                 col (cond
                       (and target (>= (+ (or extra 0) final-res) target))
                       success-die-color

                       target
                       fail-die-color

                       highlight-die-color)]]
      (draw-target)
      (rl-push-matrix)
      (rl-translatef
        (math/floor (- (+ -30 x (- (* 0.5 rw) (* 0.5 60 scale))) 100))
        (math/floor (- (* rh 0.5) (* 1 60 scale)))
        0)
      (rl-scalef (inc scale) (inc scale) 1)

      (when extra
        (draw-die
          0
          0
          60
          60
          extra
          col))
      (rl-pop-matrix)

      (rl-push-matrix)
      (rl-translatef
        (math/floor (+ x -30 (- (* 0.5 rw) (* 0.5 60 scale))))
        (math/floor (- (* rh 0.5) (* 1 60 scale)))
        0)
      (rl-scalef (inc scale) (inc scale) 1)
      (draw-die
        0
        0
        60
        60
        res
        col)
      (yield (rl-pop-matrix)))

    (def dur4 (math/floor (* dur-scale 20)))
    (loop [i :range [0 (inc dur4)]
           :let [p (/ i dur4)
                 a (- 1 (ease-in-expo p))
                 scale (- 0.4 (* 0.5 (ease-in-expo p)))
                 col [;(cond
                         (and target (>= (+ (or extra 0) final-res) target))
                         success-die-color

                         target
                         fail-die-color

                         highlight-die-color)
                      a]]]
      (draw-target)
      (rl-push-matrix)
      (rl-translatef
        (math/floor (- (+ -30 x (- (* 0.5 rw) (* 0.5 60 scale))) 100))
        (math/floor (- (* rh 0.5) (* 1 60 scale)))
        0)
      (rl-scalef (inc scale) (inc scale) 1)

      (when extra
        (draw-die
          0
          0
          60
          60
          extra
          col))
      (rl-pop-matrix)

      (rl-push-matrix)
      (rl-translatef
        (math/floor (+ x -30 (- (* 0.5 rw) (* 0.5 60 scale))))
        (math/floor (- (* rh 0.5) (* 1 60 scale)))
        0)
      (rl-scalef (inc scale) (inc scale) 1)
      (draw-die
        0
        0
        60
        60
        res
        col)
      (yield (rl-pop-matrix)))
    (after)))

(defn ease-out
  [p]
  (math/sin (* p math/pi 0.5)))

(defn ease-out-quad
  [p]
  (- 1 (math/pow (- 1 p) 2)))


(defn nice-flash!!!
  []
  (anim
    (def dur 20)
    (def col @[0 0 0 1])
    (loop [i :range-to [0 dur]
           :let [p (ease-out-quad (/ i dur))
                 p (+ 0.2 (* 0.2 p))]]
      (put col 0 p)
      (put col 1 p)
      (put col 2 p)
      (yield (put ui :die-bar-color col)))
    (loop [i :range-to [0 dur]
           :let [p (/ i dur)
                 p (+ 0.2 (* 0.8 (- 1 p)))]]
      (put col 0 p)
      (put col 1 p)
      (put col 2 p)
      (yield (put ui :die-bar-color col)))))


(defn pos
  [o]
  (let [i (find-index |(index-of o $) (dyn :world))]
    [(mod i ww)
     (math/floor (/ i ww))]))

(defn screen-pos
  [pos]
  (-> (v/v* pos [w h])
      (v/v+ offset)
      (v/v+ [(* w 0.5)
             (* h 0.5)])))


(defn get-table
  [t res]
  (var outcome nil)
  (loop [v :in (reverse (partition 2 t))]
    (when (>= res (v 0))
      (set outcome (v 1))
      (break)))
  outcome)

(defn roll-table
  [t die]
  (def res (roll-die die))
  (get-table t res))


(defonce player
  @{})

(defn circle
  [o x y]
  (def {:color color
        :color2 color2
        :hp hp} o)

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

(merge-into
  player
  @{:name "Saikyun"
    :dice (seq [_ :range [0 3]]
            (roll-die 6))
    :render-dice @[]
    :hp 17
    :max-hp 17
    :faith 14
    :max-faith 22
    :insanity 0
    :max-insanity 70
    :damage 3
    :blocking true
    :difficulty 2
    :color 0x003333ff
    :color2 0x002222ff
    :render :circle
    :inventory @{}})

(if-not (empty? (player :render-dice))
  (loop [d :in (player :render-dice)]
    (put d :changed tick))
  (anim
    (loop [i :range [0 (length (player :dice))]
           :let [n (in (player :dice) i)]]
      (loop [_ :range [0 (+ 5 (roll-die 18))]]
        (yield nil))
      (yield (die-roll-anim n
                            :after |(put-in player [:render-dice i]
                                            @{:die n
                                              :changed tick})
                            :x (* -90
                                  (- (* 0.5 (length (player :dice)))
                                     i)))))))

(defn dmg-anim
  [o dmg &keys {:color color}]
  (anim
    (with-dyns [:world world-list]
      (def col @[;color 0])
      (def dur 60)

      (loop [i :range-to [0 dur]
             :let [p2 (ease-out (/ (* 1 i) dur))
                   p (math/sin (* math/pi (/ i dur)))]]
        # fades in / out alpha
        (put col 3 p)
        (yield (draw-text (string dmg)
                          (let [pos (screen-pos (pos o))]
                            (update pos 1 - (* (+ 0.6 (* 0.3 p2)) h)))
                          :color col
                          :size 32
                          :center true))))))


(comment

  (dmg-anim player 10)
  #
)

(var latest-above
  @{})

(defn flash-text-above
  [o text]
  (def text (buffer text))
  (put latest-above o text)
  (anim
    (with-dyns [:world world-list]
      (def dur 20)
      (def col @[0 0 0 1])

      (loop [i :range-to [0 dur]
             :let [p (ease-out-quad (/ i dur))
                   p (+ 0.2 (* 0.8 p))]
             :when (= (latest-above o) text)]
        (put col 0 p)
        (put col 1 p)
        (put col 2 p)
        (yield (draw-text text
                          (let [pos (screen-pos (pos o))]
                            (update pos 1 - (* 2 h)))
                          :color col
                          :center true)))
      (loop [i :range-to [0 200]
             :when (= (latest-above o) text)]
        (yield (draw-text text
                          (let [pos (screen-pos (pos o))]
                            (update pos 1 - (* 2 h)))
                          :color col
                          :center true)))
      (loop [i :range-to [0 dur]
             :let [p (/ i dur)
                   p (+ 0.2 (* 0.8 (- 1 p)))]
             :when (= (latest-above o) text)]
        (put col 0 p)
        (put col 1 p)
        (put col 2 p)
        (yield (draw-text text
                          (let [pos (screen-pos (pos o))]
                            (update pos 1 - (* 2 h)))
                          :color col
                          :center true))))))

(defn flash-text-on
  [o text]
  (def text (buffer text))
  (put latest-above o text)
  (anim
    (with-dyns [:world world-list]
      (def dur 20)
      (def col @[1 1 1 1])

      (loop [i :range-to [0 dur]
             :when (= (latest-above o) text)]
        (yield (draw-text text
                          (let [pos (screen-pos (pos o))]
                            (update pos 1 - (* 0 h)))
                          :color col
                          :center true))))))

(defn flash-die-bar
  [o]
  (anim
    (with-dyns [:world world-list]
      (def dur 20)
      (def col @[0 0 0 1])

      (loop [i :range-to [0 dur]
             :let [p (ease-out-quad (/ i dur))
                   p (+ 0.2 (* 0.8 p))]]
        (put col 0 p)
        (put col 1 p)
        (put col 2 p)
        (yield (draw-text "Select a die"
                          (let [pos (screen-pos (pos o))]
                            (update pos 1 - h))
                          :color col
                          :center true)))
      (loop [i :range-to [0 200]]
        (yield (draw-text "Select a die"
                          (let [pos (screen-pos (pos o))]
                            (update pos 1 - h))
                          :color col
                          :center true)))
      (loop [i :range-to [0 dur]
             :let [p (/ i dur)
                   p (+ 0.2 (* 0.8 (- 1 p)))]]
        (put col 0 p)
        (put col 1 p)
        (put col 2 p)
        (yield (draw-text "Select a die"
                          (let [pos (screen-pos (pos o))]
                            (update pos 1 - h))
                          :color col
                          :center true)))))

  (anim

    (loop [i :range-to [0 40]]
      (yield))
    (def dur 20)
    (def col @[0 0 0 1])
    (loop [i :range-to [0 dur]
           :let [p (ease-out-quad (/ i dur))
                 p (+ 0.2 (* 0.8 p))]]
      (put col 0 p)
      (put col 1 p)
      (put col 2 p)
      (yield (put ui :die-bar-color col)))
    (loop [i :range-to [0 dur]
           :let [p (/ i dur)
                 p (+ 0.2 (* 0.8 (- 1 p)))]]
      (put col 0 p)
      (put col 1 p)
      (put col 2 p)
      (yield (put ui :die-bar-color col)))))

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


(defn mouse-tile
  []
  (let [[x y] (v/v- mouse-pos offset)]
    [(math/floor (/ x w))
     (math/floor (/ y h))]))


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

(def render-functions
  {:rec rec
   :rec2 rec2
   :circle circle
   :door-rec door-rec})

(def inner-wall
  @{:blocking true
    :color 0x111111ff
    :render :rec})

(def inner-wall-down
  @{:blocking true
    :color 0x111111ff
    :color2 0x333333ff
    :offset 15
    :render :rec2})

(def X inner-wall)
(def x inner-wall-down)

(def ground
  @{:blocking false
    :color [0.1 0.1 0.08]
    :hover-color [0.7 0.7 0.7]
    :hover-time 0
    :render :rec})

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

(def dmg-color [0.9 0.1 0.1])
(def faith-dmg-color [0.3 0.3 0.9])
(def insanity-dmg-color #[0.7 0.1 0.7]
  [0.3 0.8 0.3])

(defn fight
  [defender attacker &keys {:difficulty difficulty
                            :total total}]
  (def dmg (+ (attacker :damage)
              (- total difficulty)))

  (print "huh?")
  (dmg-anim defender dmg :color dmg-color)
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


(defn interact-with-tile
  [o tile total]
  (seq [other :in tile
        :let [difficulty (other :difficulty)
              _ (cond
                  (other :hp)
                  (action-log (other :name)
                              " has armour rank "
                              (other :difficulty) ".")
                  difficulty
                  (action-log (other :name) " has difficulty " (other :difficulty) "."))]
        :when (other :interact)]
    # if the roll fails
    (if (and difficulty
             (< total difficulty))

      (if (pos? (o :faith))
        (let [faith-dmg (roll-die 6)]
          (action-log (o :name) "'s faith is being tested...")
          #          (when-let [t (other :fail-quotes-table)]
          #            (flash-text-above o (get-table t faith-dmg)))
          (update o :faith - faith-dmg)
          (dmg-anim o faith-dmg :color faith-dmg-color))
        (let [insanity-dmg 1]
          (action-log (o :name) " is losing it...")
          (update o :insanity + insanity-dmg)
          (dmg-anim o insanity-dmg :color insanity-dmg-color)))

      (do # roll succeded!
        (when difficulty
          (action-log "Success!"))
        (:interact other o
                   :total total
                   :difficulty difficulty)))
    other))

(defn interact
  [o tile-i]
  (let [tile (in (dyn :world) tile-i)
        any-difficult (find |($ :difficulty) tile)
        die-i (o :selected-die)
        selected-die (get-in o [:dice die-i])]
    # if one must choose a die, use the first branch
    (if (and false # disabled branch
             any-difficult
             (not (o :selected-die)))
      (do
        (action-log (o :name) "'s faith is lacking.")
        (flash-die-bar any-difficult)
        nil)

      (if-not any-difficult
        (interact-with-tile o tile 0)

        (let [roll (roll-die 6)]
          (when selected-die
            (put o :selected-die nil)
            (put-in o [:render-dice die-i] @{:changed tick
                                             :die 0}))

          (die-roll-anim
            roll
            :extra
            selected-die

            :difficulty-label
            (cond
              (any-difficult :hp)
              "Armour rating: "

              (any-difficult :difficulty)
              "Difficulty: ")

            :target
            (any-difficult :difficulty)

            :after
            (fn []
              (let [total # with selected die
                    (if selected-die
                      (let [total (+ roll selected-die)]
                        (put-in o [:dice die-i] 0)
                        (put o :selected-die nil)
                        total)

                      (do # no die selected
                        roll))]

                (interact-with-tile o tile total)

                (set npc-turn npc-delay)))))))))

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
      (do
        (-> (dyn :world)
            (update i |(filter |(not= $ o) $))
            (update target-i array/push o))

        (when (= o player)
          (set npc-turn npc-delay))))))

(defn move-dir
  [o x y]
  (let [i (find-index |(index-of o $) (dyn :world))]
    (move-i o
            (+ (+ x (mod i ww))
               (* ww (+ y (math/floor (/ i ww))))))))

(defn move
  [o x y]
  (move-i o (+ x (* ww y))))

(defn chase-target
  [self]
  (var blocked nil)
  (def points (points-between-line
                (pos self)
                (pos player)))

  (loop [[x y] :in points
         :let [x (math/floor x)
               y (math/floor y)
               blocking (blocking? (xy->i x y))
               # _ (flash-text-on (first (get (dyn :world) (xy->i x y))) "*")
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
      (move-i self (xy->i ;(map math/floor (points 1))))
      (put self :last-known-pos (pos (self :target)))
      true)

    (self :last-known-pos)
    (when-let [p (get (points-between-line
                        (pos self)
                        (self :last-known-pos)) 1)]
      (def target (map math/floor p))
      (do (flash-text-above self "?")
        (move-i self (xy->i ;target))
        true))))

(defn find-target
  [self]

  (var blocked nil)

  (def points (points-between-line
                (pos self)
                (pos player)))

  (loop [[x y] :in points
         :let [x (math/floor x)
               y (math/floor y)
               blocking (blocking? (xy->i x y))
               # _ (flash-text-on (first (get (dyn :world) (xy->i x y))) "*")
]

         :when (and blocking
                    (not= self blocking)
                    (not= player blocking))]
    (set blocked [x y])
    #(break)
)

  (when blocked
    (put self :target nil))

  (cond (= (pos self) (self :last-known-pos))
    (do (put self :last-known-pos nil)
      (flash-text-above self ":("))

    (not blocked)
    (do (put self :target player)
      (flash-text-above self "!!")
      (put self :last-known-pos (pos (self :target)))
      true)))

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
                 (* (+ 0 (length empties))
                    (math/random)))]
    (if (< target 0)
      #(action-log (self :name) " is just standing there.")
      123
      (do
        # (action-log (self :name) " shambles about.")
        (move-i self (get empties (- target 0)))))))

(def zombie
  @{:name "Zombie"
    :hp 12
    :max-hp 12
    :damage 1
    :difficulty 3
    :blocking true
    :color 0x552255ff
    :color2 0x441144ff
    :interact fight
    :selected-dice 0
    :dice @[0]
    :act |(do
            (find-target $)
            (or (fight-neighbour $)
                (chase-target $)
                (move-randomly $))
            (find-target $))
    :render :circle})

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
  #(when (use-item picker :lockpick)
  (action-log "Success! " (picker :name) " unlocked the " (door :name) ".")
  (put door :blocking false)
  (put door :interact nil)
  (put-in door [:color 3] 0.0))
#)

(def locked-door
  @{:name "Locked Door"
    #    :hp 12
    #   :max-hp 12
    :blocking true
    :color @[0.5 0.5 0.5 1]
    :color2 @[0.1 0.1 0.1 1]
    :color3 @[0.3 0.3 0.3 1]
    :offset 10
    :difficulty 7
    :interact pick-lock
    :render :door-rec
    # thought about having quotes on fail
    # but it was sort of annoying
    #:fail-quotes-table
    #@[0 "I must be free of sin."
    #  3 "I won't be stopped so easily."
    #  6 "Fucking lock! *kicks door*"]
})

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

(def items
  {:lockpick
   {:description
    ``
    +1 Lockpicking
    ``}
   :short-sword {:description "+1 Melee damage"}
   :cloth-armor {:description "+1 Armor"}})

(def item-table
  {1 {1 :short-sword
      2 :rusty-gun
      3 [(roll-die 10) :gold]
      4 :cloth-helmet
      5 :cloth-armor
      6 :apple}
   2 {1 :long-sword
      2 :iron-gun
      3 [(+ 20 (roll-die 10)) :gold]
      4 :leather-helmet
      5 :leather-armor
      6 :holy-water}
   3 {1 :amethyst-sword
      2 :shotgun
      3 [(+ 100 (roll-die 30)) :gold]
      4 :iron-helmet
      5 :iron-armor
      6 :health-potion}})

(def nof-items-table
  [0 1
   3 2
   6 3])

(def quality-table
  [0 1
   3 2
   6 3])

(defn random-items
  []
  (let [nof (roll-table nof-items-table 6)
        quality (roll-table quality-table 6)]
    (seq [_ :range [0 nof]]
      (get-in item-table [quality
                          (roll-die 6)]))))

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
    :items (if (= :random (first items))
             (random-items)
             items)
    :render :rec2})


(def world-map
  (let [l @[ground locked-door]
        c (chest :lockpick)
        w (chest :artifact)
        r (chest :random)]

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
        X X X X . . z . . . . . . r X X >
        X X X X . . . . . . . . X X X X >
        X X X X w . . . . . . . X X X X >
        X X X X X X X X X . z X X X X X >
        X X X X X X X X X . . X X X X X >
        X X X X X X X X X . . X X X X X >
        X X X X X X X X X X X X X X X X >
        #
])

    world))

(set ww 0)
(var acc 0)


(defn new-world
  []
  {:world
   (seq [cell :in world-map
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
       @[(table/clone cell)]))
   :ww ww})

(defonce world-thing (new-world))
(def world-thing (new-world))

(set ww (world-thing :ww))
(set world-list (world-thing :world))

(var log-h 0)

(defn render-inventory
  [{:inventory inv}]
  (let [size 26
        size2 20
        c [1 1 1 0.9]
        hover-c [0.7 0.7 0 1]
        c2 [1 1 1 0.8]
        x (math/floor (+ 16 (* rw 0.7)))
        [mx my] mouse-pos]

    (var y (- ui-top 48 log-h))

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
        (def desc (get-in items [k :description]))
        (def [w h] (measure-text desc :size size))
        (draw-text desc
                   [(- x w 16) desc-y]
                   :size size
                   :color c)

        (when-let [desc (get-in items [k :flavour])]
          (def [w2 h2] (measure-text desc :size (- size 6)))
          (draw-text desc
                     [(- x w2 16) (+ h desc-y 2)]
                     :size (- size 6)
                     :color c2)))

      (draw-text (string
                   v
                   " "
                   (capitalize k))
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
    (set log-h h)
    (var y (- ui-top 48 h))

    (draw-rectangle
      0
      (- y 16)
      (* 32 rw)
      rh
      [0 0 0 1])

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

  (when (not (pos? hp))
    (draw-rectangle 0 0 (inc rw) (inc rh) :black)
    (draw-text "You died"
               (v/v+ mind-shake
                     [(/ rw 2) (/ rh 2)])
               :center true
               :size 64
               :color dmg-color)
    (break))

  (when (>= insanity 4)
    (when (< 4 (roll-die 6))
      (put mind-shake 0 (- (roll-die 6) 6)))
    (when (< 4 (roll-die 6))
      (put mind-shake 1 (- (roll-die 6) 6)))
    (draw-rectangle 0 0 (inc rw) (inc rh) :black)
    (draw-text "You lost your mind"
               (v/v+ mind-shake
                     [(/ rw 2) (/ rh 2)])
               :center true
               :size 64
               :color insanity-dmg-color)
    (let [scale 1
          w (math/floor (* 0.5 rw))
          h (math/floor (* 0.5 rh))]
      (loop [x :range [(- w) w 5]
             y :range [(- h) h 5]]
        (when (< (* (- (* 2.5 w) (math/abs x))
                    (- (* 2.5 h) (math/abs y))
                    #(* (+ 60 x) (+ 60 y))
)
                 (roll-die (math/pow (+ h w) 2.015)))
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

  (draw-bar "Health" hp max-hp [16 16] 2 0xaa3333ff)

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
              insanity-dmg-color))

  (render-action-log player)
  (render-inventory player)

  (when (number? npc-turn)
    (let [p (- 1 (/ npc-turn npc-delay))
          a (if (< p 0.5)
              (math/sin (* math/pi p))
              1)]
      (draw-rectangle 24
                      (- rh 148)
                      (math/floor (* (math/sin
                                       (* math/pi 0.5 p))
                                     (- rw 48)))
                      16
                      [0.95 0.95 0.95 a])))

  (let [dice (player :render-dice)
        padding 24
        nof (length dice)
        spacing 16
        total-w (- rw (* 2 padding)
                   (* spacing (max 5 (dec nof))))
        w (/ total-w (max nof 6))
        y (math/floor (- rh w (* 1.5 padding)))]

    (set ui-top y)

    (draw-rectangle (math/floor (* 0.5 padding))
                    y
                    (math/floor (- rw padding))
                    (math/floor (+ w padding))
                    (ui :die-bar-color))

    (loop [i :range [0 nof]
           :let [{:die die
                  :changed t} (in dice i)
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

                 timer (- tick t)

                 c
                 (if (or selected hit)
                   highlight-die-color
                   [0.81 0.85 0.6
                    (+ 0.0 (if (< timer 20)
                             (+ 0.2
                                (* 0.3
                                   (math/sin (* math/pi 0.5 (/ timer 20)))))
                             0.5))])]]

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

      (draw-die
        x
        y
        w
        h
        die
        c)))

  (set ui-press false))

(def mouse-presses
  {:press 1
   :double-click 1
   :triple-click 1})

(defn set-die
  [o n]
  (put o :selected-die
       (if (= (o :selected-die) n)
         nil
         n)))

(defn do-npc-turn
  [world]
  (print "doing npc turn")
  (def to-act @[])
  (loop [i :range [0 (length world)]
         :let [tile (in world i)
               x (mod i ww)
               y (math/floor (/ i ww))]
         o :in tile
         :when (and (not (o :dead))
                    (o :act))]
    (array/push to-act o))

  (loop [o :in to-act]
    (print (o :name) " acts.")
    (:act o)))

(defn on-event
  [ev]
  (with-dyns [:world world-list]
    (match ev
      [:mouse-move pos]
      (set mouse-pos (v/v- pos [rx ry]))

      [:mouse-drag pos]
      (set mouse-pos (v/v- pos [rx ry]))

      [(_ (mouse-presses (ev 0))) pos]
      (do
        (print "press")
        (if (on-ui?)
          (set ui-press true)
          (do
            (when npc-turn
              (do-npc-turn (dyn :world)))
            (move player ;(mouse-dir)))))

      [:key-down k]
      (unless
        (match k
          :1 (set-die player 0)
          :2 (set-die player 1)
          :3 (set-die player 2)
          :4 (set-die player 3)
          :5 (set-die player 4)
          :6 (set-die player 5))

        # (pp ev)
        (when
          (match k
            :w (do (when npc-turn
                     (do-npc-turn (dyn :world)))
                 (move-dir player 0 -1))
            :a (do (when npc-turn
                     (do-npc-turn (dyn :world)))
                 (move-dir player -1 0))
            :s (do (when npc-turn
                     (do-npc-turn (dyn :world)))
                 (move-dir player 0 1))
            :d (do (when npc-turn
                     (do-npc-turn (dyn :world)))
                 (move-dir player 1 0)))

          #(set npc-turn npc-delay)
)))))

(defn render
  [el]
  (with-dyns [:world world-list]
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
                             (* 0.35 ui-top)])
                      (v/v+ [(- (* 0.5 w))
                             (- (* 0.5 h))])))

      # then the camera offset
      (rl-translatef ;offset 0)

      (when (number? npc-turn)
        (-- npc-turn)

        (when (> 1 npc-turn)
          (set npc-turn true)))

      (let [world (dyn :world)]
        (when (true? npc-turn)
          (do-npc-turn world))

        (loop [i :range [0 (length world)]
               :let [tile (in world i)
                     x (mod i ww)
                     y (math/floor (/ i ww))]
               o :in tile
               :let [render-kw (in o :render)
                     render-f (in render-functions render-kw)]]
          (render-f o x y))))

    (when (true? npc-turn)
      (set npc-turn false))

    (render-player-ui player)
    (run-animations animations)

    (render-debug-ui)

    (++ tick)))

#(start-game {:render render
#             :on-event on-event})

(import freja/state)
(import freja/events :as e)

(defn main
  [& _]
  (assets/register-default-fonts)

  (init-window 600 800 "Rats")

  (frp/init-chans)

  (frp/subscribe-first! frp/mouse on-event)
  #(frp/subscribe-first! frp/keyboard pp)
  (e/put! state/focus :focus @{:on-event (fn [_ ev] (on-event ev))})

  (while (not (window-should-close))
    (begin-drawing)
    (clear-background :white)
    (frp/trigger (get-frame-time))

    (render {:width (get-screen-width)
             :height (get-screen-height)
             :render-x 0
             :render-y 0
             :focused? true})

    (end-drawing))

  (close-window))

#(main)

