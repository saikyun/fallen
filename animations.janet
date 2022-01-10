(use freja/flow)
(import ./animation :as anim)
(import ./dice :as dice :fresh true)
(import ./state :as s)
(import ./color :as color)
(import ./screen)
(import ./tile)

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
          (draw-circle r-x mid-y r die-col))
      (draw-text die [mid-x mid-y] :center true)
      )))


(defn die-roll
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
    #(when target
    #  (draw-text (string diff-label #target
    #                     )
    #             [(/ s/rw 2) (* s/rh 0.4)]
    #             :color :white
    #             :size 32
    #             :center true))
    )

  (default x 0)
  (anim/anim
    (def dur-scale 0)

    (def dur (math/floor (* dur-scale 40))
      )
    (var res nil)
    (var delay 0)

    
    (loop [i :range [0 dur]
           :let [p (/ i dur)
                 px (math/sin (* math/pi 0.5 p))
                 py (- 1 (anim/ease-out-bounce p))]]
      (when (<= delay 0)
        (set res (dice/roll 6))
        (set delay (* 10 p))
        (when (>= (+ i delay) dur)
          (set res final-res)))
      (-- delay)
      (draw-target)

      (draw-extra (+ x (* 0.5 s/rw))
                  (* s/rh 0.5)
                  color/highlight-die)
      (yield (draw-die
               (- (math/floor (+ x (- s/rw (* 0.5 s/rw px)))) 30)
               (math/floor (- (* s/rh 0.5) (* s/rh 0.5 py)))
               60
               60
               res
               color/highlight-die)))

    
    (set res final-res)

    (def dur2 (math/floor (* dur-scale 5)))
    (loop [i :range [0 dur2]
           :let [scale (* 0.00206
                          (math/pow 2
                                    (* 10 (math/sin (* math/pi
                                                       0.5
                                                       (/ i dur2))))))
                 scale (* 0.2 scale)
                 col (cond
                       (and target (>= (+ (or extra 0) final-res) target))
                       color/success-die

                       target
                       color/fail-die

                       color/highlight-die)]]
      (draw-target)
      (rl-push-matrix)
      (rl-translatef
        (math/floor (- (+ x (- (* 0.5 s/rw) (* 0.5 60 scale))) 100 30))
        (math/floor (- (* s/rh 0.5) (* 1 60 scale)))
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
        (math/floor (+ -30 x (- (* 0.5 s/rw) (* 0.5 60 scale))))
        (math/floor (- (* s/rh 0.5) (* 1 60 scale)))
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
    (loop [i :range [0 dur3]
           :let [p (/ i dur3)
                 scale 0.4
                 col (cond
                       (and target (>= (+ (or extra 0) final-res) target))
                       color/success-die

                       target
                       color/fail-die

                       color/highlight-die)]]
      (draw-target)
      (rl-push-matrix)
      (rl-translatef
        (math/floor (- (+ -30 x (- (* 0.5 s/rw) (* 0.5 60 scale))) 100))
        (math/floor (- (* s/rh 0.5) (* 1 60 scale)))
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
        (math/floor (+ x -30 (- (* 0.5 s/rw) (* 0.5 60 scale))))
        (math/floor (- (* s/rh 0.5) (* 1 60 scale)))
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

    (def dur4 (math/floor (* #dur-scale
                             1
                             20)))
    (loop [i :range [0 dur4]
           :let [p (/ i dur4)
                 a (- 1 (anim/ease-in-expo p))
                 scale (- 0.4 (* 0.5 (anim/ease-in-expo p)))
                 col [;(cond
                         (and target (>= (+ (or extra 0) final-res) target))
                         color/success-die

                         target
                         color/fail-die

                         color/highlight-die)
                      a]]]
      (draw-target)
      (rl-push-matrix)
      (rl-translatef
        (math/floor (- (+ -30 x (- (* 0.5 s/rw) (* 0.5 60 scale))) 100))
        (math/floor (- (* s/rh 0.5) (* 1 60 scale)))
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
        (math/floor (+ x -30 (- (* 0.5 s/rw) (* 0.5 60 scale))))
        (math/floor (- (* s/rh 0.5) (* 1 60 scale)))
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

    (print "done")
    (after)))


(defn nice-flash!!!
  []
  (anim/anim
    (def dur 20)
    (def col @[0 0 0 1])
    (loop [i :range-to [0 dur]
           :let [p (anim/ease-out-quad (/ i dur))
                 p (+ 0.2 (* 0.2 p))]]
      (put col 0 p)
      (put col 1 p)
      (put col 2 p)
      (yield (put s/ui :die-bar-color col)))
    (loop [i :range-to [0 dur]
           :let [p (/ i dur)
                 p (+ 0.2 (* 0.8 (- 1 p)))]]
      (put col 0 p)
      (put col 1 p)
      (put col 2 p)
      (yield (put s/ui :die-bar-color col)))))


(defn flash-die-bar
  [o]
  (anim/anim
    (with-dyns [:world s/world-list]
      (def dur 20)
      (def col @[0 0 0 1])

      (loop [i :range-to [0 dur]
             :let [p (anim/ease-out-quad (/ i dur))
                   p (+ 0.2 (* 0.8 p))]]
        (put col 0 p)
        (put col 1 p)
        (put col 2 p)
        (yield (draw-text "Select a die"
                          (let [pos (screen/->screen-pos o)]
                            (update pos 1 - s/h))
                          :color col
                          :center true)))
      (loop [i :range-to [0 200]]
        (yield (draw-text "Select a die"
                          (let [pos (screen/->screen-pos o)]
                            (update pos 1 - s/h))
                          :color col
                          :center true)))
      (loop [i :range-to [0 dur]
             :let [p (/ i dur)
                   p (+ 0.2 (* 0.8 (- 1 p)))]]
        (put col 0 p)
        (put col 1 p)
        (put col 2 p)
        (yield (draw-text "Select a die"
                          (let [pos (screen/->screen-pos o)]
                            (update pos 1 - s/h))
                          :color col
                          :center true)))))

  (anim/anim
    (loop [i :range-to [0 40]]
      (yield))
    (def dur 20)
    (def col @[0 0 0 1])
    (loop [i :range-to [0 dur]
           :let [p (anim/ease-out-quad (/ i dur))
                 p (+ 0.2 (* 0.8 p))]]
      (put col 0 p)
      (put col 1 p)
      (put col 2 p)
      (yield (put s/ui :die-bar-color col)))
    (loop [i :range-to [0 dur]
           :let [p (/ i dur)
                 p (+ 0.2 (* 0.8 (- 1 p)))]]
      (put col 0 p)
      (put col 1 p)
      (put col 2 p)
      (yield (put s/ui :die-bar-color col)))))


(defn dmg
  [o dmg-amount &keys {:color color}]
  (anim/anim
    (with-dyns [:world s/world-list]
      (def col @[;color 0])
      (def dur 60)

      (loop [i :range-to [0 dur]
             :let [p2 (anim/ease-out (/ (* 1 i) dur))
                   p (math/sin (* math/pi (/ i dur)))]]
        # fades in / out alpha
        (put col 3 p)
        (yield (draw-text (string dmg-amount)
                          (let [pos (screen/->screen-pos o)]
                            (update pos 1 - (* (+ 0.6 (* 0.3 p2)) s/h)))
                          :color col
                          :size 32
                          :center true))))))


(comment

  (dmg s/player 10)
  #
  )



(defn flash-text-above
  [o text]
  (def text (buffer text))
  (put s/latest-above o text)
  (anim/anim
    (with-dyns [:world s/world-list]
      (def dur 20)
      (def col @[0 0 0 1])

      (loop [i :range-to [0 dur]
             :let [p (anim/ease-out-quad (/ i dur))
                   p (+ 0.2 (* 0.8 p))]
             :when (= (s/latest-above o) text)]
        (put col 0 p)
        (put col 1 p)
        (put col 2 p)
        (yield (draw-text text
                          (let [pos (screen/->screen-pos o)]
                            (update pos 1 - (* 2 s/h)))
                          :color col
                          :center true)))
      (loop [i :range-to [0 200]
             :when (= (s/latest-above o) text)]
        (yield (draw-text text
                          (let [pos (screen/->screen-pos o)]
                            (update pos 1 - (* 2 s/h)))
                          :color col
                          :center true)))
      (loop [i :range-to [0 dur]
             :let [p (/ i dur)
                   p (+ 0.2 (* 0.8 (- 1 p)))]
             :when (= (s/latest-above o) text)]
        (put col 0 p)
        (put col 1 p)
        (put col 2 p)
        (yield (draw-text text
                          (let [pos (screen/->screen-pos o)]
                            (update pos 1 - (* 2 s/h)))
                          :color col
                          :center true))))))

(defn flash-text-on
  [o text]
  (def text (buffer text))
  (put s/latest-above o text)
  (anim/anim
    (with-dyns [:world s/world-list]
      (def dur 20)
      (def col @[1 1 1 1])

      (loop [i :range-to [0 dur]
             :when (= (s/latest-above o) text)]
        (yield (draw-text text
                          (let [pos (screen/->screen-pos o)]
                            (update pos 1 - (* 0 s/h)))
                          :color col
                          :center true))))))
 