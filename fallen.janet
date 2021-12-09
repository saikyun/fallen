(use freja/flow)
(import freja/frp)
(import freja/assets)
(import ./state :as s)
(import ./dice :as dice)
(import ./animation :as anim :fresh true)
(import ./animations :as anims :fresh true)
(import ./color :fresh true)
(import ./light :fresh true)
(import ./tile)
(import ./ui :fresh true)
(import ./screen :fresh true)
(import ./log)
(import ./text)
(import ./items)
(import ./world)
(import ./actions)
(import ./objects)

(defn circle
  [o x y]
  (def {:color color
        :color2 color2
        :hp hp} o)

  (when (pos? hp)
    (let [r (* 0.5 (min (- s/w s/spacing) (- s/h s/spacing)))]
      (loop [[l v dx dy] :in (light/light-sources x y)]
        (draw-ellipse
          (math/floor (+ (* (+ x (* 0.1 dx)) s/w)
                         (* 0.5 (- s/w s/spacing))))
          (math/floor (+ (* (+ y (* 0.1 dy)) s/h)
                         (+ (* 0.5 (- s/h s/spacing)))))
          (math/floor (* (+ 1.1 (math/abs (* 0.1 dx))) r))
          (math/floor (* (+ 1.1 (math/abs (* 0.1 dy))) r))
          [0 0 0 v]))

      (draw-circle (math/floor (+ (* x s/w)
                                  (* 0.5 (- s/w s/spacing))))
                   (math/floor (+ (* y s/h)
                                  (* 0.5 (- s/h s/spacing))))
                   r
                   color2)

      (draw-rectangle
        (* x s/w)
        (math/floor (+ (* y s/h)))
        s/w

        (-
          (math/floor (+ (* y s/h)
                         (* 0.5 (- s/h s/spacing))))
          (math/floor (+ (* y s/h))))

        color2)

      (draw-circle (math/floor (+ (* x s/w)
                                  (* 0.5 (- s/w s/spacing))))
                   (math/floor (+ (* y s/h)))
                   r
                   color))))

(merge-into
  s/player
  @{:name "Saikyun"
    :dice (seq [_ :range [0 3]]
            (dice/roll 6))
    :render-dice @[]
    :hp 17
    :max-hp 17
    :faith 14
    :max-faith 22
    :insanity 0
    :max-insanity 70
    :damage 1
    :z 2
    :blocking true
    :difficulty 2
    :color 0x003333ff
    :color2 0x002222ff
    :render :circle
    :inventory @{}})

(if-not (empty? (s/player :render-dice))
  (loop [d :in (s/player :render-dice)]
    (put d :changed s/tick))
  (anim/anim
    (loop [i :range [0 (length (s/player :dice))]
           :let [n (in (s/player :dice) i)]]
      (loop [_ :range [0 (+ 5 (dice/roll 18))]]
        (yield nil))
      (yield (anims/die-roll n
                             :after |(put-in s/player [:render-dice i]
                                             @{:die n
                                               :changed s/tick})
                             :x (* -90
                                   (- (* 0.5 (length (s/player :dice)))
                                      i)))))))

(defn mouse-dir
  []
  (let [dir (->> (screen/->screen-pos s/player)
                 (v/v- s/mouse-pos)
                 v/normalize
                 # if an axis is big enough, it is set to +/-1
                 (map |(if (> (math/abs $) 0.3333)
                         (/ $ (math/abs $))
                         0)))
        mt (tile/mouse-tile)

        tile-dir (v/v- mt (s/player :pos))]

    # if the cursor is on an adjacent tile, or on the character
    (if (> 2 (v/mag tile-dir))
      (tuple ;(v/v+ tile-dir (s/player :pos)))
      (tuple ;(v/v+ dir (s/player :pos))))))

(def hover-delay 5)

(defn on-ui?
  []
  (>= (s/mouse-pos 1) s/ui-top))

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
      (* x s/w)
      (* y s/h)
      (- s/w s/spacing)
      (- s/h s/spacing)
      c)

    (when hc
      (draw-rectangle
        (* x s/w)
        (* y s/h)
        (- s/w s/spacing)
        (- s/h s/spacing)
        hc))

    (comment
      (draw-text
        (string x "/" y)
        [(* x w)
         (* y h)])
      #
      )))

(defmacro tile-transform
  [x y & body]
  ~(do
     (rl-push-matrix)

     (defer (rl-pop-matrix)
       (rl-translatef (* ,x s/w) (* ,y s/h) 0)
       ,;body
       )
     ))

(defn rec2
  [self x y]
  (let [{:color color
         :color2 color2
         :offset offset
         :hover-color hover-color} self]

    (tile-transform
      x y
      (draw-rectangle
        0
        0
        (- s/w s/spacing)
        (- s/h s/spacing)
        color)

      (draw-rectangle
        0
        offset
        (- s/w s/spacing)
        (- s/h s/spacing offset)
        color2)
      )

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

    (tile-transform
      x y
      (draw-rectangle
        0
        (math/floor  (* s/h -0.7))
        (- s/w s/spacing)
        (- s/h s/spacing)
        color2)

      (draw-rectangle
        0
        (math/floor (+ (* s/h 0.3)  (* s/h -0.7)))
        (- s/w s/spacing)
        (math/floor (- s/h s/spacing (* s/h 0.3)))
        color3)

      (draw-rectangle
        offset
        (math/floor (* s/h -0.2))
        (- s/w s/spacing (* 2 offset))
        (math/floor (+ (- s/h s/spacing) (* s/h 0.2)))
        color)

      )

    (comment
      (draw-text
        (string x "/" y)
        [(* x w)
         (* y h)])
      #
      )))


(defn draw-light
  [self x y]
  (def r (in self :radius))
  (def nr (- r))

  (comment draw-circle (math/floor (+ (* x w) (* 0.5 w)))
    (math/floor (+ (* y h) (* 0.5 h)))
    (math/floor (/ (min s/w h) 2))
    (self :color2))

  (comment draw-ellipse (math/floor (+ (* x w) (* 0.5 w)))
    (math/floor (+ (* y h) (* 0.5 h)))
    (math/floor (* (dec r) (/ s/w 2)))
    (math/floor (* (dec r) (/ h 2)))
    (self :color2))

  (comment
    (draw-ellipse (math/floor (+ (* x w) (* 0.5 w)))
                  (math/floor (+ (* y h) (* 0.5 h)))
                  (math/floor (* r (/ s/w 2)))
                  (math/floor (* r (/ h 2)))
                  (self :color2))

    (draw-ellipse (math/floor (+ (* x w) (* 0.5 w)))
                  (math/floor (+ (* y h) (* 0.5 h)))
                  (math/floor (* (inc r) (/ s/w 2)))
                  (math/floor (* (inc r) (/ h 2)))
                  (self :color2)))

  (loop [dx :range-to [nr r]
         dy :range-to [nr r]
         :let [rx (+ x dx)
               ry (+ y dy)
               rr (+ (math/pow (math/abs dx) 2)
                     (math/pow (math/abs dy) 2))]
         :when (and (< rr (math/pow r 2))
                    #(> rr (math/pow nr 2))
                    (>= rx 0)
                    (>= ry 0))]
    (tile-transform
      x y
      (draw-rectangle 0
                      0
                      s/w
                      s/h
                      (put (in self :color)
                           3
                           (+ 0.00 (* 0.2 (- 1 (/ rr (math/pow r 2))))))))))


(def render-functions
  {:rec rec
   :rec2 rec2
   :circle circle
   :door-rec door-rec
   :light draw-light})

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
  (def to-act @[])
  (loop [i :range [0 (length world)]
         :let [tile (in world i)
               x (mod i s/ww)
               y (math/floor (/ i s/ww))]
         o :in tile
         :when (and (not (o :dead))
                    (o :act))]
    (array/push to-act o))

  (loop [o :in to-act]
    (:act o)))

(defn on-event
  [ev]
  (with-dyns [:world s/world-list]
    (match ev
      [:mouse-move pos]
      (set s/mouse-pos (v/v- pos [s/rx s/ry]))

      [:mouse-drag pos]
      (set s/mouse-pos (v/v- pos [s/rx s/ry]))

      [(_ (mouse-presses (ev 0))) pos]
      (if (on-ui?)
        (set s/ui-press true)
        (do
          (when s/npc-turn
            (do-npc-turn (dyn :world)))
          (actions/try-move s/player (mouse-dir))))

      [:key-down k]
      (unless
        (match k
          :1 (set-die s/player 0)
          :2 (set-die s/player 1)
          :3 (set-die s/player 2)
          :4 (set-die s/player 3)
          :5 (set-die s/player 4)
          :6 (set-die s/player 5))

        # (pp ev)
        (when
          (match k
            :w (do (when s/npc-turn
                     (do-npc-turn (dyn :world)))
                 (actions/try-move-dir s/player [0 -1]))
            :a (do (when s/npc-turn
                     (do-npc-turn (dyn :world)))
                 (actions/try-move-dir s/player [-1 0]))
            :s (do (when s/npc-turn
                     (do-npc-turn (dyn :world)))
                 (actions/try-move-dir s/player [0 1]))
            :d (do (when s/npc-turn
                     (do-npc-turn (dyn :world)))
                 (actions/try-move-dir s/player [1 0]))))))))

(def layers
  @[@[]
    @[]])

(defn render
  [el]
  (with-dyns [:world s/world-list]
    (draw-rectangle 0 0 (el :width) (el :height) (in objects/inner-wall :color))
    (set s/rw (el :width))
    (set s/rh (el :height))
    (set s/rx (el :render-x))
    (set s/ry (el :render-y))

    (defer (rl-pop-matrix)
      (rl-push-matrix)

      (comment
        # center camera on screen center
        (rl-translatef 0)

        # center camera on cell center
        (rl-translatef (- (* 0.5 w))
                       (- (* 0.5 h))
                       0))

      (set s/offset (-> (s/player :pos)
                        (v/v* [s/w s/h])
                        (v/v* -1)
                        (v/v+ [(* 0.5 (el :width))
                               (* 0.35 s/ui-top)])
                        (v/v+ [(- (* 0.5 s/w))
                               (- (* 0.5 s/h))])))

      # then the camera offset
      (rl-translatef ;s/offset 0)

      (when (number? s/npc-turn)
        (-- s/npc-turn)

        (when (> 1 s/npc-turn)
          (set s/npc-turn true)))

      (let [world (dyn :world)]
        (when (true? s/npc-turn)
          (do-npc-turn world))

        (loop [l :in layers]
          (array/clear l))

        (loop [i :range [0 (length world)]
               :let [tile (in world i)
                     x (mod i s/ww)
                     y (math/floor (/ i s/ww))]
               o :in tile
               :let [render-kw (in o :render)
                     render-f (in render-functions render-kw)
                     z (in o :z)]
               # we check kw because we want the error if render-f is nil
               :when render-kw]
          (put o :render/x x)
          (put o :render/y y)
          (if (nil? z)
            (render-f o x y)
            (array/push (in layers (dec z)) o)))

        # second render
        (loop [l :in layers]
          (loop [o :in l
                 :let [render-kw (in o :render)
                       render-f (in render-functions render-kw)]
                 # we check kw because we want the error if render-f is nil
                 :when render-kw]
            (render-f o (in o :render/x) (in o :render/y))))))

    (when (true? s/npc-turn)
      (set s/npc-turn false))

    (ui/render-player-ui s/player)
    (anim/run-animations anim/animations)

    (ui/render-debug-ui)

    (++ s/tick)))

(when (dyn :freja/loading-file)
  (world/init-world)
  (start-game {:render render
               :on-event on-event}))

(import freja/state)
(import freja/events :as e)

(defn main
  [& _]
  (assets/register-default-fonts)

  (init-window 600 800 "Rats")

  (set-target-fps 60)

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

 