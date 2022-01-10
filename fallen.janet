huöaälcrlhrlc(use freja/flow)
(import freja/frp)
(import freja/assets)
(import ./state :as s)
(import ./dice :as d)
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
(import ./world :fresh true)
(import ./actions)
(import ./objects)
(import ./render :fresh true)
(import ./lul)

(import spork/path)

(pp (module/cache (first (module/find "./render"))))


(printf "%M" (filter |(string/find
                        "render" $)
                     (keys module/cache)))

(merge-into
  s/player
  @{:name "Saikyun"
    :dice (seq [_ :range [0 3]]
            (d/roll 6))
    :render-dice @[]
    :hp 8
    :max-hp 8
    :ammo 3
    :max-ammo 3
    :faith 14
    :max-faith 22
    :insanity 0
    :max-insanity 70
    :max-range 2
    :damage (fn [self target]
              (d/roll-sum 2 3))
    :z 2
    :blocking true
    :difficulty 2
    :color 0x003333ff
    :color2 0x002222ff
    :render :circle
    :weapon @{:attack objects/melee}
    :update |(print "lul")
    :inventory @{}})

(if-not (empty? (s/player :render-dice))
  (loop [d :in (s/player :render-dice)]
    (put d :changed s/tick))
  (anim/anim
    (loop [i :range [0 (length (s/player :dice))]
           :let [n (in (s/player :dice) i)]]
      (loop [_ :range [0 (+ 5 (d/roll 18))]]
        (yield nil))
      (yield (anims/die-roll n
                             :after |(put-in s/player [:render-dice i]
                                             @{:die n
                                               :changed s/tick})
                             :x (* -90
                                   (- (* 0.5 (length (s/player :dice)))
                                      i)))))))

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

(defn update-player-target
  []
  (let [mt (tile/mouse-tile)
        t (tile/->tile mt)
        d (find |(and ($ :difficulty)
                      ($ :hp)
                      )
                t)]
    (put s/player :aiming nil)
    (put s/player :targeted-tile nil)
    (put s/player :looking-tile nil)
    
    (cond
      (= mt (s/player :pos))
      (put s/player :looking-tile (s/player :pos))

      d
      (do
        (put s/player :targeted-tile mt)
        (put s/player :aiming true)
        )
      (put s/player :looking-tile (screen/mouse-dir))
      )))

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
    (:act o))

  (update-player-target))

(defn on-event
  [ev]
  (with-dyns [:world s/world-list]
    (match ev
      [:mouse-move pos]
      (do
        (set s/mouse-pos (v/v- pos [s/rx s/ry]))
        (update-player-target)
        )

      [:mouse-drag pos]
      (do
        (set s/mouse-pos (v/v- pos [s/rx s/ry]))
        (update-player-target)
        )

      [(_ (mouse-presses (ev 0))) pos]
      (if (screen/on-ui?)
        (set s/ui-press true)
        (do
          (when s/npc-turn
            (do-npc-turn (dyn :world)))
          (actions/try-move s/player
                            (or (s/player :targeted-tile)
                                (s/player :looking-tile)))))

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
                     render-f (in render/render-functions render-kw)
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
                       render-f (in render/render-functions render-kw)]
                 # we check kw because we want the error if render-f is nil
                 :when render-kw]
            (render-f o (in o :render/x) (in o :render/y))))))

    (when (true? s/npc-turn)
      (set s/npc-turn false))

    (ui/render-player-ui s/player)
    (anim/run-animations anim/animations)

    (ui/render-debug-ui)

    #(draw-text lul/hej [50 50] :color :yellow)

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

 