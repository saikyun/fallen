(use freja/flow)
(import ./state :as s)
(import ./light)
(import ./screen)

(defn rec
  [self x y]
  (let [{:color color
         :hover-color hover-color
         :targeting-color targeting-color} self
        targeting (= (s/player :targeted-tile) [x y])
        hc (when (and hover-color
                      (not (screen/on-ui?)))
             (var deccing true)

             (update self :hover-time
                     (if (or targeting
                             (= (s/player :looking-tile) [x y])
                             (and (s/player :aiming)
                                  (>= (s/player :max-range)
                                      (v/dist (self :pos)
                                              (s/player :pos)))))
                       inc
                       (do
                         (set deccing true)
                         dec)))

             (update self :hover-time
                     |(min s/hover-delay (max 0 $)))

             (let [p (/ (self :hover-time)
                        s/hover-delay)]
               [;(if targeting
                   targeting-color
                   hover-color)
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
  #(print "wat")
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
      rx ry
      (draw-rectangle 0
                      0
                      s/w
                      s/h
                      (put (in self :color)
                           3
                           (+ 0.00 (* 0.2 (- 1 (/ rr (math/pow r 2)))))
                           )))))



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

(def render-functions
  {:rec rec
   :rec2 rec2
   :circle circle
   :door-rec door-rec
   :light draw-light})
 