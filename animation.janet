(var animations @[])

(defn run-animations
  [anims]
  (var i 0)
  (while (< i (length anims))
    (let [a (in anims i)]
      (if-not (fiber/can-resume? a)
        (array/remove anims i)
        (do
          (try (resume a)
            ([err fib]
              (debug/stacktrace fib err)))
          (++ i))))))

(defmacro anim
  [& body]
  ~(array/push ',animations
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


(defn ease-in-expo
  [p]
  (if (zero? p)
    0
    (math/pow 2 (- (* 10 p) 10))))


(defn ease-out
  [p]
  (math/sin (* p math/pi 0.5)))

(defn ease-out-quad
  [p]
  (- 1 (math/pow (- 1 p) 2)))
