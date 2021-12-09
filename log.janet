(import ./state :as s)

(defn action-log
  [& args]
  (put s/action-logs s/action-i @[s/action-delay (string ;args)])
  (++ s/action-i)
  (when (>= s/action-i (length s/action-logs))
    (set s/action-i 0)))
