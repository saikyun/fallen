(import ./tile)
(import ./state :as s)
(import ./actions :fresh true)
(import ./items)
# should log and text be here?
(import ./log)
(import ./text)


(def zombie
  @{:name "Zombie"
    :hp 12
    :max-hp 12
    :damage 1
    :difficulty 3
    :blocking true
    :color 0x552255ff
    :color2 0x441144ff
    :interact actions/fight
    :selected-dice 0
    :dice @[0]
    :z 2
    :act |(do
            (actions/find-target $)
            (or (actions/fight-neighbour $)
                (actions/chase-target $)
                (actions/move-randomly $))
            (actions/find-target $))
    :render :circle})

(defn zero->nil
  [v]
  (if (zero? v)
    nil
    v))

(defn use-item
  [user item]
  (if-not (get-in user [:inventory item])
    (log/action-log (user :name) " does not have a " item ".")
    (do
      (log/action-log (user :name) " used a " item ".")
      (update-in user [:inventory item] |(-> $ dec zero->nil)))))

(defn pick-lock
  [door picker &keys {:difficulty difficulty
                      :total total}]
  #(when (use-item picker :lockpick)
  (log/action-log "Success! " (picker :name) " unlocked the " (door :name) ".")
  (put door :blocking false)
  (put door :interact nil)
  (put-in door [:color 3] 0.0))
#)

(def locked-door
  @{:name "Locked Door"
    #    :hp 12
    #   :max-hp 12
    :z 2
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
      (log/action-log (taker :name) " found "
                      (string/join
                        (map text/capitalize is)
                        ", ")
                      " inside " (container :name) ".")
      (put container :difficulty nil)
      (loop [i :in is]
        (update-in taker [:inventory i] nil-safe-inc)))
    (log/action-log (container :name) " is empty.")))


(defn chest
  [& items]
  @{:name "Chest"
    #:hp 12
    #:max-hp 12
    :blocking true
    :color @[0.8 0.8 0 1]
    :color2 @[0.6 0.6 0 1]
    :offset 30
    :z 2
    :interact take-items
    :items (if (= :random (first items))
             (items/random-items)
             items)
    :render :rec2})

(defn light-source
  [radius]
  @{:name "Light"
    :color @[1 0.8 0.6 0.1]
    :color2 @[0.7 0.6 0.2 0.02]
    :radius radius
    :light true
    :z 1
    :render :light})

(def inner-wall
  @{:blocking true
    :color [0.02 0.02 0.03]
    :z 2
    :render :rec})

(def inner-wall-down
  @{:blocking true
    :color (inner-wall :color)
    :color2 [0.1 0.1 0.1]
    :offset 15
    :z 2
    :render :rec2})


(def ground
  @{:blocking false
    :color [0.06 0.05 0.05]
    :hover-color [0.2 0.2 0.2]
    :hover-time 0
    :render :rec})