(import ./dice)

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
      3 [(dice/roll 10) :gold]
      4 :cloth-helmet
      5 :cloth-armor
      6 :apple}
   2 {1 :long-sword
      2 :iron-gun
      3 [(+ 20 (dice/roll 10)) :gold]
      4 :leather-helmet
      5 :leather-armor
      6 :holy-water}
   3 {1 :amethyst-sword
      2 :shotgun
      3 [(+ 100 (dice/roll 30)) :gold]
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
  (let [nof (dice/roll-table nof-items-table 6)
        quality (dice/roll-table quality-table 6)]
    (seq [_ :range [0 nof]]
      (get-in item-table [quality
                          (dice/roll 6)]))))
