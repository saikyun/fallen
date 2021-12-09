(import ./color :fresh :true)

(var npc-delay 10)
(var npc-turn false)

(var rx 0)
(var ry 0)
(var rw 0)
(var rh 0)

(var ui-press false)

(var mouse-pos @[0 0])

(var lights @[])

(var tick 0)
(var ui-top 0)
(var world-list nil)
(var spacing 0)
(var w 32)
(var h 48)

# world width, is set later
(var ww nil)

(var log-h 0)

(var action-logs (range 0 6))

(var ui @{:die-bar-color color/die-bar})

(array/fill action-logs @[0 nil])

(var action-i 0)
(var action-delay 700)

(var logs @[])
(var named-logs @{})


# camera offset
(var offset @[0 0])

(var player
  @{})


(var latest-above
  @{})

(var world-thing nil)
