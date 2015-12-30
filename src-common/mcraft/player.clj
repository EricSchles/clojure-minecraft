(ns mcraft.player)

(defprotocol IPlayer
  (get-strafe [this])
  (set-strafe! [this val])
  (get-dy [this])
  (set-dy! [this val]))

(deftype Player [^:volatile-mutable lstrafe
                 ^:volatile-mutable ldy]
  IPlayer
  (get-strafe [this] (.lstrafe this))
  (set-strafe! [this val] (set! lstrafe val))
  (get-dy [this] (.ldy this))
  (set-dy! [this val] (set! ldy val)))
