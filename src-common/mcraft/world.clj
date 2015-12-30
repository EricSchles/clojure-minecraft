(ns mcraft.world)

(def WORLD_SIZE 10)

(defn add-grass [world]
  ;; makes the "floor"
  (let [n WORLD_SIZE
        y -2]
    (apply merge world
           (for [x (range (- 0 n) (inc n))
                 z (range (- 0 n) (inc n))]
             [x y z]))))

(defn new-world []
  ;; Makes the world of blocks
  (add-grass []))
