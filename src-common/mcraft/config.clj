(ns mcraft.config)

(def TICKS_PER_SEC 120)
;; Size of sectors used to ease block loading
(def SECTOR_SIZE 16)
(def WALKING_SPEED 5)
(def FLYING_SPEED 15)
(def GRAVITY 20.0)
(def MAX_JUMP_HEIGHT 1.0) ; About the height of a block
;; To derive the formula for calculating jump speed, first solve
;;    v_t = v_0 + a * t
;; for the time at which you achieve maximum height, where a is the acceleration
;; due to gravity and v_t = 0. This gives:
;;    t = - v_0 / a
;; Use t and the desired MAX_JUMP_HEIGHT to solve for v_0 (jump speed) in
;;    s = s_0 + v_0 * t + (a * t^2) / 2
(def JUMP_SPEED (Math/sqrt (* 2 GRAVITY MAX_JUMP_HEIGHT)))
(def TERMINAL_VELOCITY 50)
(def PLAYER_HEIGHT 2)

(defn tex_coord
  ;; Return the bounding vertices of the texture square.
  ([x y] (tex_coord x y 4))
  ([x y n] (let [m (/ 1 n)
                 dx (* x m)
                 dy (* y m)]
             [dx dy (+ dx m) dy (+ dx m) (+ dy m) dx (+ dy m)])))

(defn tex_coords [top bottom side]
  ;; Return a list of the texture squares for the top, bottom and side.
  (let [top (apply tex_coord top)
        bottom (apply tex_coord bottom)
        side (apply tex_coord side)]
    (flatten [top bottom (repeat 4 side)])))

(def TEXTURE_PATH "texture.png")

(def GRASS (tex_coords [1 0] [0 1] [0 0]))
(def SAND (tex_coords [1 1] [1 1] [1 1]))
(def BRICK (tex_coords [2 0] [2 0] [2 0]))
(def STONE (tex_coords [2 1] [2 1] [2 1]))

(def FACES [[0 1 0]
            [0 -1 0]
            [-1 0 0]
            [1 0 0]
            [0 0 1]
            [0 0 -1]])
