(ns mcraft.core
  (:require [play-clj.core :refer :all]
            [play-clj.g3d :refer :all]
            [play-clj.math :refer :all]
            [play-clj.ui :refer :all]
            [mcraft.config :as c]
            [mcraft.player :as p]
            [mcraft.world :as w])
  (:import [mcraft.player Player]))

(def mouseX (atom 0))
(def mouseY (atom 0))

(def player (Player. [0 0] 0))

(defn coords-of
  "Get the [x y z] coordinates of an entity"
  [entity]
  [(:x entity) (:y entity) (:z entity)])

(defn normalize
  "Accepts `position` of arbitrary precision and returns the block
  containing that position."
  [position]
  (let [[x y z] position]
    (map #(Math/round %) [x y z])))

(defn seq-contains? [coll target]
  (some #(= target %) coll))

(defn collide
  "Checks to see if the player at the given `position` and `height`
  is colliding with any blocks in the world."
  [world position height]
  (let [pad 0.2
        p (atom position)
        np (vec (normalize position))]
    [(doseq [face c/FACES
             i (range 3)
             :let [d (* (- (nth @p i) (nth np i))
                        (nth face i))]
             :when (and (not= 0 (nth face i))
                        (>= d pad))]
       (doseq [dy (range height)
               :let [op (assoc np 1 (- (nth np 1) dy))
                     op (assoc op i (+ (nth op i) (nth face i)))]
               :when (seq-contains? world op)]
         (if (or (= face [0 -1 0]) (= face [0 1 0]))
           (p/set-dy! player 0))
         (swap! p (fn [x] (assoc x i (- (nth x i)
                                       (* (- d pad)
                                          (nth face i))))))))]
    @p))

(defn get-environment []
  (let [attr-type (attribute-type :color :ambient-light)
        attr (attribute :color attr-type 0.8 0.8 0.8 1)]
    (environment :set attr)))

(defn get-camera []
  (doto (perspective 90 (game :width) (game :height))
    (near! 0.1)
    (far! 300)))

(defn get-material []
  (let [c (color (+ 0.5 (* 0.5 (rand)))
                 (+ 0.5 (* 0.5 (rand)))
                 (+ 0.5 (* 0.5 (rand)))
                 1)]
    (material :set (attribute! :color :create-specular 1 1 1 1)
              :set (attribute! :float :create-shininess 8)
              :set (attribute! :color :create-diffuse c))))

(defn get-attrs []
  (bit-or (usage :position) (usage :normal)))

(defn create-block!
  "Makes the thingy that lets us render blocks."
  [position]
  (let [[x y z] position]
    (-> (model-builder)
        (model-builder! :create-box 1 1 1 (get-material) (get-attrs))
        model
        (assoc :x x :y y :z z))))

(defn add-block!
  "Adds a new block to the entities collection."
  [entities position]
  (conj entities (create-block! position)))

(defn get-rotation
  "Takes screen and returns the rotation of the player."
  ;; TODO: this might be buggy
  [screen]
  (defn tang [a v vv]
    (/ (* 180
          (a (vector-3! v
                        :dot
                        (vector-3! vv :nor))))
       Math/PI))
  (let [[a b c
         d e f & m] (seq (-> screen
                             (perspective! :view)
                             (matrix-4! :cpy)
                             (matrix-4! :inv)
                             (matrix-4! :get-values)))
         vx (vector-3 a b c)
         vy (vector-3 d e f)
         q (tang #(Math/acos %) (vector-3 0 0 1) vx)
         rx (tang #(Math/acos %) (vector-3 1 0 0) vx)
         rx (if (> q 90)
              (- 360 rx)
              rx)
         ry (tang #(Math/asin %) (vector-3 0 1 0) vy)
         ry (if (> q 90)
              (- 0 ry)
              ry)]
    [rx ry]))

(defn at-position?
  "Checks if the entity occupying the position."
  [position entity]
  (let [[x y z] position]
    (and (= x (:x entity))
         (= y (:y entity))
         (= z (:z entity)))))

(defn block-exists?
  "Check if a block exists at the position."
  [entities position]
  (some #(at-position? position %) entities))

(defn get-motion-vector
  "something, something, how the player moves"
  [screen]
  (if (every? zero? (p/get-strafe player))
    [0 0 0]
    (let [[x y] (get-rotation screen)
          ;; apply doesn't like Math/atan2 for some reason
          strafe (Math/toDegrees (apply #(Math/atan2 %1 %2)
                                        (p/get-strafe player)))
          y_angle (Math/toRadians y)
          x_angle (Math/toRadians (+ x strafe))
          dy 0
          dx (Math/cos x_angle)
          dz (Math/sin x_angle)]
      [dx dy dz])))

(defn get-sight-vector
  "Returns the direction the player is looking"
  [screen]
  (let [direction (perspective! screen :direction)]
    [(x direction) (y direction) (z direction)]))

(defn hit-test
  "Check what block/block-space the player is looking at."
  [entities position vector]
  (let [max-distance 8
        m 8
        [dx dy dz] vector]
    (loop [i 0
           p position
           previous nil]
      (if (> i (* max-distance m))
        [nil nil]
        (let [[x y z] p
              key (normalize p)]
          (if (and (not= key previous)
                   (block-exists? entities key))
            [key previous]
            (recur (inc i)
                   [(+ x (/ dx m)) (+ y (/ dy m)) (+ z (/ dz m))]
                   key)))))))

(defn adjust-strafe! [player idx inc]
  (p/set-strafe! player (assoc (p/get-strafe player) idx
                               (+ (nth (p/get-strafe player) idx) inc))))

(defn remove-block!
  "Remove the block at position from the entities collection."
  [entities position]
  ;; probably idempotent
  (filter #(not (at-position? position %)) entities))

(defn get-position [screen]
  [(x screen) (y screen) (z screen)])

(defn find-block [entities block-position]
  (first (filter #(at-position? block-position %) entities)))

(defn focus-block! [entities block-position]
  (let [block (find-block entities block-position)
        replacement (create-block! block-position)]
    (replace {block replacement} entities)))

(defn draw-focused-block! [screen entities]
  (let [block-position (first (hit-test entities
                                        (get-position screen)
                                        (get-sight-vector screen)))]
    ;; (if block-position
    ;;   (focus-block! entities block-position)
    ;;   entities)
    entities))

(defscreen main-screen
  ;; the main game-logic loop thingy
  :on-show
  (fn [screen entities]
    (add-timer! screen :update 0 (/ 1.0 c/TICKS_PER_SEC))
    (let [screen (update! screen
                          :renderer (model-batch)
                          :attributes (get-environment)
                          :camera (get-camera))]
      [(for [pos (w/new-world)]
         (create-block! pos))]))

  :on-render
  (fn [screen entities]
    (let [entities (draw-focused-block! screen entities)]
      (clear! 0.5 0.69 1.0 1)
      (render! screen entities))
    entities)

  :on-timer
  (fn [screen entities]
    (let [dt (/ 1.0 c/TICKS_PER_SEC)
          ;; TODO: Implement flying?
          speed c/WALKING_SPEED
          d (* dt speed)
          [dx dy dz] (get-motion-vector screen)
          [dx dy dz] [(* dx d)
                      (+ (* dy d)
                         (* dt (p/set-dy! player
                                          (max (- (p/get-dy player)
                                                  (* dt c/GRAVITY))
                                               (- 0 c/TERMINAL_VELOCITY)))))
                      (* dz d)]
          [x1 y1 z1] [(x (position screen))
                      (y (position screen))
                      (z (position screen))]
          [x2 y2 z2] (collide (for [x entities]
                                (coords-of x))
                              [(+ x1 dx) (+ y1 dy) (+ z1 dz)]
                              c/PLAYER_HEIGHT)
          [x y z] [(- x2 x1)
                   (- y2 y1)
                   (- z2 z1)]]
      (doto screen
        (perspective! :translate x y z)
        (perspective! :update)))
    entities)

  :on-resize
  (fn [{:keys [width height] :as screen} entities]
    (size! screen width height))

  :on-touch-down
  (fn [screen entities]
    ;; user can add or remove a block
    (let [[block previous] (hit-test entities
                                     (get-position screen)
                                     (get-sight-vector screen))]
      (let [b (:button screen)]
        (cond (and (= b (button-code :left)) block)
              (remove-block! entities block)
              (and (= b (button-code :right)) previous)
              (add-block! entities previous)
              :else
              entities))))

  :on-mouse-moved
  (fn [screen entities]
    (let [m 0.5
          screenX (:input-x screen)
          screenY (:input-y screen)
          magX (* m (- @mouseX screenX))
          magY (* m (- @mouseY screenY))]
      (reset! mouseX screenX)
      (reset! mouseY screenY)
      (perspective! screen :rotate (vector-3 0 1 0) magX)
      (perspective! screen :update)
      (perspective! screen
                    :rotate
                    (vector-3! (vector-3! (direction screen) :cpy)
                               :crs (perspective! screen :up))
                    magY)
      (perspective! screen :update)
      (if (> 0 (y (perspective! screen :up)))
        (perspective! screen
                      :look-at
                      (x (perspective! screen :position))
                      (+ magY (y (perspective! screen :position)))
                      (z (perspective! screen :position))))
      (perspective! screen :update))
    entities)

  :on-key-down
  (fn [screen entities]
    (cond
      (= (:key screen) (key-code :w))
      (doseq [x [1]]
        (adjust-strafe! player 0 -1))
      (= (:key screen) (key-code :s))
      (doseq [x [1]]
        (adjust-strafe! player 0 1))
      (= (:key screen) (key-code :a))
      (doseq [x [1]]
        (adjust-strafe! player 1 -1))
      (= (:key screen) (key-code :d))
      (doseq [x [1]]
        (adjust-strafe! player 1 1))
      (= (:key screen) (key-code :space))
      (if (zero? (p/get-dy player))
        ;; TODO: Return nil in a better way.
        (doseq [x [1]]
          (p/set-dy! player c/JUMP_SPEED)))
      (= (:key screen) (key-code :escape))
      (input! :set-cursor-catched (not (input! :is-cursor-catched)))))

  :on-key-up
  (fn [screen entities]
    (cond
      (= (:key screen) (key-code :w))
      (doseq [x [1]]
        (adjust-strafe! player 0 1))
      (= (:key screen) (key-code :s))
      (doseq [x [1]]
        (adjust-strafe! player 0 -1))
      (= (:key screen) (key-code :a))
      (doseq [x [1]]
        (adjust-strafe! player 1 1))
      (= (:key screen) (key-code :d))
      (doseq [x [1]]
        (adjust-strafe! player 1 -1)))))

(defn adjust-crosshair! [crosshair screen]
  (actor! crosshair
          :set-position
          (- (/ (width screen) 2) 4)
          (- (/ (height screen) 2) 8)))

(defscreen text-screen
  :on-show
  (fn [screen entities]
    (update! screen :camera (orthographic) :renderer (stage))
    ;; (assoc (label "0" (color :white))
    ;;        :id :fps
    ;;        :x 5)
    (assoc (label "+" (color :white))
           :id :crosshair
           :set-alignment (align :bottom-left)))

  :on-render
  (fn [screen entities]
    (render!
     screen
     (for [entity entities]
       (case (:id entity)
         :fps
         (doto entity (label! :set-text (str (game :fps))))
         :crosshair
         (doto entity (adjust-crosshair! screen))
         entity))))

  :on-resize
  (fn [screen entities]
    (height! screen 300)))

(defgame mcraft
  :on-create
  (fn [this]
    (set-screen! this main-screen text-screen)))
