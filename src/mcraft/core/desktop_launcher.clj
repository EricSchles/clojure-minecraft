(ns mcraft.core.desktop-launcher
  (:require [mcraft.core :refer :all])
  (:import [com.badlogic.gdx.backends.lwjgl
            LwjglApplication
            LwjglApplicationConfiguration]
           [org.lwjgl.input Keyboard])
  (:gen-class))

(defn -main
  []
  (let [config (LwjglApplicationConfiguration.)]
    (set! (.vSyncEnabled config) true)
    (set! (.title config) "Lambdaminer")
    (set! (.width config) 800)
    (set! (.height config) 600)
    ;; (set! (. config foregroundFPS) 0)
    ;; (set! (. config backgroundFPS) 0)
    (LwjglApplication. mcraft config)
    (Keyboard/enableRepeatEvents false)))
