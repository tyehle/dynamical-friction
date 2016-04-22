(ns friction.core
  (:require [monet.core :refer [animation-frame]]
            [jayq.core :refer [show]]))

(def target (.getElementById js/document "target"))

(def width (.-width target))
(def height (.-height target))

(def size 50)


(defn next-position [state]
  (let [[x y xv yv] state]
    [(+ x xv) (+ y yv) xv yv]))


(defn render-forever [ctx state]
  (.save ctx)
  (.clearRect ctx 0 0 width height)
  (.fillRect ctx (first state) (second state) size size)
  (.restore ctx)
  (.log js/console "drawing")
  (render-forever ctx (next-position state)))


(defn go []
  (let [target (.getElementById js/document "target")
        context (.getContext target "2d")]
    (render-forever context [20 20 -1 -1])
    ;(.fillRect context 20 20 50 50)
    ))


(set! (.-onload js/window) go)

