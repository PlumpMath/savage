(ns savage.fast
  (:require-macros [reagent.ratom :refer [reaction]])
  (:require [reagent.core :as reagent :refer [cursor track track!]]
            [savage.brownian :as brownian]
            [reagent.ratom :refer [make-reaction]]))

(def renderer (reagent/adapt-react-class js/ReactTHREE.Renderer))
(def camera (reagent/adapt-react-class js/ReactTHREE.PerspectiveCamera))
(def scene (reagent/adapt-react-class js/ReactTHREE.Scene))
(def obj3d (reagent/adapt-react-class js/ReactTHREE.Object3D))
(def mesh (reagent/adapt-react-class js/ReactTHREE.Mesh))
(def line (reagent/adapt-react-class js/ReactTHREE.Line))

(def cube-rotation (atom 0))

(def box-geo (js/THREE.BoxGeometry. 1 1 1))
(def normal-material (js/THREE.MeshNormalMaterial.))

(defn v3 [x y z] (js/THREE.Vector3. x y z))
(defn euler [x y z] (js/THREE.Euler. x y z))

(defn spinning-cube [r x y z]
  [mesh {:geometry box-geo
         :material normal-material
         :rotation (euler 0 r r)
         :position (v3 x y z)}])

(defn zajal-cubes []
  [renderer {:width 500 :height 500}
   [scene {:camera "maincamera" :width 500 :height 500}
    [camera {:name "maincamera" 
             :position (v3 -10 0 0)
             :lookat (v3 1 0 0)}]
    (for [y (range -5 5)
          x (range -5 5)] 
      [spinning-cube @cube-rotation 10 (* 2 y) (* 2 x)])]])


(def line-material (js/THREE.LineBasicMaterial. #js {:color 0xff00}))
(def line-geo (js/THREE.Geometry.))
(dotimes [i 1000]
  (.. line-geo -vertices (push (v3 (* 10 (js/Math.sin (* 0.1 i)))
                                   (* 10 (js/Math.cos (* 0.1 i)))
                                   (* 0.1 i)))))


(defn line-geo* [g]
  (let [t @cube-rotation]
    (dotimes [i (count (.-vertices g))]
      (.. (aget (.-vertices g) i)
          (add (v3 (* 0.1 (- 0.5 (rand)))
                   (* 0.1 (- 0.5 (rand)))
                   0)))))
  (aset g "verticesNeedUpdate" true)
  g)

(defn zajal-lines []
  [renderer {:width 500 :height 500}
   [scene {:camera "maincamera" :width 500 :height 500}
    [camera {:name "maincamera" 
             :position (js/THREE.Vector3.
                         (* 50 (js/Math.sin (* 0.5 @cube-rotation)))
                         20
                         (* 50 (js/Math.cos (* 0.5 @cube-rotation))))
             :lookat (js/THREE.Vector3. 1 0 0)}]
    [line {:geometry (line-geo* line-geo)
           :material line-material}]]])


(defn rotate-cube [r] (+ r 0.01))

(defonce app-state
  (atom brownian/start))

(defn on-js-reload []
  #_
  (let [state (atom brownian/start)]
    (swap! state brownian/update)
    (reagent/render-component
      (brownian/draw @state)
      (. js/document (getElementById "app")))))

(defn render-loop [t]
  (.begin js/stats)
  (swap! app-state brownian/update)
  (reagent/render-component
    [brownian/draw @app-state]
    (. js/document (getElementById "app")))
  (.end js/stats)
  (.requestAnimationFrame js/window render-loop))

(defonce animation-frame
  (render-loop 0))

(on-js-reload)