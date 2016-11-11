(ns savage.css
  (:require-macros [reagent.ratom :refer [reaction]])
  (:require [reagent.core :as reagent :refer [cursor atom track track!]]
            [reagent.ratom :refer [make-reaction]]
            [clojure.set :as set]
            [clojure.string :as string]))

;; trig

(defn sqrt [x] (.sqrt js/Math x))
(defn cos [x] (.cos js/Math x))
(defn sin [x] (.sin js/Math x))

;; input

(def timestamp (atom 0))
(def delta (atom 0))
(def deltas (reaction (min (/ @delta 1000) 1)))

(def pressed-keys (atom #{}))
(def mouse (atom {:x 0 :y 0}))

(set! (.-onmousemove js/document) 
      (fn [e]
        (.preventDefault e)
        (reset! mouse {:x (.-x e) :y (.-y e)})))

(defn extract-keys [obj keys]
  (->> keys
       (mapcat (fn [k] [(keyword k)
                        (aget obj k)]))
       (apply hash-map)))

(let [event-keys ["altKey" "ctrlKey" "metaKey" "shiftKey" "keyIdentifier" "keyCode"]]
  (set! (.-onkeydown js/document) 
        (fn [e]
          (.preventDefault e)
          (if (and (>= (.-keyCode e) 32)
                   (not= (.-keyCode e) 91))
            
            (swap! pressed-keys conj
                   (extract-keys e event-keys)))))
  (set! (.-onkeyup js/document)
        (fn [e]
          (.preventDefault e)
          (swap! pressed-keys disj
                 (extract-keys e event-keys)))))

;; state
(defn update-state [s]
  (-> s
      ; (update :player #(-> %
      ;                      move-player
      ;                      rotate-player
      ;                      player-physics))
      ; (update :rocks update-rocks)
      ; detect-collisions
      ))

(defn container [{:keys [x y a]} body]
  [:div {:style #js {:position "fixed"
                     :top 0 :left 0
                     :transform
                     (str "translate(" x "px, " y "px) "
                          "rotate(" a "rad)") } }
   body])

(defn rock* [x y radius heading]
  {:x x :y y :radius radius :angle (rand) :heading heading
   :geometry (for [i (range (int (+ 5 (rand 5))))] (+ 1 (rand)))
   :id (gensym)})

(def state
  (atom {:player {:x 15 :y 15
                  :a 0
                  :speed 200
                  :radius 2}
         :rocks (for [x (range 10) y (range 10)]
                  (rock* (+ 10 (* 10 x)) (+ 10 (* 10 y)) (+ 1 (rand 1)) (rand)))}))


(defn all-texs [n]
  [:div
   (doall
     (for [i (range n)
           j (range n)]
       ^{:key (str i j)}
       [container {:x (* 25 i)
                   :y (+ 50 (+ (* 10 (sin (+ i (* 0.01 @timestamp)))) (* 25 j)))
                   :a (* 0.1 i (* 0.001 @timestamp))}
        [:p "Hello"]]))]
  )

(defn asteroids []
  [:div
   [:p (/ 1 @deltas)]
   (all-texs 20)
   ])

(enable-console-print!)

(defn on-js-reload []
  (println "Reloaded...")
  ; (reset! world/app-state (world/new-world))
  (reagent/render-component
    [asteroids]
    (. js/document (getElementById "app"))))

(on-js-reload)

(defn render-loop [t]
  (reset! delta (- t @timestamp))
  (reset! timestamp t)
  (swap! state update-state)
  (.requestAnimationFrame js/window render-loop))

(defonce animation-frame
  (.requestAnimationFrame
    js/window
    render-loop))