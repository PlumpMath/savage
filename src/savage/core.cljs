(ns savage.core
  (:require-macros [reagent.ratom :refer [reaction]])
  (:require [reagent.core :as reagent :refer [cursor atom track track!]]
            [reagent.ratom :refer [make-reaction]]
            [clojure.set :as set]
            [clojure.string :as string]))

(enable-console-print!)

(defn sqrt [x] (.sqrt js/Math x))
(defn cos [x] (.cos js/Math x))
(defn sin [x] (.sin js/Math x))

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

(let [event-keys ["altKey" "ctrlKey" "metaKey" "shiftKey" "key" "keyCode"]]
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
    
  ;; time

  ;; keys

(defn point [p]
  (let [x (or (get p :x)
              (get p 0)
              (throw (str "No x coordinate in " p)))
        y (or (get p :y)
              (get p 1)
              (throw (str "No x coordinate in " p)))]
    {:x x :y y}))

  
;; shapes
(defn- pointy [kw]
  (fn [ps]
    (let [ps (->> (if (number? (first ps))
                    (->> ps
                         (partition 2 2)
                         (map vec))
                    ps)
                  (map point)
                  (map #(str (:x %) "," (:y %) " "))
                  (apply str))]
      [kw {:points ps}])))

(def polygon (pointy :polygon))

(defn circle
  ([spec] [:circle spec])
  ([p r] (let [{:keys [x y]} (point p)]
           (circle x y r)))
  ([x y r] (circle {:cx x :cy y :r r})))

(defn text [x y s]
  [:text {:x x :y y} (str s)])

;; game
(defn draw-rock [{:keys [x y id radius angle geometry colliding?]}]
  ^{:key id}
  [:g {:id (str "debug" id)
       :style #js {:stroke "white"
                   :fill (if colliding? "white" "none")
                   :strokeWidth 0.1}
       :transform (str "translate(" x " " y ") rotate(" (mod (+ 90 (* (/ 180 3.1415) angle))
                                                             360) ")")}
   [polygon (vec (map-indexed
                    (fn [i r]
                      (let [t (/ i (count geometry))
                            a (* t 3.1415 2)
                            x (* (+ radius r) (cos a))
                            y (* (+ radius r) (sin a))]
                        [x y]))
                    geometry))]])



(defn draw-rock-css [{:keys [x y id radius angle geometry colliding?]}]
  ^{:key id}
  [:div {:id (str "debug" id)
         :style #js {:position "fixed"
                     :top 0 :left 0
                     :transform
                     (str "translate(" (* 10 x) "px, " (* 10 y) "px) "
                          ) } }
   [:svg {:viewBox "-10 -10 20 20"
          :width 50
          :style #js {:background "green"
                      :stroke "white"
                      :fill (if colliding? "white" "none")
                      :strokeWidth 0.1}}]
   [polygon (vec (map-indexed
                   (fn [i r]
                     (let [t (/ i (count geometry))
                           a (* t 3.1415 2)
                           x (* (+ radius r) (cos a))
                           y (* (+ radius r) (sin a))]
                       [x y]))
                   geometry))]])

(defn ship-css [{:keys [x y a colliding?] :as data}]
  [:div {:style #js {:position "fixed"
                     :top 0 :left 0
                     :transform
                     (str "translate(" x "px, " y "px) "
                          "rotate(" (+ (* 90 (/ 3.1415 180)) a) "rad)") } }
   [:svg 
    {:viewBox "-2 -2 4 4"
     :width 100
     :style #js {:stroke (if colliding? "none" "white")
                 :fill (if colliding? "red" "none")
                 :strokeWidth 0.1}}
    [polygon [0 -1 -1 1 0 0.5 1 1]]]])

(defn ship [{:keys [x y a colliding?] :as data}]
  [:g
   ; [debug data]
   [:g
    {:transform (str "translate(" x " " y ") rotate(" (mod (+ 90 (* (/ 180 3.1415) a))
                                                           360) ")")
     :style #js {:stroke (if colliding? "none" "white")
                 :fill (if colliding? "red" "none")
                 :strokeWidth 0.1}}
    [polygon [0 -1 -1 1 0 0.5 1 1]]]])
    


(defn move [{:keys [x y a speed] :as obj}]
  (merge obj
         {:x (+ x (* speed @deltas (cos a)))
          :y (+ y (* speed @deltas (sin a)))}))

(defn pressed? [key]
  (some #(= (name key) (:key %)) @pressed-keys))

(defn move-player [player]
  (if (pressed? :ArrowUp)
    (move player)
    player))

(defn rotate-player [p]
  (cond
    (pressed? :ArrowLeft) (update p :a #(- % (* 10 @deltas)))
    (pressed? :ArrowRight) (update p :a #(+ % (* 10 @deltas)))
    :else p))

(defn player-physics [{:keys [x y a speed inertia] :as p}]
  (if-not (pressed? :ArrowUp)
    (let [x* (+ x (* inertia @deltas (cos a)))
          y* (+ y (* inertia @deltas (sin a)))
          inertia* (* inertia 0.95)]
      (assoc p :x x* :y y* :inertia inertia*))
    (assoc p :inertia speed)))
    

(defn spin-rock [{:keys [x y angle colliding?] :as r}]
  (let [speed 0.1]
    (assoc r :angle (+ angle (* 1 @deltas)))))

(defn update-rocks [rocks]
  (->> rocks
    (map spin-rock)))

(defn touching? [{x1 :x y1 :y r1 :radius}
                 {x2 :x y2 :y r2 :radius}]
  (let [d (+ r1 r2)]
    (<= (sqrt (+ (* (- x1 x2) (- x1 x2))
                 (* (- y1 y2) (- y1 y2))))
        d)))

(defn detect-collisions [{:keys [player rocks] :as s}]
  (let [colliding-rocks (map (fn [rock] 
                               (if (touching? player rock)
                                 (assoc rock :colliding? true)
                                 (assoc rock :colliding? false)))
                             rocks)
        player (if (pos? (count (filter :colliding? colliding-rocks)))
                 (assoc player :colliding? true)
                 (assoc player :colliding? false))]
    (assoc s :player player :rocks colliding-rocks)))

(defn wrap-player [{:keys [x y] :as player} x-max y-max]
  (assoc player
    :x (if (> x x-max)
         (- x x-max)
         x)
    :y (if (> y y-max)
         (- y y-max)
         y)))

                       

(defn update-state [s]
  (-> s
      (update :player #(-> %
                           move-player
                           rotate-player
                           player-physics))
      (update :rocks update-rocks)
      detect-collisions))

(defn rock* [x y radius heading]
  {:x x :y y :radius radius :angle (rand) :heading heading
   :geometry (for [i (range (int (+ 5 (rand 5))))] (+ 1 (rand)))
   :id (gensym)})

(def state
  (atom {:player {:x 15 :y 15
                  :a 0
                  :speed 20
                  :radius 2}
         :rocks (for [x (range 10) y (range 10)]
                  (rock* (+ 10 (* 10 x)) (+ 10 (* 10 y)) (+ 1 (rand 1)) (rand)))}))

(def player (cursor state [:player]))
(def rocks (cursor state [:rocks]))
(def bullet (cursor state [:bullet]))

(defn draw-bullet [{:keys [x y radius]}]
  [circle x y radius])

(defn asteroids-old []
  [:svg {:width "100%" :height "100%"
         :style #js {:background "black"
                     :fill "white"}}
   ; (text 10 40 (/ 1 @deltas))
   [:g {:transform "scale(10)"}
    (doall (for [r @rocks] ^{:key (:id r)} (draw-rock r)))
    (if @player
      [ship @player])
    [draw-bullet @bullet]]])

(defn asteroids []
  [:div  
   [:p (/ 1 @deltas)]
   (if @player
     [ship-css @player])
   (into [:div ] (for [r @rocks] [draw-rock-css r]))])

(defn transform [& args]
  {:transform (string/join " " args)})

(defn translate [x y]
  (str "translate(" x " " y ")"))

(defn scale [x y]
  (str "scale(" x " " y ")"))
  
;; illustrator
  
(defn curve [{:keys [ax ay bx by cx cy dx dy]}]
  [:path {:d (str "M" ax "," ay " C" bx "," by " " cx "," cy " " dx "," dy)}])           

(defn copy [{:keys [x y]} thing]
  [:g
   thing
   [:g (transform (translate x y))
    thing]])

(defn ncopy [{:keys [x y n]} thing]
  (letfn [(body [x y n thing]
                (if (pos? n)
                  [:g (transform (translate x y))
                   thing
                   [body x y (dec n) thing]]))]
    [:g thing
     [body x y n thing]]))
  
(def drawing
  (atom [ncopy {:x 100 :y 500 :n 1}
         [curve {:ax 100 :ay 250 
                 :bx 100 :by 100 
                 :cx 405 :cy 421 
                 :dx 400 :dy 250}]]))

(defn line-handle [atm [x-from y-from] [x-to y-to]]
  (let [attrs (cursor atm [1])]
    [:line {:x1 (@attrs x-from)
            :y1 (@attrs y-from)
            :x2 (@attrs x-to)
            :y2 (@attrs y-to)
            :style #js {:strokeWidth 1
                        :stroke "#bbb"
                        :opacity 0.75
                        :strokeDasharray "0.5,3"}}]))

(defn drag-handle [atm x-key y-key & childs]
  (let [attrs (cursor atm [1])
        drag-fn (fn [e]
                  (let [[x y]
                        (cond (.-shiftKey e) ;; snap to grid
                              [(* 50 (.round js/Math (/ (.-clientX e) 50)))
                               (* 50 (.round js/Math (/ (.-clientY e) 50)))]
                              :else ;; normal drag
                              [(.-clientX e) (.-clientY e)])]
                    (swap! attrs assoc
                           x-key x
                           y-key y)))]
    (fn [atm x-key y-key]
      [:g (transform (translate (x-key @attrs)
                                (y-key @attrs)))
        [:circle
         {:style #js {:fill "#bbb"
                      :opacity 0.75
                      :stroke "none"}
          :r 5
          :on-mouse-down
          (fn [e]
           (.addEventListener js/window "mousemove" drag-fn)
           (.addEventListener js/window "mouseup"
                              #(.removeEventListener js/window "mousemove" drag-fn)))}]
       childs])))

(defn +-handle [atm val-key]
  (let [attrs (cursor atm [1])]
    [:g {:style #js {:fill "black"
                     :stroke "none"}}
      [:text {:x 10 :y 5  :on-click #(swap! attrs update val-key inc) } "+"]
      [:text {:x 20 :y 5 :on-click #(swap! attrs update val-key dec) } "-"]]))

(defn illustrator []
  [:svg {:width "100%" :height "100%"
         :style #js {:background "white"
                     :fill "none"
                     :stroke "black"
                     :strokeWidth 10
                     :strokeLinecap "round"
                     "-webkit-user-select" "none"}};
    @drawing
    [drag-handle drawing :x :y
     [+-handle drawing :n]]
    (let [curve (cursor drawing [2])]
      [:g
        [line-handle curve [:ax :ay] [:bx :by]]
        [line-handle curve [:dx :dy] [:cx :cy]]
        [drag-handle curve :ax :ay]
        [drag-handle curve :cx :cy]
        [drag-handle curve :bx :by]
        [drag-handle curve :dx :dy]
        ])])
    
(defn on-js-reload []
  (println "Reloaded...")
  ; (reset! world/app-state (world/new-world))
  (reagent/render-component
    [asteroids-old]
    ; [illustrator]
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

