(ns educa-matinha.main
  (:require
   [cljsjs.react]
   [cljsjs.react.dom]
   [sablono.core :as sab :include-macros true]
   [educa-matinha.physics :as physics]))

(defonce tree-positions #{{:y 1 :l 0 :r 6} {:y 10 :l 0 :r 6} {:y 20 :l 0 :r 6} {:y 35 :l 0 :r 6} {:y 1 :l 13 :r 19} {:y 10 :l 13 :r 19} {:y 20 :l 13 :r 19} {:y 30 :l 13 :r 19}})
(def floor 39)
(def g 0.000001)
(def jump-force (/ -5 100))
 
(defonce game-state (atom {:started? false
                           :player   {:pos  [10 floor] 
                                      :vel  [0 0]
                                      :acc  [0 g]
                                      :mass 1}
                           :trees    tree-positions}))

(defonce keys-down (atom #{}))

(defn to-px [num] (str num "px"))

(defn start-game []
  (swap! game-state assoc :started? true))

(defn render-tree
  [{:keys [y l]}]
  (let [dir (if (= 0 l) "left" "right")]
      (sab/html [:div
                 {:key   (str y "-" l)
                  :style {:margin-top (to-px (* 16 y))
                          :margin-left (to-px (* 16 l))
                          :position "absolute"}}
                 [:img {:src (str "../../images/" dir "-tree.png")}]])))

(defn render-player 
  [{:keys [player]}]
  (when player
    (let [[col row] (:pos player)]
      (sab/html [:div
                 {:key   (str row "-" col)
                  :style {:margin-top (to-px (* 16 row))
                          :margin-left (to-px (* 16 col))
                          :width "16px"
                          :height "16px"
                          :position "absolute"
                          :background-color "red"}}]))))

(defn collides? 
  "given the y trajectory and the x position of the player, 
   returns true whether the player is on top of a tree"
  [y y-new x]
  (let [next-tree     (->> @game-state
                           :trees
                           (filter #(<= (+ y 1) (:y %)))
                           (filter #(>= y-new (:y %)))
                           (filter #(<= x (:r %)))
                           (filter #(>= x (:l %)))
                           first)
        next-tree-pos (:y next-tree)
        colides?      (pos? next-tree-pos)]
    (cond
      colides?
      (- next-tree-pos 1)
      :else
      false)))

(defn next-obstacle [y y-new col]
  (cond 
    (> y y-new) ; not droping 
    10000

    (collides? y y-new col)
    (collides? y y-new col)

    (>= y-new floor)
    floor

    :else
    10000))

(defn gravity
  "returns an updated player"
  [player delta-time]
  (let [new-player (physics/delta-pos player delta-time)
        [x y]      (-> player :pos)
        y-new      (-> new-player :pos (second))
        obstacle   (next-obstacle y y-new x)
        new-player (assoc-in new-player [:pos 1] (min y-new obstacle))]
    (when (and (>= y-new 0) (not= y obstacle)) new-player)))

(defn remove-commands [e]
  (let [code (str (.-code e))
        _    (.preventDefault e)]

    (cond
      (= code "Space")
      (swap! keys-down disj code)

      (= code "KeyD")
      (swap! keys-down disj code)

      (= code "KeyA")
      (swap! keys-down disj code))))

(defn add-commands [e]
  (let [code (str (.-code e))
        _    (.preventDefault e)]

    (cond
      (= code "Space")
      (swap! keys-down conj code)

      (= code "KeyD")
      (swap! keys-down conj code)

      (= code "KeyA")
      (swap! keys-down conj code))))

(defn move 
  "returns an updated player"
  []
    (let [player (:player @game-state)
          [col row] (-> @game-state :player :pos)]

      (cond->> {}
        (not= g (-> player :acc second))
        (merge (physics/apply-force player [0 (* -1 jump-force)]))

        (and (= g (-> player :acc second)) (contains? @keys-down "Space"))
        (merge (physics/apply-force player [0 jump-force]))

        (contains? @keys-down "KeyD")
        (merge {:pos [(if (< col 19) (+ col 1) col) row]})

        (contains? @keys-down "KeyA")
        (merge {:pos [(if (> col 0) (- col 1) col) row]}))))

(defn update-trees [trees]
  {:trees (map (fn [tree] (if (> (:y tree) floor) (assoc tree :y -10) (assoc tree :y (+ 0.1 (:y tree))))) trees)})

(defn change-state! [delta-time]
  (when (:started? @game-state)
    (let [new-player (merge (:player @game-state) (move))]
      (swap! game-state merge {:player (-> new-player
                                           (merge (gravity new-player delta-time))
                                           #_(merge (update-trees (:trees @game-state))))}))))

(defn render-game []
  (sab/html 
     [:div.center-container
      (if (:started? @game-state)
        [:div.grid-container
         [:img {:src   "../../images/background.png"
                :style {:position "absolute"}}]
         (map render-tree (:trees @game-state))
         (render-player @game-state)]
        
        [:div
         [:a.start-button {:onClick start-game}
          "START"]])]))


(defn renderer [last-time]
  (fn [timestamp]
    (let [delta-time (if (not= nil last-time) (- timestamp last-time) 0)
          node       (.getElementById js/document "app")]
      (.render js/ReactDOM (render-game) node)
      (change-state! delta-time)
      (js/requestAnimationFrame (renderer timestamp)))))

(.addEventListener js/document "keydown" add-commands)
(.addEventListener js/document "keyup" remove-commands)

(def start (js/performance.now))
(js/requestAnimationFrame (renderer start))
