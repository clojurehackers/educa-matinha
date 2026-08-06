(ns educa-matinha.main
  (:require
   [cljsjs.react]
   [cljsjs.react.dom]
   [sablono.core :as sab :include-macros true]
   [educa-matinha.physics :as physics]))

(defonce tree-positions #{{:y 1 :l 0 :r 6} {:y 10 :l 0 :r 6} {:y 20 :l 0 :r 6} {:y 35 :l 0 :r 6} {:y 1 :l 13 :r 19} {:y 10 :l 13 :r 19} {:y 20 :l 13 :r 19} {:y 30 :l 13 :r 19}})
(def floor (* 39 16))
(def g 0.001)
(def jump-force -0.005)
 
(defonce game-state (atom {:started? false
                           :player   {:pos  [160 0] 
                                      :vel  [0.2 0]
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
                  :style {:margin-top (to-px row)
                          :margin-left (to-px col)
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

    :else
    10000))

(defn gravity
  "returns an updated player"
  [player delta-time]
  (physics/delta-pos player delta-time))

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
  [delta-time]
    (let [player (:player @game-state)
          [col row] (-> @game-state :player :pos)]

      (cond->> {}
        (and #_(= floor (-> player :pos second (Math/floor))) (contains? @keys-down "Space"))
        (merge (physics/apply-instant-force player [0 jump-force] delta-time))

        (contains? @keys-down "KeyD")
        (merge {:pos [(if (< col (* 16 19)) (+ col 1) col) row]})

        (contains? @keys-down "KeyA")
        (merge {:pos [(if (> col 0) (- col 1) col) row]}))))

(defn update-trees [trees]
  {:trees (map (fn [tree] (if (> (:y tree) floor) (assoc tree :y -10) (assoc tree :y (+ 0.1 (:y tree))))) trees)})

(defn floor-penetration
  "receives the player and returns the penetration depth between the player and the floor"
  [player]
  (- (-> player :pos second) floor))

(defn left-wall-penetration
  "receives the player and returns the penetration depth between the player and the left wall"
  [player]
  (- 0 (-> player :pos first)))

(defn right-wall-penetration
  "receives the player and returns the penetration depth between the player and the right wall"
  [player]
  (- (-> player :pos first) (* 16 19)))

#_(defn tree-penetration
  "player and a tree")

(defn collision-resolver
  "receives the player and resolves its current collisions"
  [player]
  (let [fp          (floor-penetration player)
        new-player  (if (<= fp 0.1) player (merge player (physics/resolve-collision player [0 -1] 0.5 fp)))
        lwp         (left-wall-penetration new-player)
        new-player  (if (<= lwp 0.1) new-player (merge new-player (physics/resolve-collision new-player [1 0] 1 lwp)))
        rwp         (right-wall-penetration new-player)
        new-player  (if (<= rwp 0.1) new-player (merge new-player (physics/resolve-collision new-player [-1 0] 1 rwp)))]
   new-player))

(defn change-state! [delta-time]
  (when (:started? @game-state)
    (let [new-player (-> (:player @game-state)
                         (merge (move delta-time))
                         (gravity delta-time)
                         (collision-resolver))]
      (swap! game-state merge {:player new-player}))))

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
