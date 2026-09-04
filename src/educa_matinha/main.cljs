(ns educa-matinha.main
  (:require
   [cljsjs.react]
   [cljsjs.react.dom]
   [sablono.core :as sab :include-macros true]
   [educa-matinha.physics :as physics]))

(def floor 640)
(def jump-force -0.01)

(defonce game-state (atom {:started?   false
                           :paused?    false}))

(defonce keys-down (atom #{}))

(defn to-px [num] (str num "px"))

(defn start-game! []
  (swap! game-state assoc :started? true))

(defn pause-game! []
  (when (:started? @game-state)
    (swap! game-state assoc :paused? true)))

(defn resume-game! []
  (swap! game-state assoc :paused? false))

(defn render-tree
  [{[x y] :pos}]
  (let [dir (if (= 0 x) "left" "right")]
    (sab/html [:div
               {:key   (str y "-" x)
                :style {:margin-top  (to-px y)
                        :margin-left (to-px x)
                        :position    "absolute"}}
               [:img {:src (str "../../images/" dir "-tree.png")}]])))

(defn render-player
  [player]
  (let [{[col row] :pos
         [dx dy]   :len} player]
    (sab/html [:div
               {:key   (str row "-" col)
                :style {:margin-top       (to-px row)
                        :margin-left      (to-px col)
                        :width            (to-px dx)
                        :height           (to-px dy)
                        :position         "absolute"
                        :background-color "orange"}}])))

(defn gravity
  "returns an updated player"
  [player delta-time]
  (physics/apply-acceleration player delta-time))

(defn remove-commands [e]
  (let [code (str (.-code e))
        _    (.preventDefault e)]

    (cond
      (= code "Space")
      (swap! keys-down disj code)

      (= code "KeyD")
      (swap! keys-down disj code)

      (= code "KeyA")
      (swap! keys-down disj code)

      (= code "Escape")
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
      (swap! keys-down conj code)

      (= code "Escape")
      (swap! keys-down conj code))))

(defn move
  "returns an updated player"
  [delta-time]
  (let [player (physics/get-object "player")
        [col row] (:pos player)]

    (cond->> {}
     (contains? @keys-down "Space")
      (merge (physics/apply-instant-force player [0 jump-force] delta-time))

      (contains? @keys-down "KeyD")
      (merge {:pos [(+ col 5) row]})

      (contains? @keys-down "KeyA")
      (merge {:pos [(- col 5) row]}))))

(defn update-tree [{:keys [pos] :as tree} delta-time]
  (if (> (second pos) floor)
    (assoc-in tree [:pos 1] -96)
    (physics/update-position tree delta-time)))

(defn tree-penetration
  "receives the argument player 
   and returns an updated player with resolved collisions between trees"
  [player]
  (let [collisions (->> (map #(physics/get-object (str "tree-" %)) [0 1 2 3 4 5])
                        (map #(physics/resolve-collision player % [0 -1] 0 0)))] 
    (apply merge `(~@collisions))))

(defn collision-resolver
  "receives the argument player 
   and returns an updated player with resolved collisions"
  [player]
  (let [new-player (merge player (physics/resolve-collision player (physics/get-object "floor") [0 -1] 0.5 0))
        new-player (merge new-player (physics/resolve-collision new-player (physics/get-object "left-wall") [1 0] 1 0))
        new-player (merge new-player (physics/resolve-collision new-player (physics/get-object "right-wall") [-1 0] 1 0))
        new-player (merge new-player (tree-penetration new-player))]
    new-player))     

(defn change-state! [{:keys [started? paused?]} delta-time]
  (when (and started? (not paused?))
    (let [new-player (-> "player"
                         (physics/get-object)
                         (merge (move delta-time))
                         (physics/update-position delta-time)
                         (collision-resolver)
                         (gravity delta-time))]
      (physics/update-object! "player" new-player)
      (mapv #(physics/update-object! (str "tree-" %) (update-tree (physics/get-object (str "tree-" %)) delta-time)) [0 1 2 3 4 5]))))

(defn render-game [{:keys [started? paused?]}]
  (sab/html
   [:div.center-container
    [:div.border-container
     {:style {:margin-top "-10px"}}]
    [:div.grid-container
     [:img {:src   "../../images/background.png"
            :style {:position "absolute"
                    :opacity  "60%"}}]
     (if started?
       (if paused?
         [:div
          [:a.resume-button {:onClick resume-game!}
           "RESUME"]]

         [(map #(render-tree (physics/get-object (str "tree-" %))) [0 1 2 3 4 5])
          (render-player (physics/get-object "player"))])
       [:div
        [:a.start-button {:onClick start-game!}
         "START"]])]
    [:div.border-container]]))


(defn renderer [last-time]
  (fn [timestamp]
    (let [delta-time (if (not= nil last-time) (- timestamp last-time) 0)
          node       (.getElementById js/document "app")]
      (when (contains? @keys-down "Escape") (pause-game!))
      (.render js/ReactDOM (render-game @game-state) node)
      (change-state! @game-state delta-time)
      (js/requestAnimationFrame (renderer timestamp)))))

(.addEventListener js/document "keydown" add-commands)
(.addEventListener js/document "keyup" remove-commands)

(defn start-game []
  (let [start-time (js/performance.now)
        tree-vel   0.01
        g          0.002]
    
    ;create player
    (physics/create-object! "player" [160 0] [16 16] [0.05 0] [0 g] 1)

    ;create constraints
    (physics/create-object! "floor" [0 floor] [320 20] [0 0] [0 0] 0)
    (physics/create-object! "right-wall" [320 0] [10 650] [0 0] [0 0] 0)
    (physics/create-object! "left-wall" [-10 0] [10 650] [0 0] [0 0] 0)

    ;create trees: 
    (mapv #(physics/create-object! (str "tree-" %) [0 (* % 200)] [112 5] [0 tree-vel] [0 0] 0) [0 1 2])
    (mapv #(physics/create-object! (str "tree-" %) [208 (- (* % 100) 300 )] [112 5] [0 tree-vel] [0 0] 0) [3 4 5])

    (js/requestAnimationFrame (renderer start-time))))

(start-game)
