(ns educa-matinha.main
  (:require
   [cljsjs.react]
   [cljsjs.react.dom]
   [sablono.core :as sab :include-macros true]
   [educa-matinha.physics :as physics]))

(def tree-vel 0.01)

(defonce trees #{{:pos [66 32]
                  :len [66 0]
                  :vel [0 tree-vel]
                  :acc [0 0]}
                 
                 {:pos [66 160]
                  :len [66 0]
                  :vel [0 tree-vel]
                  :acc [0 0]}

                 {:pos [66 320]
                  :len [66 0]
                  :vel [0 tree-vel]
                  :acc [0 0]}

                 {:pos [66 460]
                  :len [66 0]
                  :vel [0 tree-vel]
                  :acc [0 0]}

                 {:pos [274 16]
                  :len [66 0]
                  :vel [0 tree-vel]
                  :acc [0 0]}

                 {:pos [274 160]
                  :len [66 0]
                  :vel [0 tree-vel]
                  :acc [0 0]}

                 {:pos [274 320]
                  :len [66 0]
                  :vel [0 tree-vel]
                  :acc [0 0]}

                 {:pos [274 480]
                  :len [66 0]
                  :vel [0 tree-vel]
                  :acc [0 0]}})

(def floor 624)
(def g 0.002)
(def jump-force -0.01)

(defonce game-state (atom {:started? false
                           :paused?  false
                           :player   {:pos  [160 floor]
                                      :len  [8 8]
                                      :vel  [0.05 0]
                                      :acc  [0 g]
                                      :mass 1}
                           :trees    trees}))

(defonce keys-down (atom #{}))

(defn to-px [num] (str num "px"))

(defn start-game! []
  (swap! game-state assoc :started? true))

(defn pause-game! []
  (when (:started? @game-state)
    (swap! game-state assoc :paused? true)))

(defn resume-game! []
  (when (:started? @game-state)
    (swap! game-state assoc :paused? false)))

(defn render-tree
  [tree]
  (let [{[x y]   :pos
         [dx dy] :len} tree
        l              (- x dx)
        y              (- y dy)
        dir            (if (= 0 l) "left" "right")]
    (sab/html [:div
               {:key   (str y "-" l)
                :style {:margin-top  (to-px y)
                        :margin-left (to-px l)
                        :position    "absolute"}}
               [:img {:src (str "../../images/" dir "-tree.png")}]])))

(defn render-player
  [{:keys [player]}]
  (when player
    (let [{[col row] :pos
           [dx dy]   :len} player
          col              (- col dx)
          row              (- row dy)]
      (sab/html [:div
                 {:key   (str row "-" col)
                  :style {:margin-top       (to-px row)
                          :margin-left      (to-px col)
                          :width            "16px"
                          :height           "16px"
                          :position         "absolute"
                          :background-color "orange"}}]))))

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
  (let [player (:player @game-state)
        [col row] (-> @game-state :player :pos)]

    (cond->> {}
      (contains? @keys-down "Space")
      (merge (physics/apply-instant-force player [0 jump-force] delta-time))

      (contains? @keys-down "KeyD")
      (merge {:pos [(+ col 5) row]})

      (contains? @keys-down "KeyA")
      (merge {:pos [(- col 5) row]}))))

(defn update-tree [{:keys [pos] :as tree} delta-time]
  (if (> (second pos) (+ 16 floor))
    (assoc-in tree [:pos 1] -96)
    (physics/update-position tree delta-time)))

(defn update-trees [trees delta-time]
  (->> trees
       (map #(update-tree % delta-time))
       set))

(defn floor-penetration
  "receives the player and returns the penetration depth between the player and the floor"
  [{:keys [pos len]}]
  (- (second pos) (second len) floor))

(defn left-wall-penetration
  "receives the player and returns the penetration depth between the player and the left wall"
  [{:keys [pos len]}]
  (+ (first len) (- 0 (first pos))))

(defn right-wall-penetration
  "receives the player and returns the penetration depth between the player and the right wall"
  [{:keys [pos len]}]
  (- (first pos) (first len) (* 16 19)))

(defn tree-penetration
  "given a player, 
   returns the penetration deph of the closest tree"
  [player]
  (->> @game-state
       :trees
       (map #(physics/rect-rect-collision % player))
       (apply max)))

(defn collision-resolver
  "receives the player and resolves its current collisions"
  [player]
  (let [fp         (floor-penetration player)
        new-player (if (< fp 0) player (merge player (physics/resolve-collision player [0 -1] 0.5 fp)))
        lwp        (left-wall-penetration new-player)
        new-player (if (< lwp 0) new-player (merge new-player (physics/resolve-collision new-player [1 0] 1 lwp)))
        rwp        (right-wall-penetration new-player)
        new-player (if (< rwp 0) new-player (merge new-player (physics/resolve-collision new-player [-1 0] 1 rwp)))
        tp         (tree-penetration new-player)
        new-player (if (< tp 0.1) new-player (merge new-player (physics/resolve-collision new-player [0 -1] 0 tp)))]
    new-player))

(defn change-state! [delta-time]
  (when (and (:started? @game-state) (not (:paused? @game-state)))
    (let [new-player (-> (:player @game-state)
                         (merge (move delta-time))
                         (physics/update-position delta-time)
                         (collision-resolver)
                         (gravity delta-time))
          new-trees (update-trees (:trees @game-state) delta-time)]
      (swap! game-state merge {:player new-player
                               :trees  new-trees}))))

(defn render-game []
  (sab/html
   [:div.center-container
    [:div.border-container]
    [:div.grid-container
     [:img {:src   "../../images/background.png"
            :style {:position "absolute"
                    :opacity  "60%"}}]
     (if (:started? @game-state)
       (if (:paused? @game-state)
         [:div
          [:a.resume-button {:onClick resume-game!}
           "RESUME"]]
         
         [(map render-tree (:trees @game-state))
          (render-player @game-state)])
       
       [:div
        [:a.start-button {:onClick start-game!}
         "START"]])]
    [:div.border-container]]))


(defn renderer [last-time]
  (fn [timestamp]
    (let [delta-time (if (not= nil last-time) (- timestamp last-time) 0)
          node       (.getElementById js/document "app")]
      (when (contains? @keys-down "Escape") (pause-game!))
      (.render js/ReactDOM (render-game) node)
      (change-state! delta-time)
      (js/requestAnimationFrame (renderer timestamp)))))

(.addEventListener js/document "keydown" add-commands)
(.addEventListener js/document "keyup" remove-commands)

(def start (js/performance.now))
(js/requestAnimationFrame (renderer start))
