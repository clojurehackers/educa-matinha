(ns educa-matinha.main
  (:require
   [cljsjs.react]
   [cljsjs.react.dom]
   [sablono.core :as sab :include-macros true]
   [educa-matinha.physics :as physics]))

(def tree-vel 0.01)

(defonce trees #{{:pos [0 32]
                  :len [112 0]
                  :vel [0 tree-vel]
                  :acc [0 0]}

                 {:pos [0 160]
                  :len [112 0]
                  :vel [0 tree-vel]
                  :acc [0 0]}

                 {:pos [0 320]
                  :len [112 0]
                  :vel [0 tree-vel]
                  :acc [0 0]}

                 {:pos [0 460]
                  :len [112 0]
                  :vel [0 tree-vel]
                  :acc [0 0]}

                 {:pos [208 16]
                  :len [112 0]
                  :vel [0 tree-vel]
                  :acc [0 0]}

                 {:pos [208 160]
                  :len [112 0]
                  :vel [0 tree-vel]
                  :acc [0 0]}

                 {:pos [208 320]
                  :len [112 0]
                  :vel [0 tree-vel]
                  :acc [0 0]}

                 {:pos [208 480]
                  :len [112 0]
                  :vel [0 tree-vel]
                  :acc [0 0]}})

(def floor 640)
(def g 0.002)
(def jump-force -0.01)

(defonce game-state (atom {:started?   false
                           :paused?    false
                           :player     {:pos  [160 0]
                                        :len  [16 16]
                                        :vel  [0.05 0]
                                        :acc  [0 g]
                                        :mass 1}
                           :trees      trees
                           :floor      {:pos  [0 floor]
                                        :len  [320 10]
                                        :vel  [0 0]
                                        :acc  [0 0]
                                        :mass 0}
                           :left-wall  {:pos  [-10 0]
                                        :len  [10 650]
                                        :vel  [0 0]
                                        :acc  [0 0]
                                        :mass 0}
                           
                           :right-wall {:pos  [320 0]
                                        :len  [10 650]
                                        :vel  [0 0]
                                        :acc  [0 0]
                                        :mass 0}}))

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
                        :width            "16px"
                        :height           "16px"
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
  (if (> (second pos) floor)
    (assoc-in tree [:pos 1] -96)
    (physics/update-position tree delta-time)))

(defn update-trees [trees delta-time]
  (->> trees
       (map #(update-tree % delta-time))
       set))

(defn tree-penetration
  "given a player, 
   returns the penetration deph of the closest tree"
  [player]
  (->> @game-state
       :trees
       (map #(physics/rect-rect-collision % player [0 1]))
       (apply max)))

(defn collision-resolver
  "receives the player and resolves its current collisions"
  [player]
  (let [new-player (merge player (physics/resolve-collision player (:floor @game-state) [0 -1] 0.5 0))
        new-player (merge player (physics/resolve-collision new-player (:left-wall @game-state) [1 0] 1 0))
        new-player (merge player (physics/resolve-collision new-player (:right-wall @game-state) [-1 0] 1 0))
        tp         (tree-penetration new-player)
        new-player (if (< tp 0.1) new-player (merge new-player (physics/resolve-collision new-player [0 -1] 0 tp)))]
    new-player))

(defn change-state! [{:keys [started? paused? trees player]} delta-time]
  (when (and started? (not paused?))
    (let [new-player (-> player
                         (merge (move delta-time))
                         (physics/update-position delta-time)
                         (collision-resolver)
                         (gravity delta-time))
          new-trees (update-trees trees delta-time)]
      (swap! game-state merge {:player new-player
                               :trees  new-trees}))))

(defn render-game [{:keys [started? paused? trees player]}]
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

         [(map render-tree trees)
          (render-player player)])

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

(def start (js/performance.now))
(js/requestAnimationFrame (renderer start))
