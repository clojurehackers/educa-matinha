(ns educa-matinha.main
  (:require
   [cljsjs.react]
   [cljsjs.react.dom]
   [sablono.core :as sab :include-macros true]))

(defonce game-state (atom {:started? false
                           :debug?   true
                           :jumping? false}))

(defonce left-tree-positions #{{:y 1 :l 0 :r 6} {:y 10 :l 0 :r 6} {:y 20 :l 0 :r 6} {:y 35 :l 0 :r 6}})
(defonce right-tree-positions #{{:y 1 :l 13 :r 19} {:y 10 :l 13 :r 19} {:y 20 :l 13 :r 19} {:y 30 :l 13 :r 19}})
(def floor 39)
(def g -1)

(defn start-game
  []
  (swap! game-state
         (fn [state]
           (-> state
               (assoc :started? (not (:started? @game-state)))
               (assoc :player {:pos {:row floor :col 10}})))))

(defn render-tree
  [row col dir]
    (do
      (sab/html [:div.grid-cell
                 {:key   (str row "-" col)}
                 [:img {:src (str "../../images/" dir "-tree.png")}]])))

(defn main-template []
  (sab/html
   (if (:started? @game-state)
     [:div.center-container
      [:div.grid-container
       (for [row (range 40)
             col (range 20)]
         (let [player-pos (-> @game-state :player :pos)
               is-player? (and (= row (:row player-pos))
                               (= col (:col player-pos)))]
           [:div.grid-cell
            {:key   (str row "-" col)
             :style (merge
                                        ; For debug purpose only
                     (if (:debug? @game-state)
                       {:background-color "#f0f0f0"
                        :border           "1px solid #ddd"
                        :box-sizing       "border-box"}
                       {})
                     {:background-color
                      (if is-player?
                        "red"
                        (get-in @game-state [:grid row col]))})}

            (when (and (= col 0) (contains? left-tree-positions {:y row :l 0 :r 6})) (render-tree row col "left"))
            (when (and (= col 13) (contains? right-tree-positions {:y row :l 13 :r 19})) (render-tree row col "right"))]))]]
     [:div
      [:div.h1 "game not started"]
      [:a.start-button {:onClick start-game}
       "START"]])))

(defn sleep [ms]
  (js/Promise.
   (fn [resolve _reject]
     (js/setTimeout resolve ms))))

(defn collides? 
  "given the y trajectory and the x position of the player, 
   returns true whether the player is on top of a tree"
  [y y-new x]
  (let [next-tree     (->> left-tree-positions
                           (into right-tree-positions)
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

(defn gravity [vel started?]
  (let [{:keys [row col]} (-> @game-state :player :pos)
        new-vel    (- vel g)
        y-new      (+ row new-vel)
        obstacle   (next-obstacle row y-new col)
        y-new      (min y-new obstacle)]
    (when (or started? (not= row obstacle))
      (js/setTimeout (fn [] (do (swap! game-state assoc-in [:player :pos :row] y-new)
                                (gravity new-vel false))) 25))))

(defn jump [e]
  (when (and (:started? @game-state) (not= true (:jumping? @game-state)))
    (let [key     (.-key e)
          ini-vel -6]
      (when (= key " ")
        (.preventDefault e)
        (swap! game-state assoc :jumping? true)
        (gravity ini-vel true)
        (swap! game-state assoc :jumping? false)))))

(defn walk [e]
  (when (:started? @game-state)
    (let [key               (.-key e)
          {:keys [row col]} (-> @game-state :player :pos)
          right-pos           {:row row
                               :col (if (< col 19) (+ col 1) col)}
          left-pos           {:row row
                              :col (if (> col 0) (- col 1) col)}]
      (cond
        (= key "d")
        (do (.preventDefault e)
            (swap! game-state assoc-in [:player :pos] right-pos)
            (println "player-pos" (-> @game-state :player :pos)
                     (gravity 0 true)))
        (= key "a")
        (do (.preventDefault e)
            (swap! game-state assoc-in [:player :pos] left-pos)
            (println "player-pos" (-> @game-state :player :pos)
                     (gravity 0 true)))))))

(let [node (.getElementById js/document "app")]
  (defn renderer []
    (.render js/ReactDOM (main-template) node)))

(add-watch game-state :renderer (fn [_ _ _ _] (renderer)))

(renderer)

(.addEventListener js/document "keydown" jump)
(.addEventListener js/document "keydown" walk)
;; TODO: event listener keypress C-c d toggle debug
