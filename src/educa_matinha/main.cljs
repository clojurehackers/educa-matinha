(ns educa-matinha.main
  (:require
   [cljsjs.react]
   [cljsjs.react.dom]
   [sablono.core :as sab :include-macros true]))

(defonce game-state (atom {:started? false
                           :debug?   true
                           :jumping? false}))

(defonce left-tree-positions #{{:y 0 :tam 7} {:y 10 :tam 7} {:y 20 :tam 7} {:y 35 :tam 7}})
(defonce right-tree-positions #{{:y 0 :tam 7} {:y 10 :tam 7} {:y 20 :tam 7} {:y 30 :tam 7}})
(def floor 39)
(def g -1)

(defn start-game
  []
  (swap! game-state
         (fn [state]
           (-> state
               (assoc :started? (not (:started? @game-state)))
               (assoc :player {:pos {:row floor :col 10}})))))

(defn tree
  [row col dir]
  (let [tree-width 7]
    (do
      (sab/html [:div.grid-cell
                 {:key   (str row "-" col)}
                 [:img {:src (str "../../images/" dir "-tree.png")}]]))))

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

            (when (and (= col 0) (contains? left-tree-positions {:y row :tam 7})) (tree row col "left"))
            (when (and (= col 13) (contains? right-tree-positions {:y row :tam 7})) (tree row col "right"))]))]]
     [:div
      [:div.h1 "game not started"]
      [:a.start-button {:onClick start-game}
       "START"]])))

(defn sleep [ms]
  (js/Promise.
   (fn [resolve _reject]
     (js/setTimeout resolve ms))))

(defn is-tree-left? [y y-new col]
  (let [next-tree     (->> left-tree-positions
                           (filter #(<= (+ y 1) (:y %)))
                           (filter #(>= y-new (:y %)))
                           first)
        next-tree-pos (:y next-tree)
        next-tree-len (:tam next-tree)
        colides?      (and (pos? next-tree-pos) (< col next-tree-len))]
    (cond
      colides?
      (- next-tree-pos 1)
      :else
      false)))

(defn is-tree-right? [y y-new col]
  (let [next-tree     (->> right-tree-positions
                           (filter #(<= (+ y 1) (:y %)))
                           (filter #(>= y-new (:y %)))
                           first)
        next-tree-pos (:y next-tree)
        colides?      (and (pos? next-tree-pos) (> col 12))]
    (cond 
      colides? 
      (- next-tree-pos 1)
      :else
      false)))

(defn is-tree? [y y-new col]
  (cond
    (is-tree-right? y y-new col)
    (is-tree-right? y y-new col)

    (is-tree-left? y y-new col)
    (is-tree-left? y y-new col)))

(defn next-obstacle [y y-new col]
  (cond 
    (> y y-new) ; not droping 
    10000

    (is-tree? y y-new col)
    (is-tree? y y-new col)

    (>= y-new floor)
    floor

    :else
    10000))

(defn gravity [vel started?]
  (let [player-pos (-> @game-state :player :pos)
        y          (:row player-pos)
        x          (:col player-pos)
        new-vel    (- vel g)
        y-new      (+ y new-vel)
        obstacle   (next-obstacle y y-new x)
        _          (println "here: " y y-new obstacle)
        y-new      (min y-new obstacle)]
    (when (or started? (and (not= y floor) (not= y obstacle)))
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
