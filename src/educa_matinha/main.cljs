(ns educa-matinha.main
  (:require
   [cljsjs.react]
   [cljsjs.react.dom]
   [sablono.core :as sab :include-macros true])
  #_(:import
     [java.lang Thread]))

(defonce game-state (atom {:started? false
                           :debug?   true}))

(defonce left-tree-positions #{0 10 20 35})
(defonce right-tree-positions #{0 10 20 30 49})
(def floor 39)

(defn start-game
  []
  (swap! game-state
         (fn [state]
           (-> state
               (assoc :started? (not (:started? @game-state)))
               (assoc :player {:pos {:row floor :col 10}})))))

(defn set-tree ;fix bug
  [row col sz]
  (when (and (>= row 0) (>= col 0) (> sz 0))
    (println "setting tree at" row col)
    (swap! game-state assoc-in [:trees :pos (keyword (str row)) (keyword (str col))] true)
    (println "trees: " (:trees @game-state))
    #_(set-tree row (+ 1 col) (- sz 1))))

(defn tree
  [row col dir]
  (let [tree-width 7]
    (do
      #_(set-tree row col 7)
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

            (when (and (= col 0) (contains? left-tree-positions row)) (tree row col "left"))
            (when (and (= col 13) (contains? right-tree-positions row)) (tree row col "right"))]))]]
     [:div
      [:div.h1 "game not started"]
      [:a.start-button {:onClick start-game}
       "START"]])))

(defn sleep [ms]
  (js/Promise.
   (fn [resolve _reject]
     (js/setTimeout resolve ms))))

(defn gravity []
  (let [player-pos (-> @game-state :player :pos)
        y (:row player-pos)
        ykey (keyword (str y))
        xkey (keyword (str (:col player-pos)))]
    (when (and (< y floor) ()#_(= false (-> @game-state :trees :pos ykey xkey))) ;this has a bug
      (js/setTimeout (fn [] (do (swap! game-state assoc-in [:player :pos :row] (+ y 1))
                                (println "going down: " (-> @game-state :player :pos))
                                (gravity))) 25))))

(defn jump [e]
  (when (:started? @game-state)
    (let [key               (.-key e)
          {:keys [row col]} (-> @game-state :player :pos)
          new-pos           {:row (if (= row floor) (- row 5) row)
                             :col col}]
      (when (= key " ")
        (.preventDefault e)
        (swap! game-state assoc-in [:player :pos] new-pos)
        (println "player-pos" (-> @game-state :player :pos))
        (gravity)))))

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
            (println "player-pos" (-> @game-state :player :pos)))
        (= key "a")
        (do (.preventDefault e)
            (swap! game-state assoc-in [:player :pos] left-pos)
            (println "player-pos" (-> @game-state :player :pos)))))))

(let [node (.getElementById js/document "app")]
  (defn renderer []
    (.render js/ReactDOM (main-template) node)))

(add-watch game-state :renderer (fn [_ _ _ _] (renderer)))

(renderer)

(.addEventListener js/document "keydown" jump)
(.addEventListener js/document "keydown" walk)
;; TODO: event listener keypress C-c d toggle debug
