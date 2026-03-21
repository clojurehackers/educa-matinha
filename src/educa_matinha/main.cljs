(ns educa-matinha.main
  (:require
   [cljsjs.react]
   [cljsjs.react.dom]
   [sablono.core :as sab :include-macros true]))

(defonce game-state (atom {:started? false
                           :debug?   false}))

(defn start-game
  []
  (swap! game-state
         (fn [state]
           (-> state
               (assoc :started? (not (:started? @game-state)))
               (assoc :player {:pos {:row 30 :col 5}})))))

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
                        (get-in @game-state [:grid row col]))})}]))]]
     [:div
      [:div.h1 "game not started"]
      [:a.start-button {:onClick start-game}
       "START"]])))

(defn jump [e]
  (when (:started? @game-state)
    (let [key               (.-key e)
          {:keys [row col]} (-> @game-state :player :pos)
          new-pos           {:row (if (<= row 1)
                                    0
                                    (- row 2))
                             :col col}]
      (when (= key " ")
        (.preventDefault e)
        (swap! game-state assoc-in [:player :pos] new-pos)))))

(let [node (.getElementById js/document "app")]
  (defn renderer []
    (.render js/ReactDOM (main-template) node)))

(add-watch game-state :renderer (fn [_ _ _ _] (renderer)))

(renderer)

(.addEventListener js/document "keydown" jump)
;; TODO: event listener keypress C-c d toggle debug
