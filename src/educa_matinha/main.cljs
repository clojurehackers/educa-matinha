(ns educa-matinha.main
  (:require
   [cljsjs.react]
   [cljsjs.react.dom]
   [sablono.core :as sab :include-macros true]))

(defonce game-state (atom {:started? false
                           :debug? false}))

(defn start-game
  []
  (swap! game-state assoc :started? (not (:started? @game-state))))

(defn main-template []
  (sab/html
   (if (:started? @game-state)
     [:div.center-container
      [:div.grid-container
       (for [row (range 40)
             col (range 20)]
         [:div.grid-cell
          {:key (str row "-" col)
           :style (merge
                   {:background-image (get-in @game-state [:grid row col])}
                   ; For debug purpose only
                   (if (:debug? @game-state)
                     {:background-color "#f0f0f0"
                      :border "1px solid #ddd"
                      :box-sizing "border-box"}
                     {}))}])]]
     [:div
      [:div.h1 "game not started"]
      [:a.start-button {:onClick start-game}
       "START"]])))

(let [node (.getElementById js/document "app")]
  (defn renderer []
    (.render js/ReactDOM (main-template) node)))

(add-watch game-state :renderer (fn [_ _ _ _] (renderer)))

(renderer)
