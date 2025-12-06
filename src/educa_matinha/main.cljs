(ns educa-matinha.main
  (:require
   [cljsjs.react]
   [cljsjs.react.dom]
   [sablono.core :as sab :include-macros true]))

(defn what-kind? []
  "Cruel")

(js/console.log (what-kind?))

(defonce game-state (atom {:started false}))

(defn start-game
  []
  (swap! game-state assoc :started (not (:started @game-state))))

(defn main-template []
  (sab/html [:div.h1 (if (:started @game-state) "game started" "game not started")
             (sab/html [:a.start-button {:onClick start-game}
                        (if (:started @game-state)
                          "QUIT"
                          "START")])]))

(let [node (.getElementById js/document "app")]
  (defn renderer []
    (.render js/ReactDOM (main-template) node)))

(add-watch game-state :renderer (fn [_ _ _ _] (renderer)))

(renderer)
