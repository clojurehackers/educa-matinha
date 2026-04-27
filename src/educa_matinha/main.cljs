(ns educa-matinha.main
  (:require
   [cljsjs.react]
   [cljsjs.react.dom]
   [sablono.core :as sab :include-macros true]))

(defonce game-state (atom {:started? false
                           :debug?   true
                           :jumping? false
                           :vert-vel 0
                           :hort-vel 0}))

(defonce tree-positions #{{:y 1 :l 0 :r 6} {:y 10 :l 0 :r 6} {:y 20 :l 0 :r 6} {:y 35 :l 0 :r 6} {:y 1 :l 13 :r 19} {:y 10 :l 13 :r 19} {:y 20 :l 13 :r 19} {:y 30 :l 13 :r 19}})
(def floor 39)
(def g -1)
(def ini-vel -5)

(defn to-px [num] (str num "px"))

(defn start-game
  []
  (swap! game-state
         (fn [state]
           (-> state
               (assoc :started? (not (:started? @game-state)))
               (assoc :player {:pos {:row floor :col 10}})))))

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
    (let [{:keys [row col]} (player :pos)]
        (sab/html [:div
                   {:key   (str row "-" col)
                    :style {:margin-top (to-px (* 16 row))
                            :margin-left (to-px (* 16 col))
                            :width "16px"
                            :height "16px"
                            :position "absolute"
                            :background-color "red"}}]))))

(defn collides? 
  "given the y trajectory and the x position of the player, 
   returns true whether the player is on top of a tree"
  [y y-new x]
  (let [next-tree     (->> tree-positions
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

(defn gravity []
  (let [vel (@game-state :vert-vel)
        started? (@game-state :jumping?)
        {:keys [row col]} (-> @game-state :player :pos)
        new-vel    (- vel g)
        y-new      (+ row new-vel)
        obstacle   (next-obstacle row y-new col)
        y-new      (min y-new obstacle)]
    (when (or started? (not= row obstacle))
      (swap! game-state merge {:jumping? false 
                               :vert-vel new-vel 
                               :player {:pos {:row y-new 
                                              :col col}}}))) 25)

(defn move [e]
  (when (:started? @game-state)
    (let [key               (.-key e)
          {:keys [row col]} (-> @game-state :player :pos)
          right-pos           {:row row
                               :col (if (< col 19) (+ col 1) col)}
          left-pos           {:row row
                              :col (if (> col 0) (- col 1) col)}]
        (cond
          (= key " ")
          (do
            (.preventDefault e)
            (swap! game-state merge {:jumping? true
                                     :vert-vel ini-vel}))
          (= key "d")
          (do (.preventDefault e)
              (swap! game-state assoc-in [:player :pos] right-pos))
          (= key "a")
          (do (.preventDefault e)
              (swap! game-state assoc-in [:player :pos] left-pos))))))

(defn main-template []
  (sab/html
   (if (:started? @game-state)
     (do
       (js/setTimeout (fn [] (gravity)) 25)
       [:div.center-container
        [:div.grid-container
          [:img {:src "../../images/background.png"
                 :style {:position "absolute"}}]
           (map render-tree tree-positions)
           (render-player @game-state)]])
     [:div
      [:div.h1 "game not started"]
      [:a.start-button {:onClick start-game}
       "START"]])))

(let [node (.getElementById js/document "app")]
  (defn renderer []
    (.render js/ReactDOM (main-template) node)))

(add-watch game-state :renderer (fn [_ _ _ _] (renderer)))

(renderer)

(.addEventListener js/document "keydown" move)
;; TODO: event listener keypress C-c d toggle debug
