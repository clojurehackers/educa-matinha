(ns educa-matinha.main
  (:require
   [cljsjs.react]
   [cljsjs.react.dom]
   [sablono.core :as sab :include-macros true])
  #_(:import
     [java.lang Thread]))

(def base-grid-cols 56)
(def base-grid-rows 31)
(def container-padding-px 16)
(def preferred-cell-size-px 18)
(def default-grid {:cols base-grid-cols
                   :rows base-grid-rows
                   :floor (dec base-grid-rows)
                   :square? true})

(defonce game-state (atom {:started? false
                           :debug?   true
                           :jumping? false
                           :physics-running? false
                           :grid default-grid}))

(def tick-ms 25)
(def dt (/ tick-ms 1000))
(def player-mass 1)
(def meters-per-cell 0.1)
(def earth-gravity-mps2 9.81)
(def jump-height-m 0.38)
(def jump-force-duration-ms 70)
(def run-jump-force-boost 0.6)
(def max-run-speed 24)
(def run-accel 28)
(def run-decel 32)
(def tree-count 40)

(defn viewport-grid-dimensions
  []
  (let [viewport-width (.-innerWidth js/window)
        viewport-height (.-innerHeight js/window)
        usable-width (max 120 (- viewport-width (* 2 container-padding-px)))
        usable-height (max 120 (- viewport-height (* 2 container-padding-px)))
        cell-size preferred-cell-size-px
        cols (max 12 (int (js/Math.floor (/ usable-width cell-size))))
        rows (max 12 (int (js/Math.floor (/ usable-height cell-size))))]
    {:cols cols
     :rows rows
     :floor (dec rows)
     :square? true}))

(def gravity-force
  {:row (/ earth-gravity-mps2 meters-per-cell)
   :col 0})

(def jump-force
  (let [gravity-rows-per-s2 (:row gravity-force)
        jump-height-cells (/ jump-height-m meters-per-cell)
        takeoff-velocity (js/Math.sqrt (* 2 gravity-rows-per-s2 jump-height-cells))
        jump-duration-s (/ jump-force-duration-ms 1000)
        jump-accel (+ (/ takeoff-velocity jump-duration-s)
                      gravity-rows-per-s2)]
    {:row (- jump-accel)
     :col 0}))

(defn random-tree-dir
  []
  (if (< (rand) 0.5) "left" "right"))

(defn blocked-tree-cell?
  [grid row col]
  (let [{:keys [cols rows]} grid
        spawn-col (js/Math.floor (/ cols 2))]
    (or (>= row (- rows 3))
        (and (>= row (- rows 8))
             (<= (js/Math.abs (- col spawn-col)) 2)))))

(defn random-tree-map
  ([]
   (random-tree-map (:grid @game-state)))
  ([grid]
   (loop [trees {}]
    (if (>= (count trees) tree-count)
      trees
      (let [{:keys [cols rows]} grid
            row (rand-int (- rows 4))
            col (rand-int cols)
            cell [row col]]
        (if (blocked-tree-cell? grid row col)
          (recur trees)
          (recur (assoc trees cell (random-tree-dir)))))))))

(defn start-game
  []
  (swap! game-state
         (fn [state]
           (let [{:keys [cols floor]} (:grid state)]
             (-> state
               (assoc :started? (not (:started? @game-state)))
               (assoc :player {:pos {:row floor
                                     :col (js/Math.floor (/ cols 2))}
                               :vel {:row 0 :col 0}})
                (assoc :jump-force-until nil)
               (assoc :jump-force-scale 1)
                (assoc :pressed-keys #{})
               (assoc :physics-running? false)
                (assoc :trees (random-tree-map (:grid state))))))))

(defn sync-grid-dimensions!
  []
  (let [new-grid (viewport-grid-dimensions)]
    (swap! game-state
           (fn [state]
             (let [prev-grid (:grid state)
                   {:keys [cols rows floor]} new-grid
                   player (or (:player state)
                              {:pos {:row floor
                                     :col (js/Math.floor (/ cols 2))}
                               :vel {:row 0 :col 0}})
                   clamped-player {:pos {:row (-> (get-in player [:pos :row] floor)
                                                  (max 0)
                                                  (min floor))
                                         :col (-> (get-in player [:pos :col] 0)
                                                  (max 0)
                                                  (min (dec cols)))}
                                   :vel (:vel player)}
                   kept-trees (into {}
                                    (filter (fn [[[row col] _dir]]
                                              (and (< row rows)
                                                   (< col cols))))
                                    (:trees state))]
               (-> state
                   (assoc :grid new-grid)
                   (assoc :player clamped-player)
                   (assoc :trees (if (or (nil? prev-grid) (empty? kept-trees))
                                   (random-tree-map new-grid)
                                   kept-trees))))))))

(defn tree
  [row col dir]
  (sab/html
   [:img.tree-image
    {:key (str row "-" col "-tree")
     :src (str "../../images/" dir "-tree.png")
     :alt ""}]))

(defn main-template []
  (sab/html
   (if (:started? @game-state)
     (let [{:keys [cols rows square?]} (:grid @game-state)
           player-pos (-> @game-state :player :pos)
           trees (:trees @game-state)
           player-x (:col player-pos)
           player-y (:row player-pos)]
       [:div.center-container
        [:div.grid-container
         {:class (when square? "grid-container-square")
          :style {:grid-template-columns (str "repeat(" cols ", var(--cell-size))")
                  :grid-template-rows (str "repeat(" rows ", var(--cell-size))")
                  :width (str "calc(var(--cell-size) * " cols ")")
                  :height (str "calc(var(--cell-size) * " rows ")")}}
         (for [row (range rows)
               col (range cols)]
           (let [tree-dir (get trees [row col])]
             [:div.grid-cell
              {:key   (str row "-" col)
               :style (merge
                       (if (:debug? @game-state)
                         {:background-color "#f0f0f0"
                          :border           "1px solid #ddd"
                          :box-sizing       "border-box"}
                         {})
                       {:background-color (get-in @game-state [:grid row col])})}

              (when tree-dir (tree row col tree-dir))]))
         [:div.player-dot
          {:style {:transform (str "translate(calc(var(--cell-size) * " player-x "), "
                                   "calc(var(--cell-size) * " player-y "))")}}]]])
     [:div
      [:div.h1 "game not started"]
      [:a.start-button {:onClick start-game}
       "START"]])))

(defn sleep [ms]
  (js/Promise.
   (fn [resolve _reject]
     (js/setTimeout resolve ms))))

(defn airborne?
  []
  (< (get-in @game-state [:player :pos :row])
     (get-in @game-state [:grid :floor])))

(defn horizontal-direction
  []
  (let [pressed-keys (:pressed-keys @game-state)
        left? (contains? pressed-keys "a")
        right? (contains? pressed-keys "d")]
    (cond
      (and left? (not right?)) -1
      (and right? (not left?)) 1
      :else 0)))

(defn net-force
  []
  (let [jumping? (:jumping? @game-state)
        jump-force-scale (:jump-force-scale @game-state)
        jump-force-active? (and jumping?
                                (some? (:jump-force-until @game-state))
                                (< (.now js/Date) (:jump-force-until @game-state)))]
    {:row (+ (:row gravity-force)
             (if jump-force-active?
               (* (:row jump-force) jump-force-scale)
               0))
     :col 0}))

(defn next-horizontal-vel
  [current-vel]
  (let [direction (horizontal-direction)
        target-vel (* direction max-run-speed)
        accel-step (* run-accel dt)
        decel-step (* run-decel dt)]
    (cond
      (pos? direction) (min target-vel (+ current-vel accel-step))
      (neg? direction) (max target-vel (- current-vel accel-step))
      (pos? current-vel) (max 0 (- current-vel decel-step))
      (neg? current-vel) (min 0 (+ current-vel decel-step))
      :else 0)))

(declare physics-step)

(defn ensure-physics-loop!
  []
  (when (and (:started? @game-state)
             (not (:physics-running? @game-state)))
    (swap! game-state assoc :physics-running? true)
    (physics-step)))

(defn physics-step
  []
  (let [{:keys [cols floor]} (:grid @game-state)
        {:keys [pos vel]} (:player @game-state)
        force (net-force)
        next-horizontal-vel (next-horizontal-vel (:col vel))
        accel {:row (/ (:row force) player-mass)
               :col (/ (:col force) player-mass)}
        next-col (-> (+ (:col pos) (* next-horizontal-vel dt))
                     (max 0)
                     (min (dec cols)))
        next-row (+ (:row pos)
                    (* (:row vel) dt)
                    (* 0.5 (:row accel) dt dt))
        next-vel-row (+ (:row vel) (* (:row accel) dt))
        clamped-row (-> next-row (max 0) (min floor))
        landed? (>= next-row floor)
        should-continue? (or (not landed?)
                             (not (zero? next-horizontal-vel))
                             (not (zero? (horizontal-direction))))]
    (swap! game-state
           (fn [state]
             (-> state
                 (assoc-in [:player :pos :col] next-col)
                 (assoc-in [:player :pos :row] clamped-row)
                 (assoc-in [:player :vel :col] next-horizontal-vel)
                 (assoc-in [:player :vel :row] (if landed? 0 next-vel-row))
                 (assoc :jumping? (not landed?))
                 (assoc :physics-running? should-continue?)
                 (assoc :jump-force-scale (if landed? 1 (:jump-force-scale state)))
                 (assoc :jump-force-until
                        (when (and (not landed?)
                                   (< (.now js/Date) (or (:jump-force-until state) 0)))
                          (:jump-force-until state))))))
    (when should-continue?
      (js/setTimeout physics-step tick-ms))))

(defn jump [e]
  (when (:started? @game-state)
    (let [key (.-key e)]
      (when (= key " ")
        (.preventDefault e)
        (when (and (not (:jumping? @game-state))
                   (not (airborne?)))
          (let [run-speed-ratio (min 1 (/ (js/Math.abs (get-in @game-state [:player :vel :col]))
                                          max-run-speed))
                jump-force-scale (+ 1 (* run-speed-ratio run-jump-force-boost))]
            (swap! game-state
                   (fn [state]
                     (-> state
                         (assoc :jumping? true)
                         (assoc :jump-force-scale jump-force-scale)
                         (assoc :jump-force-until (+ (.now js/Date) jump-force-duration-ms)))))
            (ensure-physics-loop!)))))))

(defn track-keydown [e]
  (let [key (.-key e)]
    (when (contains? #{"a" "d" " "} key)
      (swap! game-state update :pressed-keys (fnil conj #{}) key)
      (when (contains? #{"a" "d"} key)
        (ensure-physics-loop!)))))

(defn track-keyup [e]
  (let [key (.-key e)]
    (when (contains? #{"a" "d" " "} key)
      (swap! game-state update :pressed-keys disj key))))

(let [node (.getElementById js/document "app")]
  (defn renderer []
    (.render js/ReactDOM (main-template) node)))

(sync-grid-dimensions!)

(add-watch game-state :renderer (fn [_ _ _ _] (renderer)))

(renderer)

(.addEventListener js/window "resize" sync-grid-dimensions!)
(.addEventListener js/document "keydown" track-keydown)
(.addEventListener js/document "keyup" track-keyup)
(.addEventListener js/document "keydown" jump)
;; TODO: event listener keypress C-c d toggle debug
