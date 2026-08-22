;; This implementation is primarily based on the paper “Rhythm-Based Level Generation for 2D Platformers”, available at: https://www.researchgate.net/publication/220795055_Rhythm-based_level_generation_for_2D_platformers

(ns educa-matinha.world-generation)

(def player-action #{:player-action/jump})
(def beat-type     #{:beat-type/regular :beat-type/random :beat-type/swing})
(def beat-density  #{:beat-density/low :beat-density/medium :beat-density/high})

(def tree-width  #{:tree-width/small :tree-width/medium :tree-width/large})
(def tree-height #{:tree-height/low :tree-height/high})
(def tree        #{tree-width tree-height})

(defn- round-2 [n]
  (js/Number (.toFixed n 2)))

(defn- rhythm-generator-1
  ([previous-action possibly-actions]
   (let [min-time (if (empty? previous-action)
                    1
                    (inc (nth previous-action 2)))
         start    (+ min-time (rand-int 5))
         delta    (/ (+ 15 (rand-int 86)) 100.00)]
     (vector
      (rand-nth possibly-actions)
      (round-2 start)
      (-> start (+ delta) round-2)))))

(defn rhythm-generator
  [possibly-actions]
  (->> []
       (iterate #(rhythm-generator-1 % (vec possibly-actions)))
       rest))
