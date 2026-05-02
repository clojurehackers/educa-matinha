(ns educa-matinha.physics
  "a particle is defined as a map of keys as the first argument where each key is a vector of coordinates
    :pos vector of the particle's position in space
    :vel vector of the particle's velocity
    :acc vector of the particle's acceleration
    :mass integer")

#_(
   {:pos [y x]
    :vel [y x]
    :acc [y x]
    :mass m}
   )

(defn delta-pos
  "receives a particle as first argument
   and delta-time between the current and last mesurement as the second argument
   returns an updated map with new position, velocity and acceleration"
  [{:keys [pos vel acc]}
   delta-time]
  (let [delta-vel (mapv #(* delta-time %) acc)
        new-vel   (mapv + vel delta-vel)
        new-pos   (mapv + pos new-vel)]
    {:pos new-pos
     :vel new-vel
     :acc acc}))

(defn apply-force 
  "receives a particle as first argument and force as second argument
   retuns the particle after the force was applied"
  [{:keys [acc mass] :as particle} force]
  (let [delta-acc (mapv #(/ % mass) force)
        new-acc   (mapv + acc delta-acc)]
    (assoc particle :acc new-acc)))

#_(defn collides?
  "receives ")

(comment
  (js/performance.now)
  (def time-a (js/performance.now))
  (def time-b (+ 60 time-a))
  (def particle {:pos [10 10] :vel [2 0] :acc [0 0] :mass 1})
  (-> particle 
      (apply-force [2 0])
      (delta-pos (- time-b time-a)))
  )