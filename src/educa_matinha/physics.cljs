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
  [{:keys [pos vel acc] :as particle}
   delta-time]
  (let [delta-vel  (mapv #(* delta-time %) acc)
        new-vel    (mapv + vel delta-vel)
        delta-pos1 (mapv #(* delta-time %) vel)
        delta-pos2 (mapv #(* % delta-time delta-time 0.5) acc)
        new-pos    (mapv + pos delta-pos1 delta-pos2)]
    (assoc particle
           :pos new-pos
           :vel new-vel)))

(defn apply-force 
  "receives a particle as first argument and force as second argument
   retuns the particle after the force was applied"
  [{:keys [acc mass] :as particle} force]
  (let [delta-acc (mapv #(/ % mass) force)
        new-acc   (mapv + acc delta-acc)]
    (assoc particle :acc new-acc)))

#_(defn apply-force
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
  (def particle {:pos [0 0]
                 :vel [0 0]
                 :acc [0 0]
                 :mass 0.5})

  (delta-pos particle 8)
  ;; gravity force
  (def gravity [0 -20])
  (def jump [0 4])
  (apply-force particle gravity)


  (-> (apply-force particle jump)
      (apply-force (mapv #(* % -1) jump)))

  
  )
