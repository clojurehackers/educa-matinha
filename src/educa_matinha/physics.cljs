(ns educa-matinha.physics
  "a particle is defined as a map of keys as the first argument where each key is a vector of coordinates
    :pos vector of the particle's position in space
    :vel vector of the particle's velocity
    :acc vector of the particle's acceleration
    :mass real inverse of the mass")

(defn ax
  "receives a scalar a and a vector x, returns the multiplication ax"
  [a x]
  (map #(* a %) x))

(defn sum
  "receives two vectors and returns their sum"
  [a b]
  (map + a b))

(defn sub
  "receives two vectors and subtracts them"
  [a b]
  (sum a (ax -1 b)))

(defn dot
  "receives two vectors and retuns their dot product"
  [a b]
  (apply + (map * a b)))

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

(defn separating-velocity
  "receives two particles and the contact normal,
   returns the scalar separating velocity"
  [p0 p1 normal]
  (let [{vel0 :vel}  p0
        {vel1 :vel}  p1
        relative-vel (if (p1) (sub vel0 vel1) (vel0))]
    (dot relative-vel normal)))

(defn resolve-velocity
  "receives two particles, the contact normal and restitution constant,
   returns the new velocities of the particles"
  [p0 p1 normal restitution]
  (let [sep-vel (separating-velocity p0 p1 normal)]
    (when (sep-vel <= 0)
      (let [new-sep-vel      (->> sep-vel (* -1) (* restitution))
            delta-vel        (- new-sep-vel sep-vel)
            total-inv-mass   (if p1 (+ (:mass p0) (:mass p1)) (:mass p0))
            impulse          (/ delta-vel total-inv-mass)
            impulse-per-mass (ax impulse normal)
            new-vel0         (ax (:mass p0) impulse-per-mass)
            new-vel1         (ax (:mass p1) impulse-per-mass)]
        [new-vel0 new-vel1]))))

(defn resolve-interpenetration
  "receives two particles, the contact normal and penetration
   returns new positions of the particles"
  [p0 p1 normal penetration]
  (when (> penetration 0)
    (let [total-inv-mass (if p1 (+ (:mass p0) (:mass p1)) (:mass p0))
          mov-per-mass   (->> normal (ax -1) (ax (/ penetration total-inv-mass)))
          new-pos0       (sum (:pos p0) (ax (:mass p0) mov-per-mass))
          new-pos1       (sum (:pos p1) (ax (:mass p1) mov-per-mass))]
      [new-pos0 new-pos1])))

#_(defn resolve-collision
  "receives two particles"
  [p0 p1]
  (resolve-velocity p0 p1)
  (resolve-interpenetration p0 p1))

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
      (apply-force (mapv #(* % -1) jump))))
