(ns educa-matinha.physics
  "a particle is defined as a map of keys as the first argument where each key is a vector of coordinates
    :pos vector of the particle's position in space
    :vel vector of the particle's velocity
    :acc vector of the particle's acceleration
    :mass real inverse of the mass")

(def penetration-slop 0.01)

(defn ax
  "receives a scalar a and a vector x, returns the multiplication ax"
  [a x]
  (vec (map #(* a %) x)))

(defn sum
  "receives two vectors and returns their sum"
  [a b]
  (vec (map + a b)))

(defn sub
  "receives two vectors and subtracts them"
  [a b]
  (sum a (ax -1 b)))

(defn dot
  "receives two vectors and retuns their dot product"
  [a b]
  (apply + (map * a b)))

(defn mult
  "receives two vectors and returns their multiplication"
  [a b]
  (vec (map * a b)))

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

(defn apply-instant-force
  "receives a particle as first argument and force as second argument
   retuns the particle after the force was applied for an instant"
  [{:keys [acc mass] :as particle} force delta-time]
  (let [delta-acc (mapv #(/ % mass) force)
        new-acc   (mapv + acc delta-acc)]
    (-> particle
        (assoc :acc new-acc)
        (delta-pos delta-time)
        (assoc :acc acc))))

(defn separating-velocity
  "receives one or two particles and the contact normal,
   returns the scalar separating velocity"
  ([p0 normal]
     (dot (:vel p0) normal))
  
  ([p0 p1 normal]
   (let [{vel0 :vel}  p0
         {vel1 :vel}  p1
         relative-vel (sub vel0 vel1)]
     (dot relative-vel normal))))

(defn resolve-velocity
  "receives one or two particles, the contact normal and restitution constant,
   returns the particle(s) with updated velocities"
  ([{:keys [vel]
     :as   particle} normal restitution]
   (let [sep-vel (separating-velocity particle normal)]
     (if (<= sep-vel 0)
       (let [new-sep-vel (* sep-vel -1 restitution)
             delta-vel   (- new-sep-vel sep-vel)
             new-vel     (ax delta-vel normal)]
         (assoc particle :vel (sum vel new-vel)))
       particle)))
  
  ([p0 p1 normal restitution]
     (let [sep-vel (separating-velocity p0 p1 normal)]
       (when (<= sep-vel 0)
         (let [{vel0 :vel}  p0
               {vel1 :vel}  p1
               new-sep-vel      (* sep-vel -1 restitution)
               delta-vel        (- new-sep-vel sep-vel)
               total-inv-mass   (+ (:mass p0) (:mass p1))
               impulse          (/ delta-vel total-inv-mass)
               impulse-per-mass (ax impulse normal)
               new-vel0         (ax (:mass p0) impulse-per-mass)
               new-vel1         (ax (:mass p1) impulse-per-mass)]
           [(assoc p0 :vel (sum vel0 new-vel0))
            (assoc p1 :vel (sum vel1 new-vel1))])))))

(defn resolve-interpenetration
  "receives one or two particles, the contact normal and penetration
   returns new positions of the particles"
  ([{:keys [pos] 
     :as   particle} normal penetration]
   (if (> penetration penetration-slop)
     (assoc particle :pos (sum pos (ax penetration normal)))
     particle))

  ([p0 p1 normal penetration]
   (when (> penetration 0)
     (let [total-inv-mass (+ (:mass p0) (:mass p1))
           mov-per-mass   (->> normal (ax -1) (ax (/ penetration total-inv-mass)))
           new-pos0       (sum (:pos p0) (ax (:mass p0) mov-per-mass))
           new-pos1       (sum (:pos p1) (ax (:mass p1) mov-per-mass))]
       [(assoc p0 :pos new-pos0) (assoc p1 :pos new-pos1)]))))

(defn resolve-collision
  "receives one or two particles and resolves their collision"
  ([particle normal restitution penetration]
   (-> particle
       (resolve-velocity normal restitution)
       (resolve-interpenetration normal penetration)))
  
  ([p0 p1 normal restitution penetration]
   (-> p0
       (resolve-velocity p1 normal restitution)
       (resolve-interpenetration p1 normal penetration))))

(defn interval-intersect
  "receives two intervals and returns whether they intersect"
  [a b c d]
  (if (< a c)
    (if (< b c) 0 (- b c))
    (if (< d a) 0 (- d a))))

(defn rect-rect-collision
  "receives two rectangles:
   {:p1 [x y] (top-left corner)
    :p2 [x y] (bottom-right corner)}
   and returns the penetration depth and the normal"
  [rect1 rect2]
  (let [{[a b]   :pos
         [da db] :len} rect1
        {[d c]   :pos
         [dd dc] :len} rect2
        x1             (- a da)
        y1             (- b db)
        x2             (+ a da)
        y2             (+ b db)
        x3             (- d dd)
        y3             (- c dc)
        x4             (+ d dd)
        y4             (+ c dc)
       ; _ (println y1 y2 y3 y4)
       ; _ (println x1 x2 x3 x4)
        ]
    (if (> (interval-intersect x1 x2 x3 x4) 0)
      (interval-intersect y1 y2 y3 y4)
      0)))

(comment
  (js/performance.now)
  (def time-a (js/performance.now))
  (def time-b (+ 60 time-a))
  (def particle {:pos [0 -1]
                 :vel [0 5]
                 :acc [0 0]
                 :mass 1})
  
  (dot [1 0] [0 1])
  (dot (:vel particle) [1 0])
  
  (separating-velocity particle [0 1])
  (* -1 (separating-velocity particle [0 1]))
  (-> particle (resolve-velocity [0 -1] 1)
      first
      (resolve-interpenetration [0 -1] 1))
  (resolve-velocity particle [0 -1] 1)
  (resolve-interpenetration particle [0 -1] 1)
  (resolve-collision particle [0 -1] 1 1)
  
  (delta-pos particle 8)
  ;; gravity force
  (def gravity [0 -20])
  (def jump [0 4])
  (apply-force particle gravity)
  (if (not= nil particle) [1 1] [0 0])


  (-> (apply-force particle jump)
      (apply-force (mapv #(* % -1) jump)))
  
  (def tree {:pos [66 40]
             :len [66 40]} )

  
  (def player {:pos [200 0]
               :len [8 8]})
  (rect-rect-collision tree player)

  (apply max [0 1 nil])
  )
