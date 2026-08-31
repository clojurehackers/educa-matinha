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
  (mapv #(* a %) x))

(defn sum
  "receives vectors and returns a vector representing their sum"
  ([a] a)
  ([a & vectors]
   (mapv + a (apply sum vectors))))

(defn sub
  "receives two vectors and subtracts them"
  [a b]
  (sum a (ax -1 b)))

(defn dot
  "receives two vectors and retuns their dot product"
  [a b]
  (apply + (map * a b)))

(defn update-position
  "receives a particle as the first argument,
   time between the current and last mesurement as the second argument,
   returns a particle with new velocity based on current acceleration"
  [{:keys [pos vel acc]
    :as   particle}
   delta-time]
  (let [delta-pos1 (ax delta-time vel)
        delta-pos2 (ax (* delta-time delta-time 0.5) acc)
        new-pos    (sum pos delta-pos1 delta-pos2)]
    (assoc particle :pos new-pos)))

(defn apply-acceleration
  "receives a particle as the first argument,
   time between the current and last mesurement as the second argument,
   returns a particle with new velocity based on current acceleration"
  [{:keys [vel acc] 
    :as   particle} delta-time]
  (let [delta-vel (ax delta-time acc)
        new-vel   (sum vel delta-vel)]
    (assoc particle :vel new-vel)))

(defn apply-force 
  "receives a particle as first argument and force as second argument
   retuns the particle after the force was applied"
  [{:keys [acc mass] :as particle} force]
  (let [delta-acc (ax (/ 1 mass) force)
        new-acc   (sum acc delta-acc)]
    (assoc particle :acc new-acc)))

(defn apply-instant-force
  "receives a particle as the first argument, force as the second argument 
   and delta-time as the third argument
   retuns the particle after the force was applied for an instant"
  [{:keys [acc]
    :as   particle} force delta-time]
  (-> particle
      (apply-force force)
      (apply-acceleration delta-time)
      (assoc :acc acc)))

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
       (if (<= sep-vel 0)
         (let [{vel0 :vel}      p0
               {vel1 :vel}      p1
               new-sep-vel      (* sep-vel -1 restitution)
               delta-vel        (- new-sep-vel sep-vel)
               total-inv-mass   (+ (:mass p0) (:mass p1))
               impulse          (/ delta-vel total-inv-mass)
               impulse-per-mass (ax impulse normal)
               new-vel0         (ax (:mass p0) impulse-per-mass)
               new-vel1         (ax (:mass p1) impulse-per-mass)]
           [(assoc p0 :vel (sum vel0 new-vel0))
            (assoc p1 :vel (sum vel1 new-vel1))])
         [p0 p1]))))

(defn resolve-interpenetration
  "receives one or two particles, the contact normal and penetration depth
   returns particles with updated positions"
  ([{:keys [pos] 
     :as   particle} normal penetration]
   (assoc particle :pos (sum pos (ax penetration normal))))

  ([p0 p1 normal penetration]
   (let [total-inv-mass (+ (:mass p0) (:mass p1))
         mov-per-mass   (->> normal (ax (/ penetration total-inv-mass)))
         new-pos0       (sum (:pos p0) (ax (:mass p0) mov-per-mass))
         new-pos1       (sum (:pos p1) (ax (:mass p1) mov-per-mass))]
     [(assoc p0 :pos new-pos0) (assoc p1 :pos new-pos1)])))

(defn interval-intersect
  "receives two intervals and returns whether they intersect"
  [a b c d]
  (if (< a c)
    (if (< b c) 0 (- b c))
    (if (< d a) 0 (- d a))))

(defn rect-rect-collision
  "receives two rectangles:
   {:pos [x y] (top-left corner)
    :len [l h] (lenght of the sides)}
   and the normal
   returns the penetration depth"
  [{[x1 y1] :pos
    [l1 h1] :len}
   {[x2 y2] :pos
    [l2 h2] :len}
   normal]
  (let [px (interval-intersect x1 (+ x1 l1) x2 (+ x2 l2))
        py (interval-intersect y1 (+ y1 h1) y2 (+ y2 h2))]
    (if (and (> py 0) (> px 0))
      (Math/abs (dot [px py] normal)) ; TODO: add direction
      0)))

(defn resolve-collision
  "receives one or two particles and resolves their collision"
  ([particle normal restitution penetration]
   (if (> penetration penetration-slop)
     (-> particle
         (resolve-velocity normal restitution)
         (resolve-interpenetration normal penetration))
     particle))
  
  ([p0 p1 normal restitution _p]
   (let [penetration (rect-rect-collision p0 p1 normal)] 
     (if (> penetration penetration-slop)
       (-> p0
           (resolve-velocity p1 normal restitution)
           first
           (resolve-interpenetration p1 normal penetration)
           first)
       nil))))

(defonce world (atom {}))

#_(defn collision-orchestrator
  "when called, checks for any collision between objects in the world"
  []
  (let [objects (-> @world vals vec)
        pairs (combo/combinations objects 2)]
    (->> pairs
        (map resolve-collision))))

#_(collision-orchestrator)

(defn create-object!
  "receives the name, position, length, mass, velocity and acceleration
   mutates the word, adding the object"
  [name pos len vel acc mass]
  (swap! world assoc (keyword name) {:pos  pos
                                     :vel  vel
                                     :len  len
                                     :mass mass
                                     :acc  acc}))

(defn update-object!
  [name object]
  (swap! world merge {(keyword name) object}))

(defn get-object
  "receive the object"
  [name]
  ((keyword name) @world))

(comment
  (def game {:started? false
             :paused?  false
             :player   {:pos  [160 642]
                        :len  [16 16]
                        :vel  [0.05 3]
                        :acc  [0 0.002]
                        :mass 1}
             :floor    {:pos  [0 640]
                        :len  [320 10]
                        :vel  [0 0]
                        :acc  [0 0]
                        :mass 0}})
  (separating-velocity (:player game) [0 -1])
  (separating-velocity (:player game) (:floor game) [0 -1])
  (resolve-velocity (:player game) #_(:floor game) [0 -1] 0.5)
  (resolve-velocity (:player game) (:floor game) [0 -1] 0.5)
  (resolve-interpenetration (:player game) [0 -1] 2)
  (resolve-interpenetration (:player game) (:floor game) [0 -1] 2)
  (resolve-collision (:player game) (:floor game) [0 -1] 0.5 2)
  (Math/abs 0)
  )