
;; MECHANIC/LOGIC - creating these fields 
(defn vec+vec [v0 v1] 
  (map + v0 v1))
(defn vec-vec [p0 p1]
  (map - p0 p1))
(defn vec*vec [p0 p1]
  (map * p0 p1))
(defn vec*scl [p s]
  (map * p (repeat s)))
(defn vec->len [p]
  (Math/pow (reduce + (map #(Math/pow % 2) p)) 0.5))
(defn vec->norm [p]
  (vec*scl p (/ 1.0 (vec->len p) )))
(defn vec->len-vec [p l]
  (vec*scl (vec->norm p) l))

(defn vec<vec [p0 p1 dim]
  (< (nth p0 dim) (nth dim)))
(defn vec>vec [p0 p1 dim]
  (> (nth p0 dim) (nth dim)))
(defn vec=vec? [v0 v1]
  (every? identity (map = v0 v1)))
(defn vec-dist [v0 v1]
  (Math/pow (reduce + (map #(Math/pow % 2) (vec-vec v0 v1)))
            0.5))

(defn vec-vec->dist [v0 v1]
  (Math/pow (reduce + (map #(Math/pow % 2) (vec-vec v0 v1)))
            0.5))
(defn vec-vec->diff [v0 v1]
  (map - v1 v0))
(defn vec-vec->dir [v0 v1]
  (vec->norm (vec-vec->diff v0 v1)))


;; rotate 
(defn vec-rad->rotate [[p0 p1] rad]        
  [(- (* p0 (Math/cos rad)) (* p1 (Math/sin rad)))
   (+ (* p0 (Math/sin rad)) (* p1 (Math/cos rad)))])
(defn vec-deg->rotate [pos deg]        
  (vec-rad->rotate pos (Math/toRadians deg)))

;; so it's the vectors between - we will see more 
(defn vec2d-vec2d->rad [v0 v1]
  (Math/acos (/ (apply + (vec*vec v0 v1))
                (* (vec->len v0) (vec->len v1)))))
(defn vec2d-vec2d->deg [v0 v1]
  (Math/toDegrees (vec2d-vec2d->rad v0 v1)))


;; getting them 
(defn values->unitize [vs]
  (let [vmax (apply max vs)
        vmin (apply min vs)
        diff (- vmax vmin)]
    (map (fn [v] (/ (- v vmin) diff)) vs)))
  
  


;; weighted average stuff around :)
(defn sum [vs]
  (apply + vs))
(defn avg [vs]
  (/ (apply + vs) (count vs)))
(defn wavg [vs ws]
  (/ (apply + (map * vs ws))
     (apply + ws)))

(defn vecs->wavg [vs ws]
  (loop [[v0 & vr] vs
         [w0 & wr] ws
         ret (repeat 0)]
     
    (if v0
      (recur vr wr (vec+vec ret (vec*scl v0 w0)))
      (vec*scl ret (/ 1.0 (apply + ws))))))
