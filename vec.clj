
;; MECHANIC/LOGIC - creating these fields 
(defn vec+vec [p0 p1]
  (map + p0 p1))
(defn vec-vec [p0 p1]
  (map - p0 p1))
(defn vec*scl [p s]
  (map * p (repeat s)))
(defn vec->len [p]
  (Math/pow (reduce + (map #(Math/pow % 2) p)) 0.5))
(defn vec->norm [p]
  (vec*scl p (/ 1.0 (vec->len p) )))
(defn vec-len->vec [p l]
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
