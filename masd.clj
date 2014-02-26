

(defn arr2d->dims [a2d]
  [(alength a2d) (alength (aget a2d 1))])

(defn arr2d-map [f a2d & params]
  (let [[d0 d1] (arr2d->dims a2d)]
    (to-array-2d 
     (for [i0 (range d0)]
       (for [i1 (range d1)]
         (apply f (cons (aget a2d i0 i1) params)))))))

(defn arr2d->vals [a2d]
  (let [[d0 d1] (arr2d->dims a2d)]
    (for [i0 (range d0)
          i1 (range d1)]
      (aget a2d i0 i1))))
 
;; (aget (array-2d-map (fn [v] (+ 10 v)) (to-array-2d [[1 2 3] [3 4 5]]))
;; (defn array-2d-max [rs]
;;   (let [[d0 d1] (raster->dim rs)]
;;     (apply max
      
(defn nths [coll [i0 & ir]]
  (if ir 
    (nths (nth coll i0) ir)
    (nth coll i0)))

;; (defn rec-map [f coll &rcoll]
;;   (if (coll? coll)
;;     (rec

(defn nonneg? [v]
  (not (neg? v)))