
;; the elements in the listings are 
 



;; A star implementation - this is terrible :)
(defn state->cost-sign [s s-end cost s-s->heur-f]
  [(+ cost (s-s->heur-f s s-end)) cost])
(defn state-state->cost-sign [s0 s1 s-end cost s-s->cost-f s-s->heur-f]
  (let [c (+ (s-s->cost-f s0 s1) cost)]
    (state->cost-sign s1 s-end s-s->heur-f)))

;; (defn state->heur-cost-sign [s0 s-goal cost s-s->heur-f]
;;   [(+ cost (s-s->heur-f s0 s-goal)) cost])

;; creating the next listing 
;; doing the A* search - ohh who are these ..        
;; listing :: [[sum cost] list-previous-states..] - so that's around 
(defn listing->last-state [ls]
  (first (second ls)))
(defn listing->cost-sign [ls]
  (first ls))

(defn state-listing->next-listing [s1 ls s-goal s-s->cost-f s-s->heur-f] 
  (let [[[c-sum c-cost] ss] ls
        [s0 & sr] ss                    ;the states 
        cost-sign (state-state->cost-sign s0 s1 s-goal c-cost 
                                          s-s->cost-f s-s->heur-f)]
    ;; the new listing .. 
    [cost-sign (cons s1 ss)]))


;; comparator for the sorted map 
;; the keys in the sorted map [sum cost]
(defn listing-comparator [sign0 sign1]
  (let [[s0 c0] sign0 
        [s1 c1] sign1]
    (< s0 s1)))
        
    
;; not sure about sorted-map-by 
(defn astar-search [s-start
                    s-end
                    
                    s->reachable-ss-f 
                    s-s->cost-f
                    s-s->heur-f]

  (let [ls (assoc (sorted-map-by listing-comparator)
             (state->cost-sign s-start s-end 0 s-s->heur-f)
             [s-start])]

    (loop [[l0 & lr] ls]
      ;; if there is listing 
      (if (= l0 s-end)
        (
        
             
                          
  (loop [[l0 & lr] [[(state->cost-sign str-s end-s s-s->heur-f 0)] [str-s]]]
    (let [[[_ h0 c0] ss0] l0
          ss (s->reachable-ss-f (first ss0))
      
         
;; you have nodes like that ... 
;; we will feel like           
(sorted-map-by (fn [