(ns bioinformatics.utils)

(defn running-count [pred]
  (fn [step]
    (let [cnt (volatile! 0)]
      (fn
        ([] (step))
        ([r x]
         (if (pred x) (vswap! cnt inc))
         (step r @cnt))
        ([r]
         (step r))))))

(defn count-same
  ([] (count-same identity))
  ([kf] (fn [step]
          (let [cnt (volatile! 0)
                prev (volatile! ::none)
                prevx (volatile! ::none)]
            (fn
              ([] (step))
              ([r x]
               (let [xx (kf x)
                     dprev @prev]
                 (if (identical? dprev xx)
                   (do (vswap! cnt inc)
                       r)
                   (let [dprevx  @prevx
                         dcnt    @cnt]
                     (vreset! prev xx)
                     (vreset! prevx x)
                     (vreset! cnt 1)
                     (if (not= dprev ::none)
                       (step r (list dprevx dcnt))
                       r)
                     ))))
              ([r]
               (let [r (step r (list @prevx @cnt))]
                 (step r))))))))
