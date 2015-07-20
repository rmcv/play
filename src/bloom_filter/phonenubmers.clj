(ns bloom-filter.phonenumbers
  (:require [bloom-filter.core :refer :all]))



(def bf (volatile! (create 5000000 0.001)))

(time (doseq [p (take 5000000 (repeatedly #(+ 9000000000 (rand-int 100000000))))]
        (vswap! bf #(insert (str p) %))))

(set! *print-length* 10)


(->> (take 10000 (repeatedly #(str (+ 9000000000 (rand-int 100000000)))))
     (map #(find-element % @bf))
     (filter true?)
     count
     time)
