(ns bioinformatics.utils-test
  (:use midje.sweet)
  (:use bioinformatics.utils))


(fact
 (into [] (running-count #(= % \a)) "aabbcCcddeeggffabcd")
 => [1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 3 3 3])

(fact
 (into [] (comp (running-count #(= % \a))
                (remove #(= 1 %))
                (filter even?)) "aabbcCcddeeggffabcd")
 => [2 2 2 2 2 2 2 2 2 2 2 2 2 2])

(fact
 (sequence (running-count #(= % \a)) "aabbcCcddeeggffabcd")
 => '(1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 3 3 3))

(fact
 (into [] (count-same) "12233344445666667")
 => ['(\1 1) '(\2 2) '(\3 3) '(\4 4) '(\5 1) '(\6 5) '(\7 1)])


(fact
 (into [] (comp (running-count #(= % \a))
                (count-same)
                (map last)) "aabbcCcddeeggffabcd")
 => [1 14 4])
