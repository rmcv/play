(ns bioinformatics.ex6d-test
  (:use midje.sweet)
  (:use bioinformatics.ex6d))


(fact
 (shared-kmers-problems 3 "AAACTCATC" "TTTCAAATC")
 => '([0 4] [0 0] [4 2] [6 6]))
