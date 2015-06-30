(ns bioinformatics.ex1g-test
  (:use midje.sweet)
  (:use bioinformatics.ex1g))


(fact
 (pattern-match "ATAT" "GATATATGCATATACTT")
 => '(1 3 9))
