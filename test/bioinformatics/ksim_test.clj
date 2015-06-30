(ns bioinformatics.ksim-test
  (:use midje.sweet)
  (:use bioinformatics.ksim))


(fact
 (match-up-to 2 "ACGTAG" "ACGGATCGGCATCGT")
 => '((1 4) (1 5) (1 6)))
