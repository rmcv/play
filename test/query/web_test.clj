(ns query.web-test
  (:use midje.sweet)
  (:use query.web))

(fact
 (standardize {:id 3136,
               :band "A1",
               :cores {:eng-lang 7, :chi-lang 4, :math 6, :ls 7},
               :electives
               {"Biology" 6,
                "Chemistry" 5,
                "Eng Lit" 6,
                "Mathematics Extended Modules I" 6}})
 => {:id 3136,
     :band "A1",
     :4+1 30,
     :4+2 36,
     :4+3 41,
     :best5 32,
     :best6 37,
     :all 41.5})
