(ns bioinformatics.ex7g
  (:use [clojure.pprint]
        [bioinformatics.utils]))

(comment
  (let [[input output] (io "rosalind_7g")]
    (->> (time (suffix-array (first input)))
         (map first)
         (cl-format nil "~{~a~^, ~}")
         output)))
