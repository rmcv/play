(ns bioinformatics.ex7n
  (:use [bioinformatics.utils]))

(defn multi-pattern-match [txt s]
  (let [mx (count (first s))]
    (->> (partition mx 1 txt)
         (map-indexed vector)
         (filter (fn [[_ p]] (contains? s p)))
         (map first))))

(comment
  (let [input  (readfile "rosalind_7n.txt")
        txt    (first input)
        ps     (into #{} (map seq (drop 1 input)))]
    (->> (multi-pattern-match txt ps)
         (map str)
         sort
         (cl-format nil "~{~a~^ ~}")
         (writefile "rosalind_7n_output.txt"))))
