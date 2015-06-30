(ns bioinformatics.ex7n)

(defn multi-pattern-match [txt s]
  (let [mx (count (first s))]
    (->> (partition mx 1 txt)
         (map-indexed vector)
         (filter (fn [[_ p]] (contains? s p)))
         (map first))))

(comment
  (let [input  (->> (slurp (str (System/getProperty "user.home") "/Downloads/rosalind_7n.txt"))
                    (clojure.string/split-lines))
        txt    (first input)
        ps     (into #{} (map seq (drop 1 input)))]
    (->> (multi-pattern-match txt ps)
         (map str)
         sort
         (cl-format nil "~{~a~^ ~}")
         (spit (str (System/getProperty "user.home") "/Downloads/rosalind_7n_output.txt")))))
