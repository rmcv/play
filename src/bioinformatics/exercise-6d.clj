(ns bioinformatics.exercises)

(use 'clojure.pprint)

(defn reverse-complement [xs]
  (let [dna-complement {\A \T, \T \A, \C \G, \G \C}]
    (->> xs
         reverse
         (mapv dna-complement))))

(defn shared-kmers-problems [k s1 s2]
  (letfn [(get-pair [l1 l2]
            (when-let [[a i1] (first l1)]
              (when-let [[b i2] (first l2)]
                (condp #(%1 %2 0) (compare a b)
                  =  (cons [i1 i2]
                           (lazy-seq (get-pair (rest l1) (rest l2))))
                  <  (lazy-seq (get-pair (rest l1) l2))
                  >  (lazy-seq (get-pair l1 (rest l2)))))))
          (gen-seq [genome f]
            (->> (partition k 1 genome)
                 (map-indexed (fn [i e] [(f e) i]))
                 (sort-by first)))]
    (let [l1 (gen-seq s1 vec)
          l2 (gen-seq s2 vec)
          l3 (gen-seq s2 (comp reverse-complement vec))]
      (->> (get-pair l1 l2)
           (map (fn [[x y]] [x 0 y]))
           (concat (->> (get-pair l1 l3)
                        (map (fn [[x y]] [x 1 y]))))
           sort
           (map (fn [[x _ z]] [x z]))
           (cl-format nil "~{(~{~a~^, ~})~%~}")))))

(shared-kmers-problems 3 "AAACTCATCATC" "TTTCAAATC")

(def path #(str "./" %))
(def in-file (path "rosalind_6d.txt"))
(def out-file (path "rosalind_6d_output.txt"))

(let [txt  (clojure.string/split-lines (slurp in-file))
      k    (read-string (first txt))
      s1   (nth txt 1)
      s2   (nth txt 2)]

  (->> (shared-kmers-problems k s1 s2)
       (spit out-file)))

(let [txt  (clojure.string/split-lines (slurp in-file))
      k    (read-string (first txt))
      s1   (nth txt 1)
      s2   (nth txt 2)
      l1   (vec (partition k 1 s1))
      l2   (vec (partition k 1 s2))
      out  (->> (slurp out-file)
                (clojure.string/split-lines)
                (map read-string))
      s    #(apply str %)]

  (->> out
       (map (fn [[x y]]
              (let [a      (s (nth l1 x))
                    b      (s (nth l2 y))
                    c      (s (reverse-complement a))
                    a-eq-b (= a b)
                    b-eq-c (= b c)]
                [x y a b c a-eq-b b-eq-c (or a-eq-b b-eq-c)])))
       #_(filter #(nth % 5)) ; exact match
       #_(filter #(nth % 6)) ; matches reverse-complement
       (filter #(not (last %))) ; not match?
       count
       pprint
       #_(cl-format "~{~{~a~^, ~}~}")))
