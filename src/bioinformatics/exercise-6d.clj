(ns bioinformatics.exercises)

(use 'clojure.pprint)

(defn reverse-complement [xs]
  (let [dna-complement {\A \T, \T \A, \C \G, \G \C}]
    (->> xs
         reverse
         (mapv dna-complement))))

(defn shared-kmers-problems-q [k s1 s2]
  (letfn [(gen-seq [genome f]
            (->> (partition k 1 genome)
                 (map-indexed (fn [i e] [(f e) i]))
                 (sort-by first)))
          (drop-elements [x xs]
            (drop-while (fn [[e _]] (= e x)) xs))
          (get-pairs [xs ys]
            (when-let [[a ai] (first xs)]
              (when-let [[b bi] (first ys)]
                (condp #(%1 %2 0) (compare a b)
                  <  (lazy-seq (get-pairs (rest xs) ys))
                  >  (lazy-seq (get-pairs xs (rest ys)))
                  =  (let [matches (for [[x xi] xs :while (= a x)
                                         [y yi] ys :while (= a y)]
                                     [xi yi])]
                       (concat matches
                               (lazy-seq (get-pairs (drop-elements a xs)
                                                    (drop-elements a ys)))))))))]
    (let [xs (gen-seq s1 vec)
          ys (gen-seq s2 vec)
          rs (gen-seq s2 (comp reverse-complement vec))]
      (->> (get-pairs xs ys)
           (map (fn [[x y]] [x 0 y]))
           (concat (->> (get-pairs xs rs)
                        (map (fn [[x y]] [x 1 y]))))
           sort
           (map (fn [[x _ z]] [x z]))))))

(defn shared-kmers-problems [k s1 s2]
  (let [iseq #(->> (partition k 1 %) (map-indexed vector))
        s1s  (iseq s1)
        s2m  (group-by last (iseq s2))
        s2rm (zipmap (map reverse-complement (keys s2m))
                     (vals s2m))]
    (for [[xi x] s1s
          [yi _] (concat (s2m x) (s2rm x))]
      [xi yi])))


(def path #(str (System/getProperty "user.home") "/Downloads/" %))
(def in-file (path "rosalind_6d.txt"))
(def out-file (path "rosalind_6d_output.txt"))

(let [txt  (clojure.string/split-lines (slurp in-file))
      k    (read-string (first txt))
      s1   (nth txt 1)
      s2   (nth txt 2)]

  (println "k=" k "genome1="(count s1) "genome2=" (count s2))

  (->> (time (doall (shared-kmers-problems k s1 s2)))
       (cl-format nil "~{(~{~a~^, ~})~%~}")
       (spit out-file)))

(let [txt  (clojure.string/split-lines (slurp in-file))
      k    (read-string (first txt))
      s1   (nth txt 1)
      s2   (nth txt 2)
      xs   (vec (partition k 1 s1))
      ys   (vec (partition k 1 s2))
      out  (->> (slurp out-file)
                (clojure.string/split-lines)
                (map read-string))
      s    #(apply str %)]

  (let [results (map (fn [[x y]]
                       (let [a      (s (nth xs x))
                             b      (s (nth ys y))
                             c      (s (reverse-complement a))
                             a-eq-b (= a b)
                             b-eq-c (= b c)]
                         [x y a b c a-eq-b b-eq-c (or a-eq-b b-eq-c)]))
                     out)
        exact-match  (filter #(nth % 5) results)
        revert-match (filter #(nth % 6) results)
        no-match     (filter #(not (last %)) results)
        ]
    (cl-format *out* "Exact matches: ~d~%Revert matches: ~d~%No match: ~d"
               (count exact-match)
               (count revert-match)
               (count no-match))))
