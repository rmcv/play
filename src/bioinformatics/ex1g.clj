(ns bioinformatics.ex1g
  (:require [clojure.math.combinatorics :as combo]
            [clojure.core.reducers :as r]))

(use 'clojure.pprint)
(use 'criterium.core)

(defn reverse-complement [xs]
  (let [dna-complement {\A \T, \T \A, \C \G, \G \C}]
    (->> xs
         reverse
         (mapv dna-complement))))

(defn gen-seq [w replacements]
  (reduce (fn [s [i c]] (assoc s i c)) w replacements))

(defn mutations [s k d]
  (let [cbs (combo/combinations (range k) d)
        sls (combo/selections s d)
        rps (->> (combo/cartesian-product cbs sls)
                 (map #(apply map vector %)))]
    (fn [w]
      (let [w  (vec w)
            xf (comp (map #(gen-seq w %)) (dedupe))]
        (sequence xf rps)))))

(defn freq-words-with-mismatches [text k d]
  (let [mutate-f  (mutations #{\A \C \G \T} k d)
        word-freq (->> (partition k 1 text)
                       frequencies
                       (into []))]
    ;; using r/fold aka java join/fork
    (->> (r/fold 128
                 (fn ([] {})
                   ([a b] (merge-with + a b)))
                 (fn [s [w v]]
                   (merge-with + s (zipmap (mutate-f w) (repeat v))))
                 word-freq)
         (group-by val)
         (apply max-key key)
         val
         (map key))))


(defn pattern-match [pattern genome]
  (let [pattern (vec pattern)]
    (->> (partition (count pattern) 1 genome)
         (keep-indexed #(if (= %2 pattern) %1)))))

(comment (->> (pattern-match "ATAT" "GATATATGCATATACTT")
              (cl-format *out* "~%~{~a~^ ~}")))

(defn skew [genome]
  (letfn [(skew-seq [xs gcnt ccnt]
            (let [v  (- gcnt ccnt)
                  sf #(cons v (lazy-seq (skew-seq (rest xs) %1 %2)))]
              (if-let [x (first xs)]
                (case x
                  \G  (sf (inc gcnt) ccnt)
                  \C  (sf gcnt (inc ccnt))
                  (sf gcnt ccnt))
                [v])))]
    (skew-seq genome 0 0)))

(defn min-skew [genome]
  (let [ss  (skew genome)
        si  (map vector ss (range))
        ms  (apply min ss)]
    (println ms)
    (->> si
         (filter #(= ms (first %)))
         (map last))))

(skew "CATGGGCATCGGCCATACGCC")

(defn cnt-seq [pred xs]
  (letfn [(count-pred [xs cnt]
            (if-let [x (first xs)]
              (cons cnt (lazy-seq (count-pred
                                   (rest xs)
                                   (if (pred x) (inc cnt) cnt))))
              [cnt]))]
    (count-pred xs 0)))

(defn skew [genome]
  (map -
       (cnt-seq #(= \G %) genome)
       (cnt-seq #(= \C %) genome)))

(defn min-skew [genome]
  (let [ss (skew genome)
        m  (apply min ss)]
    (keep-indexed #(if (= %2 m) %1) ss)))


(comment
  ;; using pmap to do multithread
  (defn freq-words-with-mismatches [text k d]
    (let [mutate-f    (mutations #{\A\C\G\T} k d)
          word-freq   (->> (partition k 1 text)
                           frequencies
                           (into []))
          r-word-freq (mapv (fn [[k v]] [(reverse-complement k) v])
                            word-freq)]
      (->> (concat word-freq r-word-freq)
           (pmap (fn [[w v]] (zipmap (mutate-f w) (repeat v))))
           (apply merge-with +)
           (group-by val)
           (apply max-key key)
           val
           (map key))))

  (time (reduce + (cnt-seq #(= 3 %) (repeatedly 10000000 #(rand-int 10)))))

  (time (reduce (fn [s e] (if (= 3 e) (+ s e) s)) (repeatedly 10000000 #(rand-int 10))))
;;;;;

  (let [w1 "CTGCGTGACCTGCGTGACTCAGATGCAGCTGCGTGACCTGCGTGACATGGTAGAATGGTAGACGCGTCGACCCGCGTCGACCTCAGATGCAGTCAGATGCAGCGAAATTCAATGGTAGATCAGATGCAGTCAGATGCAGCGAAATTCACGAAATTCATCAGATGCAGTCAGATGCAGCGCGTCGACCCTGCGTGACTCAGATGCAGCTGCGTGACATGGTAGATCAGATGCAGCGCGTCGACCCTGCGTGACCGAAATTCACGCGTCGACCCGCGTCGACCTCAGATGCAGCGAAATTCACTGCGTGACATGGTAGATCAGATGCAGCTGCGTGACATGGTAGATCAGATGCAGATGGTAGAATGGTAGACGCGTCGACCCGCGTCGACCCTGCGTGACCTGCGTGACCTGCGTGACATGGTAGACTGCGTGACCGCGTCGACCCGAAATTCAATGGTAGAATGGTAGATCAGATGCAGCGCGTCGACCCGCGTCGACCATGGTAGACTGCGTGACCGAAATTCAATGGTAGAATGGTAGACGAAATTCACTGCGTGACATGGTAGACGAAATTCACTGCGTGACCGCGTCGACCTCAGATGCAGTCAGATGCAGCGCGTCGACCCTGCGTGACCTGCGTGACCGAAATTCACGAAATTCAATGGTAGACGAAATTCATCAGATGCAGCTGCGTGACATGGTAGACGAAATTCATCAGATGCAGATGGTAGACGCGTCGACCCGCGTCGACCCGAAATTCACTGCGTGACCGAAATTCACTGCGTGACCTGCGTGACCGCGTCGACCCGCGTCGACCATGGTAGACGAAATTCACGCGTCGACCTCAGATGCAGCGAAATTCATCAGATGCAGCGCGTCGACCCGCGTCGACCATGGTAGACGAAATTCA"
        ]
    (->> (time (freq-words-with-mismatches w1 7 2))
         (cl-format *out* "~{~{~a~}~%~}")))

  (let [w1 "CACAGTAGGCGCCGGCACACACAGCCCCGGGCCCCGGGCCGCCCCGGGCCGGCGGCCGCCGGCGCCGGCACACCGGCACAGCCGTACCGGCACAGTAGTACCGGCCGGCCGGCACACCGGCACACCGGGTACACACCGGGGCGCACACACAGGCGGGCGCCGGGCCCCGGGCCGTACCGGGCCGCCGGCGGCCCACAGGCGCCGGCACAGTACCGGCACACACAGTAGCCCACACACAGGCGGGCGGTAGCCGGCGCACACACACACAGTAGGCGCACAGCCGCCCACACACACCGGCCGGCCGGCACAGGCGGGCGGGCGCACACACACCGGCACAGTAGTAGGCGGCCGGCGCACAGCC"
        ]
    (->> (bench (freq-words-with-mismatches w1 12 2))
         (cl-format *out* "~{~{~a~}~%~}")))

  (let [w1 "ACGTTGCATGTCGCATGATGCATGAGAGCT"]
    (->> (time (freq-words-with-mismatches w1 3 1))
         (cl-format *out* "~{~{~a~}~%~}")))

  (let [w1 "TTTTTCAAGCAGGATGAGCAGGATGAATATCTCTCGTTCACCTGCGTTCACCTGTTTTTCAAGCAGGATGACGTTCACCTGCAGAAGCATTTTTCAAGCAGGATGATTTTTCAAATATCTCTTTTTTCAAGCAGGATGACAGAAGCACAGAAGCACGTTCACCTGCAGAAGCATTTTTCAAATATCTCTCAGAAGCATTTTTCAACGTTCACCTGCAGAAGCACGTTCACCTGGCAGGATGACGTTCACCTGATATCTCTTTTTTCAACAGAAGCAGCAGGATGAGCAGGATGACAGAAGCAATATCTCTATATCTCTCGTTCACCTGATATCTCTATATCTCTCAGAAGCATTTTTCAATTTTTCAACGTTCACCTGTTTTTCAAGCAGGATGAATATCTCTTTTTTCAATTTTTCAATTTTTCAACGTTCACCTGATATCTCTGCAGGATGAATATCTCTATATCTCTCGTTCACCTGTTTTTCAACAGAAGCAATATCTCTGCAGGATGAATATCTCTCAGAAGCAGCAGGATGATTTTTCAACAGAAGCAGCAGGATGACAGAAGCAATATCTCTTTTTTCAAATATCTCTCGTTCACCTGCAGAAGCACGTTCACCTGCAGAAGCATTTTTCAAGCAGGATGATTTTTCAAGCAGGATGATTTTTCAAGCAGGATGACAGAAGCAGCAGGATGAGCAGGATGATTTTTCAACGTTCACCTGATATCTCTATATCTCTGCAGGATGAATATCTCTATATCTCTTTTTTCAACAGAAGCAGCAGGATGACGTTCACCTGCGTTCACCTGATATCTCTCGTTCACCTGCGTTCACCTGCGTTCACCTGCAGAAGCACGTTCACCTGGCAGGATGAATATCTCTGCAGGATGACAGAAGCA"
        ]
    (bench (freq-words-with-mismatches w1 12 2)))

  (let [w1 "AAAACGAAAGTATTCATGTATTCATTATATTCCTGTATTCATGTATTCATGTATTCATAAAACGAAAAAAACGAAAAAAACGAAAGGGTGCTCGTATTCATAAAACGAAAGGGTGCTCGTATTCATGTATTCATGGCCATAGGGGTGCTCAAAACGAAAGGCCATAGGGCCATAGGGGTGCTCTATATTCCTGTATTCATGGGTGCTCGTATTCATAAAACGAAATATATTCCTGGCCATAGGTATTCATAAAACGAAATATATTCCTTATATTCCTGGGTGCTCAAAACGAAATATATTCCTGTATTCATGGGTGCTCTATATTCCTAAAACGAAATATATTCCTGTATTCATGGGTGCTCTATATTCCTGTATTCATGGCCATAGGTATTCATGTATTCATTATATTCCTGTATTCATGGGTGCTCAAAACGAAAAAAACGAAAGGCCATAGTATATTCCTGTATTCATGTATTCATGGGTGCTCAAAACGAAAAAAACGAAAAAAACGAAAGGGTGCTCGGCCATAGGTATTCATTATATTCCTGTATTCATTATATTCCTGGCCATAGGTATTCATGTATTCATTATATTCCTGTATTCATGGCCATAGAAAACGAAAGTATTCATGGGTGCTCAAAACGAAAAAAACGAAATATATTCCTGTATTCATGGGTGCTCAAAACGAAAAAAACGAAATATATTCCTGGGTGCTCGTATTCATTATATTCCTGTATTCATTATATTCCTGTATTCATGTATTCATAAAACGAAAGGCCATAGGTATTCATAAAACGAAAAAAACGAAAGGGTGCTCGGGTGCTCGTATTCAT"
        ]
    (->> (time (freq-words-with-mismatches w1 10 2))
         (cl-format *out* "~{~{~a~}~%~}"))))
