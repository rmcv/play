(ns genome.core
  (:require [clojure.math.combinatorics :as combo]
            [clojure.core.reducers :as r]))

(use 'clojure.pprint)

(defn gen-seq [w replacements]
  (reduce (fn [w [i c]]
            (assoc w i c)) w replacements))

(defn mutations [s k d]
  (let [cbs (combo/combinations (range k) d)
        sls (combo/selections s d)
        rps (->> (combo/cartesian-product cbs sls)
                 (map (fn [[indices replacements]]
                        (map vector indices replacements))))]
    (fn [w]
      (let [w (vec w)]
        (->> (map #(gen-seq w %) rps)
             (into #{}))))))

(defn freq-words-with-mismatches [text k d]
  (let [mf  (mutations #{\A\C\G\T} k d)]
    (->> (partition k 1 text)
         frequencies
         (map (fn [[w v]]
                (persistent!
                 (reduce (fn [m s] (assoc! m s v)) (transient {}) (mf w)))))
         #_(map (fn [[w v]]
                (reduce (fn [m s] (assoc m s v)) {} (mf w))))
         (apply merge-with +)
         (group-by val)
         (apply max-key key)
         val
         (map key)

         )))

(time (r/fold + ((map inc) +) (vec (range 1000000))))

;;;;;

(let [w1 "CACAGTAGGCGCCGGCACACACAGCCCCGGGCCCCGGGCCGCCCCGGGCCGGCGGCCGCCGGCGCCGGCACACCGGCACAGCCGTACCGGCACAGTAGTACCGGCCGGCCGGCACACCGGCACACCGGGTACACACCGGGGCGCACACACAGGCGGGCGCCGGGCCCCGGGCCGTACCGGGCCGCCGGCGGCCCACAGGCGCCGGCACAGTACCGGCACACACAGTAGCCCACACACAGGCGGGCGGTAGCCGGCGCACACACACACAGTAGGCGCACAGCCGCCCACACACACCGGCCGGCCGGCACAGGCGGGCGGGCGCACACACACCGGCACAGTAGTAGGCGGCCGGCGCACAGCC"
      g1 "GCACACAGAC"
      g2 "GCGCACACAC"]
  (->> (time (freq-words-with-mismatches w1 10 3))
       (cl-format *out* "~{~{~a~}~%~}")))

(let [w1 "ACGTTGCATGTCGCATGATGCATGAGAGCT"
      g1 "ATGT"
      g2 "ATGC"
      g3 "CATG"
      g4 "GCAT"]
  (time (freq-words-with-mismatches w1 4 1)))

(let [w1 "TTTTTCAAGCAGGATGAGCAGGATGAATATCTCTCGTTCACCTGCGTTCACCTGTTTTTCAAGCAGGATGACGTTCACCTGCAGAAGCATTTTTCAAGCAGGATGATTTTTCAAATATCTCTTTTTTCAAGCAGGATGACAGAAGCACAGAAGCACGTTCACCTGCAGAAGCATTTTTCAAATATCTCTCAGAAGCATTTTTCAACGTTCACCTGCAGAAGCACGTTCACCTGGCAGGATGACGTTCACCTGATATCTCTTTTTTCAACAGAAGCAGCAGGATGAGCAGGATGACAGAAGCAATATCTCTATATCTCTCGTTCACCTGATATCTCTATATCTCTCAGAAGCATTTTTCAATTTTTCAACGTTCACCTGTTTTTCAAGCAGGATGAATATCTCTTTTTTCAATTTTTCAATTTTTCAACGTTCACCTGATATCTCTGCAGGATGAATATCTCTATATCTCTCGTTCACCTGTTTTTCAACAGAAGCAATATCTCTGCAGGATGAATATCTCTCAGAAGCAGCAGGATGATTTTTCAACAGAAGCAGCAGGATGACAGAAGCAATATCTCTTTTTTCAAATATCTCTCGTTCACCTGCAGAAGCACGTTCACCTGCAGAAGCATTTTTCAAGCAGGATGATTTTTCAAGCAGGATGATTTTTCAAGCAGGATGACAGAAGCAGCAGGATGAGCAGGATGATTTTTCAACGTTCACCTGATATCTCTATATCTCTGCAGGATGAATATCTCTATATCTCTTTTTTCAACAGAAGCAGCAGGATGACGTTCACCTGCGTTCACCTGATATCTCTCGTTCACCTGCGTTCACCTGCGTTCACCTGCAGAAGCACGTTCACCTGGCAGGATGAATATCTCTGCAGGATGACAGAAGCA"
      g1 "ATGT"
      g2 "ATGC"
      g3 "GATG"]
  (time (freq-words-with-mismatches w1 5 2)))
