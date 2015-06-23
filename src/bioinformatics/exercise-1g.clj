(ns bioinformatics.exercises
  (:require [clojure.math.combinatorics :as combo]
            [clojure.core.reducers :as r]))

(use 'clojure.pprint)

(defn gen-seq [w replacements]
  (reduce (fn [s [i c]]
            (assoc s i c)) w replacements))

(defn mutations [s k d]
  (let [cbs (combo/combinations (range k) d)
        sls (combo/selections s d)
        rps (->> (combo/cartesian-product cbs sls)
                 (map (fn [[indices replacements]]
                        (map vector indices replacements))))]
    (fn [w]
      (let [w (vec w)]
        (->> (map #(gen-seq w %) rps)
             dedupe)))))

(defn freq-words-with-mismatches [text k d]
  (let [mutate-f  (mutations #{\A \C \G \T} k d)
        word-freq (->> (partition k 1 text)
                       frequencies
                       (into []))]
    (->> (r/fold 128
                 (fn ([] {})
                   ([a b] (merge-with + a b)))
                 (fn [s [w v]]
                   (merge-with + s (reduce (fn [m s] (assoc m s v)) {} (mutate-f w))))
                 word-freq)
         (group-by val)
         (apply max-key key)
         val
         (map key))))

;;;;;

(let [w1 "CACAGTAGGCGCCGGCACACACAGCCCCGGGCCCCGGGCCGCCCCGGGCCGGCGGCCGCCGGCGCCGGCACACCGGCACAGCCGTACCGGCACAGTAGTACCGGCCGGCCGGCACACCGGCACACCGGGTACACACCGGGGCGCACACACAGGCGGGCGCCGGGCCCCGGGCCGTACCGGGCCGCCGGCGGCCCACAGGCGCCGGCACAGTACCGGCACACACAGTAGCCCACACACAGGCGGGCGGTAGCCGGCGCACACACACACAGTAGGCGCACAGCCGCCCACACACACCGGCCGGCCGGCACAGGCGGGCGGGCGCACACACACCGGCACAGTAGTAGGCGGCCGGCGCACAGCC"
      ]
  (->> (time (freq-words-with-mismatches w1 10 2))
       (cl-format *out* "~{~{~a~}~%~}")))

(let [w1 "ACGTTGCATGTCGCATGATGCATGAGAGCT"]
  (->> (time (freq-words-with-mismatches w1 4 1))
       (cl-format *out* "~{~{~a~}~%~}")))

(let [w1 "TTTTTCAAGCAGGATGAGCAGGATGAATATCTCTCGTTCACCTGCGTTCACCTGTTTTTCAAGCAGGATGACGTTCACCTGCAGAAGCATTTTTCAAGCAGGATGATTTTTCAAATATCTCTTTTTTCAAGCAGGATGACAGAAGCACAGAAGCACGTTCACCTGCAGAAGCATTTTTCAAATATCTCTCAGAAGCATTTTTCAACGTTCACCTGCAGAAGCACGTTCACCTGGCAGGATGACGTTCACCTGATATCTCTTTTTTCAACAGAAGCAGCAGGATGAGCAGGATGACAGAAGCAATATCTCTATATCTCTCGTTCACCTGATATCTCTATATCTCTCAGAAGCATTTTTCAATTTTTCAACGTTCACCTGTTTTTCAAGCAGGATGAATATCTCTTTTTTCAATTTTTCAATTTTTCAACGTTCACCTGATATCTCTGCAGGATGAATATCTCTATATCTCTCGTTCACCTGTTTTTCAACAGAAGCAATATCTCTGCAGGATGAATATCTCTCAGAAGCAGCAGGATGATTTTTCAACAGAAGCAGCAGGATGACAGAAGCAATATCTCTTTTTTCAAATATCTCTCGTTCACCTGCAGAAGCACGTTCACCTGCAGAAGCATTTTTCAAGCAGGATGATTTTTCAAGCAGGATGATTTTTCAAGCAGGATGACAGAAGCAGCAGGATGAGCAGGATGATTTTTCAACGTTCACCTGATATCTCTATATCTCTGCAGGATGAATATCTCTATATCTCTTTTTTCAACAGAAGCAGCAGGATGACGTTCACCTGCGTTCACCTGATATCTCTCGTTCACCTGCGTTCACCTGCGTTCACCTGCAGAAGCACGTTCACCTGGCAGGATGAATATCTCTGCAGGATGACAGAAGCA"
      ]
  (time (freq-words-with-mismatches w1 5 2)))
