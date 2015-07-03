(ns bioinformatics.ex7c
  (:use [clojure.pprint]))

(def cx (comparator (fn [a b]
                      (let [ans (loop [xs a
                                       ys b]
                                  (if (or (empty? xs) (empty? ys))
                                    (compare (count xs) (count ys))
                                    (let [c (compare (first xs) (first ys))]
                                      (if (= c 0)
                                        (recur (rest xs) (rest ys))
                                        c))
                                    ))]
                        (> 0 ans)))))

(defn lcp [f x]
  (let [lcp* (fn [colls]
               (->> (apply map = colls)
                    (take-while true?)
                    count))])
  (->> (map f x)
       (partition 2 1)
       (map lcp*)))

(defn longest-repeat [s]
  (let [ss   (loop [t   (seq s)
                    ans (transient [])]
               (if (empty? t)
                 (persistent! ans)
                 (recur (rest t)
                        (conj! ans t))))
        sss   (->> (map-indexed vector ss)
                   (sort-by last cx))
        lsi   (map-indexed vector (lcp last sss))
        [i l] (->> (map-indexed vector (lcp last sss))
                    (apply max-key last))
        [_ t] (nth sss i)]
    (apply str (take l t))
    ))


(comment
  (def w1 "CTTTAAATTTTAGCGTTAGCTGTTTCGCATGATCTAGAATATGGAAAAATCAGCCTATACCTCGTGGACAGAGAATTCACTAATTCCTGGCATATCCTACGAGGCGATGAAACCGTGAGTTGGATACGTTGAATAAAGCAGCATAGGTTGTACGACAGACCCGGGGGCACGTCCGCTCTCAACCAGCTTCATAAATGGAATCCTAACACTCCTCCATAAGGATTGATCATTATGACTATTGGTTTATTGCGACTTTTAAGCGACGCAAGCGCTCAAGAGTTAGGTCATTTAGTTCCAATTGCGCAAGATTACCCGCAATCGCCTATCAGCTTCGGGAAGAGATTCGACAAGGCTCCCCAAAACGCAAACTCATGAAGTGTCGTACGCTGGGAAACAGGATTGTTCACAGTGCTGTAGGAGAGGGCGACCTCCGAAGGCGCCGCACCGGCAAAGCCGCTGCCGCTTTACACTGGGTCCCCTGGAGCAGTGAAGAGGTGGAGGAAGAGGGGAAGAACCTAGTCCTGTGACAAGGATATGGGGTACGTAGCGACACACGTTAGCCACCGGGACCTATCTTAGCCATGTTCATCTTACACACCTGAGCGACAGACCCGGGGGCACGTCCGCTCTCAACCAGCTTCATAAATGGAATCCTAACACTCCTCCATAAGGATTGATCATTATGACTATTGGTTTACGCCTGTATGCACATTGCAATCTAGCAAGGTCGGGCTTTCGATAGTAGTTAATACCACCCCCTGAGGATGTAATCGAGTGCGTGGGCTTTCTGCTTATTCCCAGGATGTTATGTTACCGTGGGTATGATTCGACTGTCTTCTCACGGCGCGTGGTTGAAGTGTAGACCTGACTACACAGAACCCTAGGGGCCTCATGTAGGTACCTATTTCGAGGTTCTACACCTTATCCGTACCGTTTTTACAGTCTGCCTGATGTGCGATTCAGAGATGTTCACACGTCGTCGGGGGAAATGAGCTCCGTGAGTTTGAGGCCGAGTCGACAGACCCGGGGGCACGTCCGCTCTCAACCAGCTTCATAAATGGAATCCTAACACTCCTCCATAAGGATTGATCATTATGACTATTGGTTTATACGTCGCCGCTATGGACCATCGCTTTGTGGCTCGAACCTGCACCTCATTGAATTGCCAGGTGACGGGGTTCCTACATTTAAACACAGATGTGAGCGGAAGGCGTACTTCTAGGTGACACTACCCCTTATTAGAACCGCCAGCCCTAAGAGAACCGTGCCCACTGTGCGTAGT")
  (time (longest-repeat w1)))
