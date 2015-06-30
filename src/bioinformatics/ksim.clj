(ns bioinformatics.ksim)

(use 'clojure.pprint)

(defn- cnt-seq [pred xs]
  (letfn [(count-pred [xs cnt]
            (if (empty? xs)
              ()
              (let [x (first xs)
                    c (if (pred x) (inc cnt) cnt)]
                (cons c (lazy-seq (count-pred (rest xs) c))))))]
    (count-pred xs 0)))

(defn match-up-to [d motif genome]
  (let [mlen  (count motif)
        min   (dec (- mlen d))
        gs    (->> (partition mlen 1 genome)
                   (map-indexed vector))]
    (for [[i g] gs
          :let [m  (->> (map = g motif)
                        (cnt-seq false?))]
          :while (<= (last m) d)
          :let [mi (map-indexed vector m)]
          [i2 c] mi
          :when (>= i2 min)]
      (list (inc i) (inc i2)))))


(comment (->> (match-up-to 2 "ACGTAG" "ACGGATCGGCATCGT")
              (cl-format *out* "~{~{~a~^ ~}~%~}"))

         (let [txt   (->> (slurp (str (System/getProperty "user.home") "/Downloads/a.txt"))
                          clojure.string/split-lines)
               k     (read-string (first txt))
               motif (nth txt 1)
               genome (last txt)]
           (->> (match-up-to k motif genome)
                (cl-format *out* "~{~{~a~^ ~}~%~}")))


         (defn count-n [s]
           (->> s
                frequencies
                (into (sorted-map))
                (map val)
                (cl-format *out* "~{~a~^ ~}")))

         (count-n "GAATGTGACCTGCACGCATTGACCCCAGGCACAGCTTCTTATCCCTTATGGTCCTCACTCACCAGCATGAACATGATACCAGACGAATGTCCTCGGACAGCAGCGTAATAGGCGCGTGGGTATCTGGCTGAGTAACTCTAACGTAGCGACTAATGTCTTAGCGCTAAAGTGACCTTTCTGCCATTACTTTACGGGCTTAACGTACGGGGAGTTTACGCACCCCCACTGACCCATGGGTGGCTGGGCGTATTGCAGTGCTTCGTAATCAATTGTATTCCAAGGTTCCCCTTACGCGTTCTGGTTCGTCTCGGGCTTATGCTGGATAGTGCTGGTATATTCAGATCGACTAACGAATAGCTTCTCGCACGACTGACGTGTGACCGACCCCAGAGTTAGTCCGATGGCACAATATTCCTGTCGTGTTCTGCCAGGCGTTATAGCCGGAGGGGATGCAGTGGGTCCTCAAGTTTCAGGGGAACGTGCGTGAGAAAAGCGTTCAGTCAACGATTGTAATAAAACAAGGGGCCTACCCAGACTATTCCTAGAATGGTATTGGGTAGGCCATGAGCGGACAACGGTAGTACTACTCGATCACGTGTGGTTGCTGGGACAGCCAATCGGGTTGGCGCTAGGATCACGCGATTACCGGGTACTAGTAGGAAGCACAGGACATTCACATCCCTGGGTAACGGGACTGCTCTGACTGACGAGAAAAATTTGAGCTAGCAGAAAAGTATGGGGAAGAAGATGGAGGCGCCGGACCAGTGTGTAGCTCAACCTATATGCCCCACGATCACACTTCATGTACTCTATT")

         (defn rna [s]
           (->> s
                (map {\A \A \C \C \G \G \T \U})
                (cl-format *out* "~{~a~}")))

         (rna "TAATCTAACGTTGATTAGACGACTTGGCGGGGCCCGGTTGGGAGCAAGGACGGACGTATCTTACTTCTCTCTCTCATAAATAAGTCCAGCAACGAGAAAGTCCTTGCCCTAATCTGGCATTAAATCAGGTGCTACGCTATATCCTAACTAGTCTCGTCCTTTGTAGTGCGCAGATTTGCTGTCGAAATACTTACATCAGCAGGATTTCTGGTCCGAACCCGTAGCATATTTGCTTTCAAGAACATTACCCGATATGCAAGAGATCGCGACAAGCTGTTATGTAAGGTATAGTACCAGCCGCTAGTCAGAAACCTAGTGGTGCGAAAACGGCCTAACGCCGAAGTAAAGAGTGATAAATGAGGCCGGGTGGCATATACTACGAGCGACCCCCCCGGCTGACAGTACCTCCCCCTAGCCCATTCGATCCACAATGACAGCTGGCGTAATTATTTGAGCTAGGGATGTCGTTCTGATTGCGCAAAATACAAGATAAGAGATATTATTAACTCGTCAAATGCTTTGATCTTTTCATGGAGTCTAGGTGGGCCTAGGTCGTACGTGTCCGACGCTCGGGTGTTATCCAGGGAGGCATAGCAAAATGCCTTCTGGTTTTCCACAACGGGCGTACAGGGGTATCGTTGTAAAGCGCTCACAAGCCCAGACGATGCGGTGAGCCGATTTTTGTTGTTACTAGGTCCCCGTTGGTCTACCTACCAGCGCACTCCGAAAAACCCGGACCAGCGGCGCAGCCACTCTCTGCGCGGGGCATATGTTAAGTATGGTGCCTCCCACTTGACCGGCTCCGGCACCGTCAACGGATTTACATAGGGCATGTTCCCGTCAATGCGCGCTCTATCGAGCATCCAGTAATCCCGAGATTGCGGAATCAATCTCGACGTCCAACAACTTCCGGGGATCTGTGTGGACCACCTTAGCGCGAACGGGCCTTATATTGGGAGCGTCGATTTATATCTGTGCACCGTCTGGCGTGTAAACA")


         (->> (reverse-complement "ATCGTCTGCCGGAAAACTGCTGGTGCTCTACCAATAGAGGCACAATGTAGTCTATAATAACGCCAACAAAATCGGGTGCACATATTTATTTAACAGACATGATATACAGGTCCGTATCTATCACTTGAGTTAAGATATGAAAACCGGCGTAACGGCGGAGGACTGAGATGATAGCATTCCACCGCGTTAAAGACTCCTAGGATACAGAGTTCGTCGGGCCAACGAACAGGCACTTCCCCGGCGGGTATGTTCAGACCCAGATTAAACAACCAATTTTTTACCTATTAAGATCTACGATGGGTCTCTCCGCTGCCCAATCACGTGGCGAGTTGAGTTTGTAGTCTCCGCACCCTTCACCGTCAAATTCTGCGAGCGTGTGTCTGCTCGCTGCATCCTGACTATTGTTCACGCCGACACTCGCCACGCTAAAAAGTGGACTTCCACTAGTATCGTAGATAGAACAACAAGGCAATCAACGTTGCACAATCTCTGGGTGTTGGGCAAAGGTAGCGGGTTGAGCTTGCGAGTGACAAATTATCGTCCTAACCCGTCGGTGGCCATAATCCTGCCGCCGGTATAGAGACTCCGGCTACGTGGCCCGGTTCTATCTATGCCTAGATTTTCGAGAACCTAAAACCTCAGGGTGGCTAATGTATTTTGACCAAAATGGCACCGTTGTTAGGTCTACGTCGGGCTATAAGAAGAGATTTGCCTGCGGCATACGCAATCCGGCGAACCTTCAGGGAGGGTCCACGCGACTCCCTGCTACTGGAAATACCACTTGAGTACGTTGCAAACGCTAACCGCGCAGCATTTTAGATTACATGGTTGATCATTCAAGCGCGTTGTTTCGGGCCGGGAGGTAGGGGGACATTCTAAAGCGTTCCGGCGCAACTCCCGCTTACCTACTACCGTGGCCCAAAGATAGCGACAGATACATCATTCCTGTTAGTTCCATACCCTTGCGGCTCCACCTGG")
              (cl-format *out* "~{~a~}"))

         (defn find-motif [g m]
           (let [m (vec m)]
             (->> (partition (count m) 1 g)
                  (map-indexed vector)
                  (filter (fn [[_ c]] (= c m)))
                  (map (fn [[i _]] (inc i)))
                  (cl-format *out* "~{~a~^ ~}"))))

         (find-motif "GCTCCTCGCACTTGCGTACACTTGCGTCTTGCGTCTTGCGTAACCTTGCGTCGGTGTCTTGCGTAGCTTGCGTCTTGCGTAGGGCTTGCGTCTTGCGTCTTGCGTCTTGCGTCTCTCTCTTGCGTCTTGCGTTCTTGCGTAACTTGCGTCTTGCGTTCTTGCGTGTCCGCTTGCGTATCTTGCGTCTTGCGTGCTGTAGATCTTGCGTCCTTGCGTCTTGCGTCGCTTGCGTCTTGCGTCTTGCGTCTTGCGTTCTCTTGCGTGACTTGCGTCTTGCGTCTTGCGTCGATCTTGCGTAAGCCTTGCGTGATTCTGCTCTTGCGTCACCTTGCGTACTTGCGTTCTTGCGTTCTTGCGTGTCTTGCGTATTCTTGCGTTCTTGCGTTCTTGCGTGCTTGCGTAGCTTGCGTCCCCTTGCGTGCTTGCGTCTTGCGTATCCTTGCGTACAACTCTCTTGCGTCTTGCGTGTCTTGCGTACTTGCGTATTCTTGCGTCTTGCGTTTAATCTTGCGTAACGCTTGCGTACCTTGCGTGCCTTGCGTAGACTTGCGTCTACCTTGCGTCCTTGCGTCTTGCGTGGTCACTTGCGTCGCTTGCGTACTTGCGTCTTGCGTTACTTGCGTCTTGCGTTCCTTGCGTCAGCTCAAAACTTGCGTGCCTTGCGTTTGCGGCCTTGCGTAGGTAGAGTCTTGCGTGACTTTAGGCTTCTTGCGTTGCACGGACTTGCGTTGCACTTGCGTTCTTGCGTGACTTGCGTCATCTTGCGTTGCCTTGCGTAACCTTGCGTATATCTTGCGTAGTGGGCACCTTGCGTCTTGCGTTCCGCCACTTGCGT" ""))
