(ns bioinformatics.nwa
  (:require [clojure.string :as str])
  (:use [bioinformatics.utils]
        [clojure.pprint]
        [criterium.core]))


(def BLOSUM62
  "A  C  D  E  F  G  H  I  K  L  M  N  P  Q  R  S  T  V  W  Y
A  4  0 -2 -1 -2  0 -2 -1 -1 -1 -1 -2 -1 -1 -1  1  0  0 -3 -2
C  0  9 -3 -4 -2 -3 -3 -1 -3 -1 -1 -3 -3 -3 -3 -1 -1 -1 -2 -2
D -2 -3  6  2 -3 -1 -1 -3 -1 -4 -3  1 -1  0 -2  0 -1 -3 -4 -3
E -1 -4  2  5 -3 -2  0 -3  1 -3 -2  0 -1  2  0  0 -1 -2 -3 -2
F -2 -2 -3 -3  6 -3 -1  0 -3  0  0 -3 -4 -3 -3 -2 -2 -1  1  3
G  0 -3 -1 -2 -3  6 -2 -4 -2 -4 -3  0 -2 -2 -2  0 -2 -3 -2 -3
H -2 -3 -1  0 -1 -2  8 -3 -1 -3 -2  1 -2  0  0 -1 -2 -3 -2  2
I -1 -1 -3 -3  0 -4 -3  4 -3  2  1 -3 -3 -3 -3 -2 -1  3 -3 -1
K -1 -3 -1  1 -3 -2 -1 -3  5 -2 -1  0 -1  1  2  0 -1 -2 -3 -2
L -1 -1 -4 -3  0 -4 -3  2 -2  4  2 -3 -3 -2 -2 -2 -1  1 -2 -1
M -1 -1 -3 -2  0 -3 -2  1 -1  2  5 -2 -2  0 -1 -1 -1  1 -1 -1
N -2 -3  1  0 -3  0  1 -3  0 -3 -2  6 -2  0  0  1  0 -3 -4 -2
P -1 -3 -1 -1 -4 -2 -2 -3 -1 -3 -2 -2  7 -1 -2 -1 -1 -2 -4 -3
Q -1 -3  0  2 -3 -2  0 -3  1 -2  0  0 -1  5  1  0 -1 -2 -2 -1
R -1 -3 -2  0 -3 -2  0 -3  2 -2 -1  0 -2  1  5 -1 -1 -3 -3 -2
S  1 -1  0  0 -2  0 -1 -2  0 -2 -1  1 -1  0 -1  4  1 -2 -3 -2
T  0 -1 -1 -1 -2 -2 -2 -1 -1 -1 -1  0 -1 -1 -1  1  5  0 -2 -2
V  0 -1 -3 -2 -1 -3 -3  3 -2  1  1 -3 -2 -2 -3 -2  0  4 -3 -1
W -3 -2 -4 -3  1 -2 -2 -3 -3 -2 -1 -4 -4 -2 -3 -3 -2 -3 11  2
Y -2 -2 -3 -2  3 -3  2 -1 -2 -1 -1 -2 -3 -1 -2 -2 -2 -1  2  7"
)


(defn sub-score* [matrix]
  (let [[h & d] (str/split-lines matrix)
        cs      (zipmap (map first (str/split h #"  +"))
                        (range))
        dm      (mapv (fn [l] (->> (str/split l #" +")
                                  (drop 1)
                                  (mapv read-string))) d)
        ]
    (fn [a b]
      (let [i (cs a)
            j (cs b)]
        (nth (nth dm i) j))))
  )

(def sub-score (sub-score* BLOSUM62))

(def gap -5)

(defrecord Res [score source])

(defn score  [x y i j]
  (let [top?  (= i 0)
        left? (= j 0)]
    (if (or top? left?)
      (let [s (* gap (+ i j))]
        (Res. s (cond
                  (and top? left?) :done
                  top?             :l
                  left?            :t)))
      (let [{d :score}  (score x y (dec i) (dec j))
            {t :score}  (score x y (dec i) j)
            {l :score}  (score x y i (dec j))
            dv          (+ d (sub-score (nth y i) (nth x j)))
            tv          (+ t gap)
            lv          (+ l gap)
            m           (max dv tv lv)
            source      (condp = m
                          dv :d
                          tv :t
                          lv :l)
            ]
        (Res. m source)))))


(defn track-back [x y path]
  (let [xlen (count x)
        ylen (count y)]
    (reduce (fn [[s i j] e]
              (if (= :done e)
                s
                (condp = e
                  :l [(cons [(nth x j) nil] s) i (dec j)]
                  :t [(cons [nil (nth y i)] s) (dec i) j]
                  :d [(cons [(nth x j) (nth y i)] s) (dec i) (dec j)])))
            [[] (dec ylen) (dec xlen)]
            path)))

(defn global-align [x y]
  (let [x (vec (cons \$ x))
        y (vec (cons \$ y))]
    (with-redefs [score (memoize score)]
      (let [ans (last (for [i (range (count y))
                            j (range (count x))]
                        (score  x y i j)))]
        (-> ans
            #_(update 1 #(track-back x y %)))))))

(let [[input output] (io "rosalind_5e")
      [a b] input
      a (take 4000 a)
      b (take 500 b)
      r (time (global-align a b))
      ;pf    #(cl-format nil "~{~a~}" (map (fn [x] (if x x \-)) %))
      ;s1    (pf (map first path))
      ;s2    (pf (map last path))
      ;s     (str val "\n" s1 "\n" s2)
      ]
  r
  #_(output s))

(time (global-align "SEND" "AND"))

(time (global-align "PLEASANTLY" "MEANLY"))
