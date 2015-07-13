(ns bioinformatics.nwa
  (:require [clojure.string :as str]
            ;[clojure.core.matrix :refer :all]
            [clatrix.core :refer [mget mset! matrix] :as cm])
  (:use [bioinformatics.utils]
        [clojure.pprint]
        [criterium.core]
        ))

(set! *warn-on-reflection* true)

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

(set-current-implementation :jblaz)

(defn sub-score* [m]
  (let [[h & d] (str/split-lines m)
        cs      (zipmap (map first (str/split h #"  +"))
                        (range))
        dm      (matrix (mapv (fn [l] (->> (str/split l #" +")
                                          (drop 1)
                                          (mapv read-string))) d))
        ]
    (fn [a b]
      (let [i (cs a)
            j (cs b)]
        (mget dm i j))))
  )

(def sub-score (sub-score* BLOSUM62))

(def gap -5)

(defn global-align [a b]
  (let [a (vec (cons \$ a))
        b (vec (cons \$ b))
        cols (count a)
        rows (count b)
        sa  (matrix (make-array Integer/TYPE rows cols))
        da  (matrix (make-array Integer/TYPE rows cols))
        gm (volatile! Integer/MIN_VALUE)
        gi (volatile! 0)
        gj (volatile! 0)]
    (dotimes [i rows]
      (mset!  da i 0 1)   ; no-b
      (mset!  sa i 0 (* i gap)))
    (dotimes [j cols]
      (mset!  da 0 j 2)   ; :no-a
      (mset!  sa 0 j (* j gap)))
    (mset! da 0 0 0)

    (dotimes [i (dec rows)]
      (let [i  (inc i)
            bi (nth b i)]
        (dotimes [j (dec cols)]
          (let [j  (inc j)
                aj (nth a j)
                sc (int (sub-score bi aj))
                d  (+ sc  (mget sa (dec i) (dec j)))
                t  (+ gap (mget sa (dec i) j))
                l  (+ gap (mget sa i (dec j)))
                m  (max d t l)]
            (if (> m @gm)
              (do
                (vreset! gm m)
                (vreset! gi i)
                (vreset! gj j)))
            (mset! sa i j m)
            (mset! da i j (condp = m
                           d 3 ;:a-b
                           t 2 ;:no-a
                           l 1 ;:no-b
                           ))))))
    {:score sa
     :path  da
     :max   {:score @gm
             :i     @gi
             :j     @gj}}))


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


(let [[input output] (io "rosalind_5e")
      [a b] input
;      a (take 4000 a)
;      b (take 4000 b)
      r (time (global-align a b))
      ;pf    #(cl-format nil "~{~a~}" (map (fn [x] (if x x \-)) %))
      ;s1    (pf (map first path))
      ;s2    (pf (map last path))
      ;s     (str val "\n" s1 "\n" s2)
      ]
  (println r)
  #_(output s))

(defn print-matrix [[sa da]]
  (cl-format *out* "~{~{~4d~}~%~}" sa)
  (cl-format *out* "~{~{~7a~}~%~}" da))

(set! *print-length* 3)
(->> (time (global-align "SEND" "AND"))
     #_(print-matrix))


(->> (time (global-align "PLEASANTLY" "MEANLY"))
     #_print-matrix)


(def mm (matrix (make-array Integer/TYPE 30 30)))
(mset mm 1 2 -3)
