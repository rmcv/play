(ns bioinformatics.utils
  (:require [clojure.core.async :as async :refer [chan <!! >!! >! <! timeout go go-loop close! dropping-buffer]]))

(defn io [fname]
  (let [fpath (str (System/getProperty "user.home") "/Downloads/" fname)
        inp (->> (slurp (str fpath ".txt"))
                 (clojure.string/split-lines))
        out (fn [x] (spit (str fpath "_out.txt") x))]
    [inp out]))

(defn partition-by-set
  "Usage: "
  [sel]
  (fn [step]
     (let [s  (volatile! #{})
           b  (volatile! [])]
       (fn
        ([] (step))
        ([r x]
         (let [e (sel x)]
           (if (contains? @s e)
             (let [pb @b]
               (vswap! b empty)
               (vswap! b conj x)
               (vswap! s empty)
               (vswap! s conj e)
               (step r pb))
             (do
               (vswap! b conj x)
               (vswap! s conj e)
               r))))
        ([r]
         (let [r (step r @b)]
           (step r)))))))


(defn running-count [pred]
  (fn [step]
    (let [cnt (volatile! 0)]
      (fn
        ([] (step))
        ([r x]
         (if (pred x) (vswap! cnt inc))
         (step r @cnt))
        ([r]
         (step r))))))

(defn count-same
  ([] (count-same identity))
  ([kf] (fn [step]
          (let [cnt (volatile! 0)
                prev (volatile! ::none)
                prevx (volatile! ::none)]
            (fn
              ([] (step))
              ([r x]
               (let [xx (kf x)
                     dprev @prev]
                 (if (identical? dprev xx)
                   (do (vswap! cnt inc)
                       r)
                   (let [dprevx  @prevx
                         dcnt    @cnt]
                     (vreset! prev xx)
                     (vreset! prevx x)
                     (vreset! cnt 1)
                     (if (not= dprev ::none)
                       (step r (list dprevx dcnt))
                       r)
                     ))))
              ([r]
               (let [r (step r (list @prevx @cnt))]
                 (step r))))))))

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

(defn lcp
  ([colls]
    (->> (apply map = colls)
         (take-while true?)
         count))
  ([f x]
   (->> (map f x)
        (partition 2 1)
        (map lcp))))

(defn suffix-array [s]
  (let [ss (loop [t   (seq s)
                  ans (transient [])]
             (if (empty? t)
               (persistent! ans)
               (recur (rest t)
                      (conj! ans t))))]
    (->> (map-indexed vector ss)
         (sort-by last cx))))

(defn throttle [c msgs-per-sec]
  (let [bucket (chan)
        out    (chan)]
    (go-loop [n 0]
      (if (= n 0)
        (do (<! bucket)
            (recur msgs-per-sec))
        (let [i (<! c)]
          (if (nil? i)
            (close! out)
            (do (>! out i)
                (recur (dec n)))))))
    (go
      (while true
        (<! (timeout 1000))
        (>! bucket :token)))
    out))

(comment
  (let [in        (chan)
        slow-chan (throttle in 7)
        not-nil?  (comp not nil?)]

    (go
      (dotimes [i 30]
        (>! in i))
      (close! in))

    (time (while (let [x (<!! slow-chan)]
                   (println x)
                   (not-nil? x))))))
