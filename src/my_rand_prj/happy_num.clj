(ns happy)

(defn digit-seq [x]
  (let [base 10]
    (if (>= x base)
      (let [rem (rem x base)
            i   (int (/ x base))]
        (cons rem (lazy-seq (digit-seq i))))
      (list x))))

(defn next-val [x]
  (->> (digit-seq x)
       (map #(* % %))
       (reduce +)))

(defn num-seq [x]
  (cons x (lazy-seq (num-seq (next-val x)))))

(defn happy? [x]
  (memoize (fn [x]
             (loop [seen #{}
                    xs   (num-seq x)]
               (let [e  (first xs)]
                 (cond
                   (= e 1)             true
                   (contains? seen e)  false
                   :else               (recur (conj seen e) (rest xs))))))))

(def happy-numbers (filter happy? (iterate inc 1)))

;;(take 20 happy-numbers)
