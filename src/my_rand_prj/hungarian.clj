(ns bike.bikeracer)

(use 'clojure.set)

;; assignment algorithm

(defn transpose [m]
  (apply mapv vector m))

(defn matrix [f sources targets]
  (mapv (fn [src] (mapv (fn [target] (f src target)) targets))
        sources))

(defn subtract-row-min [m]
  (mapv (fn [row row-m] (mapv - row (repeat row-m)))
        m
        (mapv #(apply min %) m)))

(defn baseline [m]
  (->> m
       subtract-row-min
       transpose
       subtract-row-min
       transpose))

(defn- col-to-assign [row orow assigned]
  (->> row
       (map-indexed (fn [i e] [i e (orow i)]))
       (filter (fn [[i e _]]
                 (and (zero? e) (neg? (assigned i)))))
       (sort-by (fn [[_ _ v]] v))
       ffirst)
  #_(->> row
       (keep-indexed
        (fn [i e]
          (if (and (= e 0) (= (assigned i) -1))
            i)))
       first))

(defn col-assignment [m om]
  (let [assigned (volatile! (vec (repeat (count m) -1)))]
    (doall
     (for [row-idx (range (count m))
           :let [row (m row-idx)
                 orow (om row-idx)
                 col-idx (col-to-assign row orow @assigned)]
           :when col-idx]
       (vswap! assigned assoc col-idx row-idx)))
    @assigned))

(defn uncovered [m {:keys [marked-rows marked-cols]}]
  (let [n-cols (count (first m))]
    (for [row-idx (range (count m))
          :when   (not (marked-rows row-idx))
          c       (range n-cols)
          :when   (not (marked-cols c))
          :let    [row (m row-idx)]]
      (row c))))

(defn- find-cols-to-mark [row-idx {:keys [m marked-cols marked-rows] :as marks}]
  (let [row       (m row-idx)
        has-zero? (fn [col-idx] (->> m
                                     (map-indexed (fn [i e] [i e]))
                                     (remove (fn [[i e]] (marked-rows i)))
                                     (map last)
                                     (filter #(zero? (% col-idx)))
                                     first))
        zcols     (->> (remove marked-cols (range (count row)))
                       (filter has-zero?))]
    (set zcols)))

(defn- find-rows-to-mark [col-idx {:keys [m marked-cols marked-rows assignment] :as marks}]
  (let [has-zero?    (fn [row-idx]
                       (let [row (m row-idx)]
                         (->> row
                              (map-indexed (fn [i e] [i e]))
                              (remove (fn [[i e]] (or (= col-idx i) (marked-cols i))))
                              (map last)
                              (filter #(zero? %))
                              first)))
        zrows        (->> (remove marked-rows (range (count m)))
                          (filter has-zero?))]
    (set zrows)))

(defn- find-covered [m col-assigned]
  {:pre  [(do
            ;; (println col-assigned)
            true)
          (not= (count (first m))
                (count (remove neg? col-assigned)))]
   :xpost [(if (empty? (uncovered m %))
            (do
              ;; (println col-assigned)
              ;; (println %)
              #_(spit "c:/mout2.csv" (clojure.pprint/cl-format nil "~{~{~d,~}~%~}" m))
              true)
            #_true)
          (every? (comp not zero?) (uncovered m %))]}
  (let [n-rows          (count m)
        assigned-rows   (set (remove neg? col-assigned))
        unassigned-rows (remove assigned-rows (range n-rows))
        cols-to-mark    (into #{} (for [r unassigned-rows
                                        c (range (count (first m)))
                                        :when (zero? (get-in m [r c]))]
                                    c))
        marks           (loop [marks        {:marked-cols #{}
                                             :marked-rows #{}
                                             :m m
                                             :assignment (zipmap (range) col-assigned)}
                               rows-to-mark nil
                               cols-to-mark cols-to-mark]
                          (if (not-empty rows-to-mark)
                            (let [m1       (update marks :marked-rows clojure.set/union rows-to-mark)
                                  new-cols (->> rows-to-mark
                                                (map #(find-cols-to-mark % m1))
                                                (apply clojure.set/union))
                                  m2       (update m1 :marked-cols clojure.set/union new-cols)]
                              (recur m2 nil new-cols))
                            (if (not-empty cols-to-mark)
                              (let [m1       (update marks :marked-cols clojure.set/union cols-to-mark)
                                    new-rows (->> cols-to-mark
                                                  (map #(find-rows-to-mark % m1))
                                                  (apply clojure.set/union))
                                    m2       (update m1 :marked-rows clojure.set/union new-rows)]
                                (recur m2 new-rows nil))
                              marks)))]
    marks))

(defn adjust-baseline [m {:keys [marked-cols marked-rows] :as cover}]
  (let [u-min (apply min (uncovered m cover))]
    (vec
     (map-indexed
      (fn [row-idx row]
        (let [is-marked-rows? (marked-rows row-idx)]
          (vec
           (map-indexed
            (fn [i e]
              (let [is-marked-cols? (marked-cols i)]
                (cond
                  (and is-marked-cols? is-marked-rows?) (+ e u-min)
                  (and (not is-marked-cols?)
                       (not is-marked-rows?)) (- e u-min)
                  :else e)))
            row))))
      m))))

(defn find-assignment [m]
  {:post [(every? (comp not neg?) (:assigned %))]}
  (loop [mx (baseline m)
         {:keys [marked-cols marked-rows] :as covers} {:marked-cols #{} :marked-rows #{}}]
    (let [assignment (col-assignment mx m)]
      (if (every? (comp not neg?) assignment)
        {:assigned assignment
         :covers covers}
        (let [covers (find-covered mx assignment)
              m2     (adjust-baseline mx covers)]
          (if (= mx m2)
            {:error mx}
            (recur m2 covers)))))))

;; problem domain

(defn edist [[x y] [m n]]
  (let [d1 (- x m)
        d2 (- y n)]
    (+ (* d2 d2) (* d1 d1))))

(defn pick [m assigned]
  (let [n-cols (count (first m))
        n-rows (count m)]
  (for [col-idx (range (count assigned))
        :when   (< col-idx n-cols)
        :let    [row-idx (assigned col-idx)]
        :when   (< row-idx n-rows)]
    (get-in m [(assigned col-idx) col-idx]))))

(defn run [lines]
  (let [parse-ints      (fn [l] (->> (re-seq #"\d+" l)
                                    (map #(Integer/parseInt %))))
        [n m k]         (parse-ints (first lines))
        bikers          (->> lines
                             rest
                             (take n)
                             (mapv parse-ints))
        bikes           (->> lines
                             (drop (inc n))
                             (take m)
                             (mapv parse-ints))
        mtx             (matrix edist bikers bikes)
        all-dists       (mapcat identity mtx)
        min-dist        (apply min all-dists)
        max-dist        (apply max all-dists)
        mk              (- (* 2 m) k)
        nk              (- (* 2 n) k)
        sz              (max mk nk)
        pad-left        (vec (repeat (- sz m) min-dist))
        pad-bottom      (vec (repeat (- sz n) (vec (repeat sz max-dist))))
        mtx2            (vec (concat (mapv #(vec (concat % pad-left)) mtx)
                                     pad-bottom))
        ans             (find-assignment mtx2)
        {:keys
         [marked-rows
          marked-cols]} (:covers ans)
        zm              (get-in ans [:covers :m])
        seen-r          (volatile! #{})
        seen-c          (volatile! #{})
        pairs           (->> (for [r (range n)
                                   c (range m)
                                   :when (or (marked-rows r) (marked-cols c))
                                   :let [v (get-in zm [r c])]
                                   :when (zero? v)
                                   ]
                               [r c (get-in mtx [r c])])
                             (sort-by last)
                             #_(filter (fn [[r c _]]
                                       (if (@seen-r r)
                                         false
                                         (if (@seen-c c)
                                           false
                                           (do
                                             (vswap! seen-c conj c)
                                             (vswap! seen-r conj r)
                                             true))))))
        xas (map vector (:assigned ans) (range))
        ]

    ;; (doall
    ;;  (for [r (range (count mtx))
    ;;        c (range (count (first mtx)))
    ;;        :when (= 9418 (get-in mtx [r c]))]
    ;;    (println [r c])))

    (doseq [x xas]
      (println x (get-in mtx x)))
(println pairs)
    ans
    ;; :details (map #(get-in mtx %) assignment)
    ;; :cost (reduce max (map #(get-in mtx %) assignment))
    #_(println (reduce max (pick mtx assignment)))))

;; (run (line-seq (java.io.BufferedReader. *in*)))


#_(with-in-str "3 3 2
0 1
0 2
0 3
100 1
200 2
300 3"
  (run (line-seq (java.io.BufferedReader. *in*)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(time
 (with-in-str (slurp "/Users/rimonchi/Projects/HackerRank/Algo/bike/input08.txt")
   (run (line-seq (java.io.BufferedReader. *in*)))))

(def m1 (matrix (fn [_ _] (+ 5 (rand-int 20))) [:a :b :c :d :f :g] [:m :n :o :p :q :r ]))

(def m1 (mapv vec (partition 4 [82	83	69	92
                                77	37	49	92
                                11	69	5	86
                                8	9	98	23])))

(find-assignment m1)

(col-assignment m1)

(col-to-assign [6 64 0 66] [3 2 -1 -1])
(col-to-assign [13 14 0 8]  [-1 -1 -1 -1])
(use 'clojure.pprint)
(pprint m1)
(pprint (baseline m1))
(subtract-row-min m1)

(def b1 (->> (find-assignment m1)
             :err
             baseline))

(->> b1
     #_(cl-format *out* "~{~{~4d~}~%~}")
     col-assignment
     (mark-all-rows-having-no-assignment! b1 (volatile! #{}))
     last
     (#(mark-all-unmarked-cols-having-zero! b1 % (volatile! #{})))
     #_(find-covered b1)
     pprint)

(cl-format *out* "~{~{~4d~}~%~}" m1)

(let [bl (baseline m1)]
  (->> bl
       #_col-assignment
       #_(find-covered bl)
       #_(adjust-baseline bl)
       #_col-assignment
       pprint))
