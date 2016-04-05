(ns algo.npuzzle2)

(defn fuzzy-partition [bulk keyfn pv k]
  {:pre [(if (not-any? number? bulk)
           true
           (println (map type bulk)))]}
  (loop [l []
         r []
         [e & b :as eb] bulk]
    (if (or (nil? e) (> (count l) k))
      [l (into r eb)]
      (if (> (keyfn e) pv)
        (recur l (conj r e) b)
        (recur (conj l e) r b)))))

(set! *print-length* 10)

(defprotocol IFuzzyPriorityQueue
  (append [this x])
  (seq [this]))

(deftype FuzzyPriorityQueue [cheap bulk keyfn cheap-min all-min]

  ;; clojure.lang.ISeq
  ;; (next [this]
  ;;   (pop this))
  ;; (cons [this arg]
  ;;   (conj this arg))
  ;; (more [this]
  ;;   (not (and (empty? cheap) (empty? bulk))))
  ;; (first [this]
  ;;   (peek this))

  IFuzzyPriorityQueue
  (seq [this]
    (lazy-cat cheap bulk))

  (append [this x]
    (let [cnt (count cheap)
          v   (keyfn x)]
      (cond
        (zero? cnt) (FuzzyPriorityQueue. [x] bulk keyfn v (min all-min v))
        (= cnt 1)   (if (< v cheap-min)
                      (FuzzyPriorityQueue. [x] (conj bulk (peek cheap)) keyfn v (min all-min v))
                      (FuzzyPriorityQueue. cheap (conj bulk x) keyfn cheap-min (min all-min cheap-min)))
        :default    (FuzzyPriorityQueue. cheap (conj bulk x) keyfn cheap-min (min all-min cheap-min)))))

  Object
  (toString [this] (str cheap-min " " all-min " " cheap bulk))

  clojure.lang.IPersistentStack
  (peek [this]
    (when-not (and (empty? cheap) (empty? bulk))
      (peek cheap)))
  (pop [this]
    (if (and (empty? cheap) (empty? bulk))
      (throw (IllegalStateException. "Can't pop empty collection"))
      (let [c (pop cheap)]
        (if (not-empty c)
          (FuzzyPriorityQueue. c bulk keyfn cheap-min all-min)
          (let [[cheap bulk] (fuzzy-partition bulk keyfn (+ all-min 4) 1000)
                cheap'       (vec (sort-by (comp - keyfn) cheap))
                minv         (if-let [v (peek cheap')]
                               (keyfn v)
                               Integer/MAX_VALUE)]
            (FuzzyPriorityQueue. cheap' bulk keyfn minv minv)))))))

(defn fuzzy-priority-queue [keyfn & xs]
  (reduce conj (FuzzyPriorityQueue. [] [] keyfn Integer/MAX_VALUE Integer/MAX_VALUE) xs))

;;;;;;

(defprotocol Problem
  (initial-state [o])
  (actions [o state])
  (result [o state action])
  (goal? [o state])
  (step-cost [o state action]))

(defrecord Node [state action parent cost])

(defn- make-initial-node
  [problem]
  (->Node (initial-state problem) nil nil 0))

(defn- make-successor-node
  [problem node action]
  (let [{state :state parent :parent cost :cost} node
        r (result problem state action)
        sc (step-cost problem state action)]
    (->Node r action node (+ sc cost))))

(defn- successors
  [problem node]
  (let [a (actions problem (:state node))]
    (map (partial make-successor-node problem node) a)))

(defn path
  [node]
  (loop [n node
         p ()]
    (if-not (:parent n)
      p
      (recur (:parent n) (conj p (:action n))))))

(defprotocol Fringe
  (insert [o node])
  (remove-next [o]))

(defn- insert-nodes
  [fringe nodes]
  (reduce insert fringe nodes))

(defn graph-search
  [problem fringe]
  (loop [f (insert fringe (make-initial-node problem))
         c #{}]
    (if (seq f)
      (let [[node f] (remove-next f)
            {state :state} node]
        (cond (goal? problem state) node
              :else (let [nodes (->> (successors problem node)
                                     (remove #(c (:state %))))]
                      (recur (insert-nodes f nodes)
                             (conj c state))))))))

(extend-type FuzzyPriorityQueue
  Fringe
  (insert [o node] (append o node))
  (remove-next [o] [(peek o) (pop o)]))

(defn- make-priority-queue [f]
  (FuzzyPriorityQueue. [] [] f Integer/MAX_VALUE Integer/MAX_VALUE))

(defn best-first-graph-search [problem f]
  (graph-search problem (make-priority-queue f)))

(defn- move-cost
  [h]
  (fn [node]
    (+ (h (:state node)) (:cost node))))

(defn astar-search [problem h]
  (let [f (move-cost h)]
    (best-first-graph-search problem f)))

(defn pos+ [position delta]
  (mapv + position delta))

(defn- mdist [[r0 c0] [r1 c1]]
  (let [dr (- r0 r1)
        dc (- c0 c1)]
    (+ (* dr dr) (* dc dc))))

(defn- calc-hc [hmap hc [rz cz] [r1 c1] v2]  ;; rz cz become v2, r1 c1 become 0
  (let [ans (- hc (mdist (hmap 0) [rz cz]) (mdist (hmap v2) [r1 c1]))
        ans (+ ans (mdist (hmap 0) [r1 c1]) (mdist (hmap v2) [rz cz]))]
    ans))

(defn move [{:keys [zpos board hc] :as b} delta hmap]
  (let [new-pos (pos+ zpos delta)]
    (let [v1     0 ;;(get-in board zpos)
          v2     (get-in board new-pos)
          board' (-> board
                     (assoc-in zpos v2)
                     (assoc-in new-pos v1))]
      (assoc b
             :hc (calc-hc hmap hc zpos new-pos v2)
             :zpos new-pos
             :board board'))))

(def deltas [[0 1] [0 -1] [-1 0] [1 0]])

(defn all-moves [{:keys [zpos board]}]
  (->> deltas
       (filter (fn [d] (get-in board (pos+ zpos d))))))

(defn solved? [{:keys [hc]}]
  (zero? hc))

(defrecord NPuz [board hmap]
  Problem
  (initial-state [o] board)
  (actions [o board] (all-moves board))
  (result [o board action] (move board action hmap))
  (goal? [o board] (solved? board))
  (step-cost [o board action] 1))

(defn- hc [board hmap k]
  (->> (for [r    (range k)
             c    (range k)
             :let [v (get-in board [r c])
                   [r0 c0] (hmap v)
                   dc (- c0 c)
                   dr (- r0 r)]]
         (+ (* dc dc) (* dr dr)))
       (reduce +)))

(defn run []
  (let [k     (Integer/parseInt (read-line))
        pints (fn [s] (mapv #(Integer/parseInt %) (re-seq #"\d+" s)))
        board (mapv pints (repeatedly k read-line))
        zpos  (->> (for [r     (range k)
                         c     (range k)
                         :let  [pt [r c]]
                         :when (= 0 (get-in board pt))]
                     pt)
                   first)
        soln  (mapv vec (partition k (range (* k k))))
        hmap  (into {} (for [r (range k)
                             c (range k)]
                         [(get-in soln [r c]) [r c]]))
        ans   (path
               (astar-search (->NPuz {:zpos zpos
                                      :board board
                                      :hc (hc board hmap k)} hmap)
                             (fn [{:keys [hc]}] hc)))]
    (println (apply str
                    (for [m ans]
                      (case m
                        [0 1]  \R
                        [0 -1] \L
                        [1 0]  \D
                        [-1 0] \U))))))

(comment
  (with-in-str "4
1 2 7 3
5 6 0 4
9 10 11 8
13 14 15 12"
    (run)))

(time
 (with-out-str (with-in-str "3
0 1 2
4 5 3
7 8 6
"
                 (run))))
