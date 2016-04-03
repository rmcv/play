(import 'clojure.lang.MapEntry 'java.util.Map 'clojure.lang.PersistentTreeMap)

(declare pm-empty)

(defmacro apply-keyfn [x] `(if ~'keyfn (~'keyfn ~x) ~x))

(deftype PersistentPriorityMap [p->soi itm->pri _meta keyfn]
  Object
  (toString [o] (str (.seq o)))

  clojure.lang.ILookup
  (valAt [o im] (get itm->pri im))
  (valAt [o im not-found] (get itm->pri im not-found))

  clojure.lang.IPersistentMap
  (count [o] (count itm->pri))

  (assoc [o im priority]
    (let [c-pri (get itm->pri im nil)]
      (if c-pri
        (if (= c-pri priority)
          o
          (let [pk (apply-keyfn priority)
                cpk (apply-keyfn c-pri)
                ims (get p->soi cpk)]
            (if (= (count ims) 1)
              (PersistentPriorityMap.
                (assoc (dissoc p->soi cpk)
                  pk (conj (get p->soi pk #{}) im))
                (assoc itm->pri im priority)
                (meta o)
                keyfn)
              (PersistentPriorityMap.
                (assoc p->soi
                  cpk (disj (get p->soi cpk) im)
                  pk (conj (get p->soi pk #{}) im))
                (assoc itm->pri im priority)
                (meta o)
                keyfn))))
        (let [pk (apply-keyfn priority)]
          (PersistentPriorityMap.
            (assoc p->soi
                   pk (conj (get p->soi pk #{}) im))
            (assoc itm->pri im priority)
            (meta o)
            keyfn)))))

  (empty [o] (PersistentPriorityMap. (empty p->soi) {} _meta keyfn))

  (cons [o e]
    (if (map? e)
      (into o e)
      (let [[im priority] e] (.assoc o im priority))))

  (equiv [o o] (= itm->pri o))
  (hashCode [o] (.hashCode itm->pri))
  (equals [o o] (or (identical? o o) (.equals itm->pri o)))

  (containsKey [o im] (contains? itm->pri im))

  (entryAt [o k]
    (let [v (.valAt o k o)]
      (when-not (identical? v o)
        (MapEntry. k v))))

  (seq [o]
    (if keyfn
      (seq (for [[priority ims] p->soi, im ims]
             (MapEntry. im (itm->pri im))))
      (seq (for [[priority ims] p->soi, im ims]
             (MapEntry. im priority)))))

  (without
    [o im]
    (let [priority (itm->pri im ::not-found)]
      (if (= priority ::not-found)
        o
        (let [pk (apply-keyfn priority)
              ims (p->soi pk)]
          (if (= (count ims) 1)
            (PersistentPriorityMap. (dissoc p->soi pk)
                                    (dissoc itm->pri im)
                                    (meta o)
                                    keyfn)
            (PersistentPriorityMap.
              (assoc p->soi pk (disj ims im)),
              (dissoc itm->pri im)
              (meta o)
              keyfn))))))

  java.io.Serializable
  clojure.lang.MapEquivalence
  Map
  (size [o] (count itm->pri))
  (isEmpty [o] (zero? (count itm->pri)))
  (containsValue [o v]
    (if keyfn
      (some (partial = v) (vals o))
      (contains? p->soi v)))
  (get [o k] (.valAt o k))
  (put [o k v] (throw (UnsupportedOperationException.)))
  (remove [o k] (throw (UnsupportedOperationException.)))
  (putAll [o m] (throw (UnsupportedOperationException.)))
  (clear [o] (throw (UnsupportedOperationException.)))
  (keySet [o] (set (keys o)))
  (values [o] (vals o))
  (entrySet [o] (set o))

  Iterable
  (iterator [o] (clojure.lang.SeqIterator. (seq o)))

  clojure.lang.IPersistentStack
  (peek [o]
    (when-not (.isEmpty o)
      (let [f (first p->soi)
            im (first (val f))]
        (if keyfn
          (MapEntry. im (itm->pri im))
          (MapEntry. im (key f))))))

  (pop [o]
    (if (.isEmpty o) (throw (IllegalStateException. ))
      (let [f (first p->soi),
            ims (val f)
            im (first ims),
            pk (key f)]
        (if (= (count ims) 1)
          (PersistentPriorityMap.
            (dissoc p->soi pk)
            (dissoc itm->pri im)
            (meta o)
            keyfn)
          (PersistentPriorityMap.
            (assoc p->soi pk (disj ims im)),
            (dissoc itm->pri im)
            (meta o)
            keyfn)))))

  clojure.lang.IFn
  (invoke [o k] (.valAt o k))
  (invoke [o k not-found] (.valAt o k not-found))

  clojure.lang.IObj
  (meta [o] _meta)
  (withMeta [o m] (PersistentPriorityMap. p->soi itm->pri m keyfn))

  clojure.lang.Reversible
  (rseq [o]
    (if keyfn
      (seq (for [[priority ims] (rseq p->soi), im ims]
             (MapEntry. im (itm->pri im))))
      (seq (for [[priority ims] (rseq p->soi), im ims]
             (MapEntry. im priority))))))


(def ^:private pm-empty (PersistentPriorityMap. (sorted-map) {} {} nil))
(defn- pm-empty-by [comparator] (PersistentPriorityMap. (sorted-map-by comparator) {} {} nil))
(defn- pm-empty-keyfn
  ([keyfn] (PersistentPriorityMap. (sorted-map) {} {} keyfn))
  ([keyfn comparator] (PersistentPriorityMap. (sorted-map-by comparator) {} {} keyfn)))


(defn priority-map
  [& keyvals]
  {:pre [(even? (count keyvals))]}
  (reduce conj pm-empty (partition 2 keyvals)))

(defn priority-map-by
  [comparator & keyvals]
  {:pre [(even? (count keyvals))]}
  (reduce conj (pm-empty-by comparator) (partition 2 keyvals)))

(defn priority-map-keyfn
  [keyfn & keyvals]
  {:pre [(even? (count keyvals))]}
  (reduce conj (pm-empty-keyfn keyfn) (partition 2 keyvals)))

(defn priority-map-keyfn-by
  [keyfn comparator & keyvals]
  {:pre [(even? (count keyvals))]}
  (reduce conj (pm-empty-keyfn keyfn comparator) (partition 2 keyvals)))

(defprotocol Problem
  (initial-state [o] )
  (actions [o state] )
  (result [o state action] )
  (goal? [o state] )
  (step-cost [o state action] ))

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
  (insert [o node] )
  (remove-next [o] ))

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

(extend-type PersistentPriorityMap
  Fringe
  (insert [o node] (conj o [node node]))
  (remove-next [o] [(first (peek o)) (pop o)]))

(defn- make-priority-queue [f]
  (priority-map-keyfn f))

(defn best-first-graph-search [problem f]
  (graph-search problem (make-priority-queue f)))

(defn astar-search [problem h]
  (let [f (fn [node] (+ (h (:state node)) (:cost node)))]
    (best-first-graph-search problem f)))

(defn pos+ [position delta]
  (mapv + position delta))

(defn move [{:keys [zpos board] :as b} delta]
  (let [new-pos (pos+ zpos delta)]
    (let [v1 0 ;;(get-in board zpos)
          v2 (get-in board new-pos)
          board' (-> board
                     (assoc-in zpos v2)
                     (assoc-in new-pos v1))]
      (assoc b
             :zpos new-pos
             :board board'))))

(def deltas [[0 1][0 -1][-1 0][1 0]])

(defn all-moves [{:keys [zpos board]}]
  (->> deltas
       (filter (fn [d] (get-in board (pos+ zpos d))))))

(defn solved? [{:keys [board]} soln]
  (= board soln))

(defrecord NPuz [board soln]
  Problem
  (initial-state [o] board)
  (actions [o board] (all-moves board))
  (result [o board action] (move board action))
  (goal? [o board] (solved? board soln))
  (step-cost [o board action] 1)
  )

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
        hfn   (fn [{:keys [board]}]
                (->> (for [r    (range k)
                           c    (range k)
                           :let [v (get-in board [r c])
                                 [r0 c0] (hmap v)
                                 dc (- c0 c)
                                 dr (- r0 r)]]
                       (+ (* dc dc) (* dr dr)))
                     (reduce +)))
        ans   (path
               (astar-search (->NPuz {:zpos zpos :board board} soln)
                             hfn))]
    (println (apply str
                    (for [m ans]
                      (case m
                        [0 1]  \R
                        [0 -1] \L
                        [1 0]  \D
                        [-1 0] \U))))))


(with-in-str "4
1 2 7 3
5 6 0 4
9 10 11 8
13 14 15 12"
  (run))
