(ns algo.npuzzle)

;; Library used:  https://github.com/clojure/data.priority-map
;;                https://github.com/yzernik/aima-clj 
;;
;; Solution starts at ####################

(import 'clojure.lang.MapEntry 'java.util.Map 'clojure.lang.PersistentTreeMap)

(declare pm-empty)

(defmacro apply-keyfn [x]
  `(if ~'keyfn (~'keyfn ~x) ~x))

; A Priority Map is comprised of a sorted map that maps priorities to hash sets of items
; with that priority (priority->set-of-items),
; as well as a hash map that maps items to priorities (item->priority)
; Priority maps may also have metadata
; Priority maps can also have a keyfn which is applied to the "priorities" found as values in
; the item->priority map to get the actual sortable priority keys used in priority->set-of-items.

(deftype PersistentPriorityMap [priority->set-of-items item->priority _meta keyfn]
  Object
  (toString [this] (str (.seq this)))

  clojure.lang.ILookup
  ; valAt gives (get pm key) and (get pm key not-found) behavior
  (valAt [this item] (get item->priority item))
  (valAt [this item not-found] (get item->priority item not-found))

  clojure.lang.IPersistentMap
  (count [this] (count item->priority))

  (assoc [this item priority]
    (let [current-priority (get item->priority item nil)]
      (if current-priority
        ;Case 1 - item is already in priority map, so this is a reassignment
        (if (= current-priority priority)
          ;Subcase 1 - no change in priority, do nothing
          this
          (let [priority-key (apply-keyfn priority)
                current-priority-key (apply-keyfn current-priority)
                item-set (get priority->set-of-items current-priority-key)]
            (if (= (count item-set) 1)
              ;Subcase 2 - it was the only item of this priority
              ;so remove old priority entirely
              ;and conj item onto new priority's set
              (PersistentPriorityMap.
                (assoc (dissoc priority->set-of-items current-priority-key)
                  priority-key (conj (get priority->set-of-items priority-key #{}) item))
                (assoc item->priority item priority)
                (meta this)
                keyfn)
              ;Subcase 3 - there were many items associated with the item's original priority,
              ;so remove it from the old set and conj it onto the new one.
              (PersistentPriorityMap.
                (assoc priority->set-of-items
                  current-priority-key (disj (get priority->set-of-items current-priority-key) item)
                  priority-key (conj (get priority->set-of-items priority-key #{}) item))
                (assoc item->priority item priority)
                (meta this)
                keyfn))))
        ; Case 2: Item is new to the priority map, so just add it.
        (let [priority-key (apply-keyfn priority)]
          (PersistentPriorityMap.
            (assoc priority->set-of-items
                   priority-key (conj (get priority->set-of-items priority-key #{}) item))
            (assoc item->priority item priority)
            (meta this)
            keyfn)))))

  (empty [this] (PersistentPriorityMap. (empty priority->set-of-items) {} _meta keyfn))

  ; cons defines conj behavior
  (cons [this e]
    (if (map? e)
      (into this e)
      (let [[item priority] e] (.assoc this item priority))))

  ; Like sorted maps, priority maps are equal to other maps provided
  ; their key-value pairs are the same.
  (equiv [this o] (= item->priority o))
  (hashCode [this] (.hashCode item->priority))
  (equals [this o] (or (identical? this o) (.equals item->priority o)))

  ;containsKey implements (contains? pm k) behavior
  (containsKey [this item] (contains? item->priority item))

  (entryAt [this k]
    (let [v (.valAt this k this)]
      (when-not (identical? v this)
        (MapEntry. k v))))

  (seq [this]
    (if keyfn
      (seq (for [[priority item-set] priority->set-of-items, item item-set]
             (MapEntry. item (item->priority item))))
      (seq (for [[priority item-set] priority->set-of-items, item item-set]
             (MapEntry. item priority)))))

  ;without implements (dissoc pm k) behavior
  (without
    [this item]
    (let [priority (item->priority item ::not-found)]
      (if (= priority ::not-found)
        ;; If item is not in map, return the map unchanged.
        this
        (let [priority-key (apply-keyfn priority)
              item-set (priority->set-of-items priority-key)]
          (if (= (count item-set) 1)
            ;;If it is the only item with this priority, remove that priority's set completely
            (PersistentPriorityMap. (dissoc priority->set-of-items priority-key)
                                    (dissoc item->priority item)
                                    (meta this)
                                    keyfn)
            ;;Otherwise, just remove the item from the priority's set.
            (PersistentPriorityMap.
              (assoc priority->set-of-items priority-key (disj item-set item)),
              (dissoc item->priority item)
              (meta this)
              keyfn))))))

  java.io.Serializable  ;Serialization comes for free with the other things implemented
  clojure.lang.MapEquivalence
  Map ;Makes this compatible with java's map
  (size [this] (count item->priority))
  (isEmpty [this] (zero? (count item->priority)))
  (containsValue [this v]
    (if keyfn
      (some (partial = v) (vals this)) ; no shortcut if there is a keyfn
      (contains? priority->set-of-items v)))
  (get [this k] (.valAt this k))
  (put [this k v] (throw (UnsupportedOperationException.)))
  (remove [this k] (throw (UnsupportedOperationException.)))
  (putAll [this m] (throw (UnsupportedOperationException.)))
  (clear [this] (throw (UnsupportedOperationException.)))
  (keySet [this] (set (keys this)))
  (values [this] (vals this))
  (entrySet [this] (set this))

  Iterable
  (iterator [this] (clojure.lang.SeqIterator. (seq this)))

  clojure.lang.IPersistentStack
  (peek [this]
    (when-not (.isEmpty this)
      (let [f (first priority->set-of-items)
            item (first (val f))]
        (if keyfn
          (MapEntry. item (item->priority item))
          (MapEntry. item (key f))))))

  (pop [this]
    (if (.isEmpty this) (throw (IllegalStateException. "Can't pop empty priority map"))
      (let [f (first priority->set-of-items),
            item-set (val f)
            item (first item-set),
            priority-key (key f)]
        (if (= (count item-set) 1)
          ;If the first item is the only item with its priority, remove that priority's set completely
          (PersistentPriorityMap.
            (dissoc priority->set-of-items priority-key)
            (dissoc item->priority item)
            (meta this)
            keyfn)
          ;Otherwise, just remove the item from the priority's set.
          (PersistentPriorityMap.
            (assoc priority->set-of-items priority-key (disj item-set item)),
            (dissoc item->priority item)
            (meta this)
            keyfn)))))

  clojure.lang.IFn
  ;makes priority map usable as a function
  (invoke [this k] (.valAt this k))
  (invoke [this k not-found] (.valAt this k not-found))

  clojure.lang.IObj
  ;adds metadata support
  (meta [this] _meta)
  (withMeta [this m] (PersistentPriorityMap. priority->set-of-items item->priority m keyfn))

  clojure.lang.Reversible
  (rseq [this]
    (if keyfn
      (seq (for [[priority item-set] (rseq priority->set-of-items), item item-set]
             (MapEntry. item (item->priority item))))
      (seq (for [[priority item-set] (rseq priority->set-of-items), item item-set]
             (MapEntry. item priority))))))

;; clojure.lang.Sorted
;; ; These methods provide support for subseq
;; (comparator [this] (.comparator ^PersistentTreeMap priority->set-of-items))
;; (entryKey [this entry] (val entry))
;; (seqFrom [this k ascending]
;;   (let [sets (if ascending (subseq priority->set-of-items >= k) (rsubseq priority->set-of-items <= k))]
;;     (seq (for [[priority item-set] sets, item item-set]
;;            (MapEntry. item priority)))))
;; (seq [this ascending]
;;   (if ascending (seq this) (rseq this))))

(def ^:private pm-empty (PersistentPriorityMap. (sorted-map) {} {} nil))
(defn- pm-empty-by [comparator] (PersistentPriorityMap. (sorted-map-by comparator) {} {} nil))
(defn- pm-empty-keyfn
  ([keyfn] (PersistentPriorityMap. (sorted-map) {} {} keyfn))
  ([keyfn comparator] (PersistentPriorityMap. (sorted-map-by comparator) {} {} keyfn)))


; The main way to build priority maps
(defn priority-map
  "Usage: (priority-map key val key val ...)
Returns a new priority map with optional supplied mappings.
(priority-map) returns an empty priority map."
  [& keyvals]
  {:pre [(even? (count keyvals))]}
  (reduce conj pm-empty (partition 2 keyvals)))

(defn priority-map-by
  "Usage: (priority-map comparator key val key val ...)
Returns a new priority map with custom comparator and optional supplied mappings.
(priority-map-by comparator) yields an empty priority map with custom comparator."
  [comparator & keyvals]
  {:pre [(even? (count keyvals))]}
  (reduce conj (pm-empty-by comparator) (partition 2 keyvals)))

(defn priority-map-keyfn
  "Usage: (priority-map-keyfn keyfn key val key val ...)
Returns a new priority map with custom keyfn and optional supplied mappings.
The priority is determined by comparing (keyfn val).
(priority-map-keyfn keyfn) yields an empty priority map with custom keyfn."
  [keyfn & keyvals]
  {:pre [(even? (count keyvals))]}
  (reduce conj (pm-empty-keyfn keyfn) (partition 2 keyvals)))

(defn priority-map-keyfn-by
  "Usage: (priority-map-keyfn-by keyfn comparator key val key val ...)
Returns a new priority map with custom keyfn, custom comparator, and optional supplied mappings.
The priority is determined by comparing (keyfn val).
(priority-map-keyfn-by keyfn comparator) yields an empty priority map with custom keyfn and comparator."
  [keyfn comparator & keyvals]
  {:pre [(even? (count keyvals))]}
  (reduce conj (pm-empty-keyfn keyfn comparator) (partition 2 keyvals)))

(defprotocol Problem
  "An abstract formulation of a search problem"
  (initial-state [this] "Initial state in which the agent starts")
  (actions [this state] "Possible actions available to the agent at a state")
  (result [this state action] "The result of taking an action at a state")
  (goal? [this state] "Determines whether a given state is a goal state")
  (step-cost [this state action] "The cost of taking an action in a state"))

(defrecord Node [state action parent cost])

(defn- make-initial-node
  "Make the initial node for a problem"
  [problem]
  (->Node (initial-state problem) nil nil 0))

(defn- make-successor-node
  "Make a successor node from the current node and the next action"
  [problem node action]
  (let [{state :state parent :parent cost :cost} node
        r (result problem state action)
        sc (step-cost problem state action)]
    (->Node r action node (+ sc cost))))

(defn- successors
  "The successor nodes of a given node for a problem"
  [problem node]
  (let [a (actions problem (:state node))]
    (map (partial make-successor-node problem node) a)))

(defn path
  "Show the actions along the path of a node"
  [node]
  (loop [n node
         p ()]
    (if-not (:parent n)
      p
      (recur (:parent n) (conj p (:action n))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol Fringe
  "A queue for storing unexplored nodes"
  (insert [this node] "Insert a new node into the fringe")
  (remove-next [this] "Remove the next node from the fringe"))

(defn- insert-nodes
  "Insert multiple nodes into a fringe"
  [fringe nodes]
  (reduce insert fringe nodes))

(defn tree-search
  "General tree search algorithm"
  [problem fringe]
  (loop [f (insert fringe (make-initial-node problem))]
    (if (seq f)
      (let [[node f] (remove-next f)]
        (if (goal? problem (:state node)) node
            (recur (insert-nodes f (successors problem node))))))))

(defn graph-search
  "General graph search algorithm"
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(extend-type clojure.lang.IPersistentList
  Fringe
  (insert [this node]
    (conj this node))
  (remove-next [this]
    (let [ans (first this)]
      [ans (rest this)])))

(defn- make-stack []
  ())

(defn depth-first-tree-search [problem]
  (tree-search problem (make-stack)))

(defn depth-first-graph-search [problem]
  (graph-search problem (make-stack)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(extend-type clojure.lang.PersistentQueue
  Fringe
  (insert [this node] (conj this node))
  (remove-next [this] [(peek this) (pop this)]))

(defn- make-queue []
  clojure.lang.PersistentQueue/EMPTY)

(defn breadth-first-tree-search [problem]
  (tree-search problem (make-queue)))

(defn breadth-first-graph-search [problem]
  (graph-search problem (make-queue)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(extend-type PersistentPriorityMap
  Fringe
  (insert [this node] (conj this [node node]))
  (remove-next [this] [(first (peek this)) (pop this)]))

(defn- make-priority-queue [f]
  (priority-map-keyfn f))

(defn best-first-graph-search [problem f]
  (graph-search problem (make-priority-queue f)))

(defn uniform-cost-search [problem]
  (best-first-graph-search problem (fn [node] (:cost node))))

(defn greedy-best-first-search [problem h]
  (best-first-graph-search problem (fn [node] (h (:state node)))))

(defn astar-search [problem h]
  (let [f (fn [node] (+ (h (:state node)) (:cost node)))]
    (best-first-graph-search problem f)))


;; ###########################
;; # Solution starts here... #
;; ###########################

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
             :let [v (get-in board [r c])]]
         (mdist [r c] (hmap v)))
       (reduce +)))


(defn run []
  (let [k     (Integer/parseInt (read-line))
        board (->> (for [_ (range (* k k))]
                     (Integer/parseInt (read-line)))
                   (partition k)
                   (mapv vec))
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
    (println (count ans))
    (doseq [m ans]
      (println (case m
                 [0 1]  "RIGHT"
                 [0 -1] "LEFT"
                 [1 0]  "DOWN"
                 [-1 0] "UP")))))

(comment (->> 
          (run)
          (with-in-str "3
0
3
8
4
1
7
2
6
5"
            )
          ;; with-out-str
          (dotimes [_ 1])
          time))

(time
 (with-out-str
   (with-in-str "3
0
1
2
4
5
3
7
8
6
"
     (run))))

(comment
  (time
   (with-in-str "4
5
1
6
4
9
3
0
7
2
10
11
8
13
14
15
12"
     (run))))
