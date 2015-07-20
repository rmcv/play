(ns bloom-filter.core)

(defprotocol change-vec
  (change [this new-vec]))

(deftype BloomFilter [bits m n p k]
  change-vec
  (change [this new-vec]
    (BloomFilter. new-vec m n p k)))


(defn- find-p
  "for a given dimension of a set and the occupation in
  bits of the filter returns the optimum value for the false
  positive rate"
  [m n]
  (Math/pow 0.5 (Math/ceil (* (/ n m) (Math/log 2)))))

(defn create
  "this method creates a BloomFilter whit optimum number of hash
   functions.
   The p value can either be a number between 0 and 1
   or not a number (for example a letter or nil).
   If p it's not a number it means that you don't need a
   specific value of probability and the optimum one
   will be used.
   The parameters m and n must be integer and positive"
  ([m n p]
   {:pre [(integer? m) (pos? m) (integer? n) (pos? n) (not (= p nil))]}
   (if (number? p)
     ;(do (if-not (and (< p 1) (> p 0))
     (do (if-not (= p (find-p m n))
           (throw (Exception.
                    "Invalid Probability value"))
           (let [bloom-vec
                 (apply vector-of :long
                        (repeat
                          (Math/ceil (/ n 64)) 0))
                 k (Math/ceil (* (/ n m) (Math/log 2)))]
             (BloomFilter. bloom-vec m n p k))))
     (let [p (find-p m n)]
       (create m n p))))
  ([m p]
   {:pre [(integer? m) (pos? m)
          (number? p) (< p 1) (> p 0)]}
   (let [n (Math/ceil
             (* m (/
                    (Math/log (/ 1 p))
                    (* (Math/log 2) (Math/log 2)))))]
     (create m (bigint n) (find-p m n)))))

(def murmur
  (let [m (com.google.common.hash.Hashing/murmur3_128)]
    (fn ^Long [^String s]
      (-> (doto (.newHasher m)
            (.putString
              s com.google.common.base.Charsets/UTF_8))
          (.hash)
          (.asLong)))))

(defn- hash-fun
  "evaluates the murmur hash function (long) of the
  given parameter and returns it as a couple of int"
  [^String a]
  (let [h64 (murmur a)]
    [(int (Math/abs (bit-shift-right h64 32)))
     (int (Math/abs (unchecked-int h64)))]))

(defn- bitmask ([n] (bit-shift-left (long 1) n)))

(defn- indexes
  "returns an array of the indexes referred to
  the k hash function on a given filter.
  They are in the form (array-index, cell-index)
  where array-index refers to a cell in the array
  and cell-index a bit into the cell"
  [^String a bloom]
  (let [[h1 h2] (hash-fun a)
        index (for [i (range (.k bloom))]
                   (mod (+ h1 (* i h2)) (.n bloom)))]
     (map (fn [y] [(int (Math/floor (/ y 64)))
                   (int (mod y 64))]) index)))

(defn insert
  "inserts an element in the given BloomFilter"
  [^String a bloom]
  {:pre [(instance? bloom_filter.core.BloomFilter bloom)]}
  (change bloom
          (reduce
            (fn [vect [array-index cell-index]]
              (assoc vect array-index
                          (bit-or (bitmask cell-index)
                                  (nth vect array-index))))
            (.bits bloom) (indexes a bloom))))

(defn insert-vec
  "insert each element from a vector of elements"
  [vec bloom]
  (reduce (fn [x y] (insert y x)) bloom vec))

(defn- get-bit
  "Reads the bit in the array-index cell and in the
   cell-index position.
   It returns zero if the bit is zero, otherwise it returns
   a power of two"
  [array-index cell-index bloom]
  (bit-and (bitmask cell-index)
           (nth (.bits bloom) array-index)))

(defn find-element
  "Finds a given element in a given BloomFilter"
  [a bloom]
  {:pre [(instance? bloom_filter.core.BloomFilter bloom)]}
  (reduce (fn
            [acc [array-index cell-index]]
            (and acc (not
                       (zero?
                         (get-bit
                           array-index
                           cell-index bloom)))))
           true
           (indexes a bloom)))
