(ns query.bmq
  (:require [clojure.data.csv :as csv]
             [clojure.java.io :as io]))

(def data
  [{:reqid 1
    :something 33
    :key1 [{:v 2
            :c 3}
           {:v 4
            :c 5}]
    :key2 [{:v 8
            :c 9}]}
   {:reqid 2
    :key2 [{:v 6
            :c 7}]}])

(defn- is-child-group? [v]
  (and (or (vector? v) (list? v))
       (or (= (count v) 0)
           (map? (first v)))))

(defn- flatten-group [m]
  (let [child-groups (filter (fn [[_ v]] (is-child-group? v)) m)
        others       (apply dissoc m (map first child-groups))]
    (if (empty? child-groups)
      [m]
      (->> child-groups
           (mapcat (fn [[k ms]]
                     (mapv #(merge % {k true})
                           (mapcat flatten-group ms))))
           (mapv #(merge % others))))))

(defn write-output [data filename]
  (with-open [writer (io/writer filename)]
    (let [maps (mapcat flatten-group data)
          hdrs (->> maps (mapcat keys) (into #{}) sort)
          data (map (apply juxt hdrs) maps)
          out  (cons (map name hdrs) data)]
      (csv/write-csv writer out))))
;;
(write-output data "output2.csv")
