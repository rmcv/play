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


(defn denorm [group-keys d]
 (let [h (->> d
              (remove (fn [[k _]] (contains? group-keys k)))
              (into {}))]
   (->> (select-keys d group-keys)
        (mapcat (fn [[k ms]]
                  (map (fn [m]
                         (merge h {k true} m)) ms))))))

(defn write-output [data group-keys filename]
  (with-open [writer (io/writer filename)]
    (let [maps (mapcat #(denorm group-keys %) data)
          hdrs (->> maps (mapcat keys) (into #{}) sort)
          data (map (apply juxt hdrs) maps)
          out  (cons (map name hdrs) data)]
      (csv/write-csv writer out))))

(write-output data #{:key2 :key1} "output2.csv")

