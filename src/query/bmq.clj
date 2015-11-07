(ns query.bmq)

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

(denorm #{:key1 :key2} (first data))
(mapcat (partial denorm #{:key1 :key2}) data)
