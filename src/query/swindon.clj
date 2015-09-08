(ns query.swindon
  (:require [clj-http.client :as c]
            [reaver :as r :refer [extract extract-from text attr parse]]))

(use 'clojure.pprint)

(let [p "xxxxxxxx"
      d (->> (c/post "https://www.swindonbooks.com/tborder_status.asp"
                     {:form-params {:FormAction    "SEARCH"
                                    :OrderBranch   "LR"
                                    :OrderNo       "72450"
                                    :ContactPhone1 (.substring p 0 3)
                                    :ContactPhone2 (.substring p 4 7)
                                    :ContactPhone  p
                                    :ContactEmail  nil}}
                     )
             :body
             parse)]
  (->> (extract-from d "table tr"
                     [:x]
                     "td" text)
       (map :x)
       (filter #(= 6 (count %)))
       (drop 1)
       (remove #(= "Collected" (nth % 5)))
       (group-by #(nth % 5))
       vals
       (apply concat)
       (map (fn [[_ n _ _ _ s]] [s n]))
       (cl-format *out* "~{~{~20A~}~%~}")))
