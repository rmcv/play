(ns query.web
  (:require [reaver :refer [parse extract-from text attr]]
            [clj-http.client :as c]))

(def score {:5** 7 :5* 6 :5 5 :4 4 :3 3 :2 2 :1 1 :U 0})

(defn all-7 [cs es maths]
  (let [es (take 3 es)]
    (reduce + (if (empty? maths)
                (concat cs es)
                (let [math-highest (reduce max maths)
                      elec-lowest  (reduce min es)
                      bonus        (if (> math-highest elec-lowest)
                                     (/ (+ math-highest elec-lowest) 2.0)
                                     elec-lowest)]
                  (concat cs (cons bonus (butlast es))))))))

(defn standardize [{:keys [id band cores electives]}]
  (let [is-ext?    (fn [[k _]] (.startsWith k "Mathematics Extended Modules"))
        maths      (vals (filter is-ext? electives))
        electives  (remove is-ext? electives)
        sort-score (comp reverse sort)
        cs         (sort-score (vals cores))
        es         (sort-score (vals electives))
        a          (sort-score (concat cs es))]
    {:id    id
     :band  band
     :4+1   (apply + (concat cs (take 1 es)))
     :4+2   (apply + (concat cs (take 2 es)))
     :4+3   (apply + (concat cs (take 3 es)))
     :best5 (apply + (take 5 a))
     :best6 (apply + (take 6 a))
     :all   (all-7 cs es maths)}))

(defn ->record [{:keys [core elective-subj elective-grade]}]
  (let [core  (vec core)
        grade #(->> (nth core %)
                    keyword
                    score)
        electives (->> (zipmap elective-subj
                               (map (comp score keyword) elective-grade))
                       (remove (fn [[k v]] (or
                                           (not (instance? String k))
                                           (nil? v)))))]
    (try
      {:id (read-string (first core))
       :band (second core)
       :cores {:eng-lang (grade 2)
               :chi-lang (grade 3)
               :math     (grade 4)
               :ls       (grade 5)}
       :electives electives}
      (catch Exception e))))

(defn get-urls2 [course year]
  (let [host "http://atu.hk/"
        data (->> (c/get (str host "adm-grades.php")
                         {:query-params {:programme course
                                         :year year}})
                  :body)]
    (->> (extract-from (parse data) "table.data tr"
                       [:core :style :elective-subj :elective-grade]
                       "td" text
                       "td" (attr :style)
                       "td .electives .electives-subj" text
                       "td .electives .electives-grade" text)
         (remove #(empty? (:core %)))
         (filter #(empty? (remove nil? (:style %))))
         (map ->record)
         (remove nil?)
         #_(map standardize))))


(defn pc [year course]
  (->> (get-urls2 course year)
       (map standardize)
       (map vals)
       (map #(clojure.pprint/cl-format *out* (str year " " course  "~{~6d~}~%") %))
       ))


(for [y (range 2012 2015)]
  (pc y "JS4501"))

                                        ; JS6456 HKU Medi
                                        ; JS4501 CU Medi


(let [course "JS4501"]
  (for [year (range 2012 2015)]
    (->> (get-urls2 course year)
         (map standardize)
         (map :4+1)
         frequencies
         clojure.pprint/pprint
         )))

(->> (standardize {:id 3136,
                   :band "A1",
                   :cores {:eng-lang 7, :chi-lang 4, :math 6, :ls 7},
                   :electives
                   {"Biology" 6,
                    "Chemistry" 5,
                    "Eng Lit" 6,
                    "Mathematics Extended Modules I" 6}})
     clojure.pprint/pprint)
