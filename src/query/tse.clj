(ns query.tse
  (:require [clj-http.client :as c]
            [reaver :refer [parse extract-from text attr]]))

(use 'clojure.pprint)

(defn get-mkt-div []
  (let [doc (->> (c/get "http://www2.tse.or.jp/tseHpFront/JJK020020Action.do")
                 :body
                 parse)
        ans (->> (extract-from doc "span.checkboxField"
                               [:txt :code]
                               "label"  text
                               "label input" (attr :value))
                 first)]
    (zipmap (:txt ans) (:code ans))))

(defn- get-cookies []
  (let [cs  (clj-http.cookies/cookie-store)
        _   (c/get "http://www2.tse.or.jp/tseHpFront/JJK020020Action.do"
                   {:cookie-store cs})]
    cs))

(defn get-details
  [cs sec-code]
  (let [doc (->> (c/post "http://www2.tse.or.jp/tseHpFront/JJK020030Action.do"
                         {:form-params {:BaseJh "BaseJh"
                                        :mgrCd  sec-code}
                          :cookie-store cs
                          })
                 :body
                 parse)]
    (->> (extract-from doc "TABLE[class=fontsizeS margin20] tr"
                       [:x :y]
                       "th" text
                                        ;"td" text
                       ))))

(defn get-securities
  ([x]
   (if (instance? String x)
     (get-securities x nil)
     (get-securities nil x)))
  ([div sec-code]
   (let [cs  (clj-http.cookies/cookie-store)
         _   (c/get "http://www2.tse.or.jp/tseHpFront/JJK020020Action.do"
                    {:cookie-store cs})
         doc (->> (c/post "http://www2.tse.or.jp/tseHpFront/JJK020010Action.do"
                          {:form-params {:dspJnPd      0
                                         :dspSsuPd     5000
                                         :dspJnKmkMiPd 1001
                                         :mgrMiTxtBx   ""
                                         :ListShow     "ListShow"
                                         :eqMgrCd      sec-code
                                         :szkbuChkbx   div
                                         }
                           :cookie-store cs
                           })
                  :body
                  parse)]
     (->> (extract-from doc "TABLE[class=tableStyle01 fontsizeS] tr"
                        [:x]
                        "td" text)
          (map :x)
          (remove empty?)
          (remove #(= % "    Display of stock price"))
          (partition 2)
          (map (fn [[l1 l2]]
                 (if (= 9 (count l1))
                   [l1 l2]
                   (let [[a b c d e f g h] l1]
                     [[a b c d "-" e f g h] l2]))))
          (map (fn [[[code mkt-div main-office balance-sheet-date balance-sheet-end-date trading-unit _ _ _]
                     [issue-name industry]]]
                 {:code                   (read-string code)
                  :mkt-div                mkt-div
                  :main-office            main-office
                  :balance-sheet-date     balance-sheet-date
                  :balance-sheet-end-date balance-sheet-end-date
                  :trading-unit           (.parse (java.text.NumberFormat/getInstance) trading-unit)
                  :issue-name             issue-name
                  :industry               industry}))))))

(comment
  (get-securities 6201)
  (get-securities 62010)
  (get-securities "004")
  (time (def data (->> (get-mkt-div)
                       vals
                       (pmap get-securities)
                       (apply concat)))))
