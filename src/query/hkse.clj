(ns query.hkse
  (:require [clj-http.client :as c]
            [reaver :refer [parse extract-from text attr]]))

(use 'clojure.pprint)

(def ^:private urls
  {:shortsell  "http://www.hkex.com.hk/unicode/PrintFriendly/PrintFriendly.asp?url=http://www.hkex.com.hk/eng/market/sec_tradinfo/stkcdorder.htm"
   :listing "http://www.hkex.com.hk/eng/market/sec_tradinfo/stockcode/eisdeqty_pf.htm"
   :company "http://www.hkex.com.hk/eng/invest/company/profile_page_e.asp?WidCoID=~a&WidCoAbbName=&Month=&langcode=e"})


(defn- http-get [url-key & args]
  (let [url (apply cl-format nil (url-key urls) args)]
    (-> (c/get url)
        :body
        parse)))

(defn shortsell-eligible []
  (let [data (http-get :shortsell)]
    (->> (extract-from data "TABLE[class=ms-rteTable-2] tr"
                       [:data]
                       "td" text)
         (map :data)
         (map (fn [[_ c d]]
                (let [s  (->> c
                              read-string
                              (cl-format nil "~5,'0d"))]
                  [s d])))
         (into {}))))

(defn get-securities []
  (let [data       (http-get :listing)
        ->security (fn [[code name board-lot ccass shortsell options futures]]
                     {:code      code
                      :name      name
                      :board-lot (read-string board-lot)
                      :ccass     (= "#" ccass)
                      :shortsell (= "H" shortsell)
                      :opitons   (= "O" options)
                      :futures   (= "F" futures)})]
    (->> (extract-from data "TABLE[class=table_grey_border] tr"
                       [:data]
                       "td" text)
         (map (comp ->security :data)))))

(defn get-company-details [code]
  (let [data        (http-get :company code)
        get-details (fn [{:keys [k v]}]
                      (zipmap (map keyword k) (drop 1 v)))]
    (->> (extract-from data "table tr"
                       [:k :v]
                       "td[colspan=\"0\"]" text
                       "td[colspan=\"3\"]" text)
         first
         get-details)))
