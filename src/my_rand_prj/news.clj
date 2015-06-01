(ns news
  (:require [clj-http.client :as h]
            [clojure.pprint :refer :all]
            [clojure.data.json :as json])
  (:import [com.mcdermottroe.apple OSXKeychain]))


(as-> (h/get "https://access.alchemyapi.com/calls/data/GetNews"
             {:query-params {:apikey (.findGenericPassword (OSXKeychain/getInstance)  "alchemyapi" "me")
                             :outputMode "json"
                             :start "now-1d"
                             :end "now"
                             :count 10
                             :q.enriched.url.enrichedTitle.relations.relation.object.entities.entity.type "Company"
                             :q.enriched.url.enrichedTitle.relations.relation.action.verb.text "O[upgrade^downgrade]"
                             :return "enriched.url.url,enriched.url.title"}}) a
      (:body a)
      (json/read-str a :key-fn keyword)
      (get-in a [:result :docs])
      (mapcat (juxt  #(get-in % [:source :enriched :url :title])
                     #(get-in % [:source :enriched :url :url])) a)
      (cl-format *out* "~{~a~%(~a) ~% ~%~}" a))
