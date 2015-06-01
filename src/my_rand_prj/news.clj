(ns news
  (:require [clj-http.client :as h]
            [clojure.pprint :refer :all]
            [clojure.data.json :as json])
  (:import [com.mcdermottroe.apple OSXKeychain]))


(-> (h/get "https://access.alchemyapi.com/calls/data/GetNews"
           {:query-params {:apikey (.findGenericPassword (OSXKeychain/getInstance)  "alchemyapi" "me")
                           :outputMode "json"
                           :start "now-1d"
                           :end "now"
                           :count 10
                           :q.enriched.url.enrichedTitle.relations.relation.object.entities.entity.type "Company"
                           :q.enriched.url.enrichedTitle.relations.relation.action.verb.text "O[upgrade^downgrade]"
                           :return "enriched.url.url,enriched.url.title"}})
    :body
    (json/read-str :key-fn keyword)
    ;(dissoc :usage)
    pprint)
