(ns warden)

;; To start the game with 4 prisoners: (play-game 4)

(use 'clojure.pprint)

(defn- create-warden [prisoners]
  (let [called (atom #{})
        total-prisoners (count prisoners)]
    (fn [[left right]]
      (let [prisoner (rand-nth prisoners)]
        (swap! called conj prisoner)
        (case (prisoner left right)
              :switch-left    {:action :continue :state [(not left) right]}
              :switch-right   {:action :continue :state [left (not right)]}
              :demand-release (let [called-in (count @called)]
                                (if (= called-in total-prisoners)
                                  {:action :release-all}
                                  {:action :execute-all})))))))

(defn- create-leader [total]
  (let [count (atom 0)]
    (fn [_ right]
      (if right
        (let [counted  (swap! count inc)
              maxCount (dec total)]
          (cl-format *out* "leader counted: ~4d" counted)
          (if (= maxCount counted)
            :demand-release
            :switch-right))
        :switch-left))))

(defn- create-gang []
  (let [clicked (atom 0)]
    (fn [_ right]
      (if right
        :switch-left
        (if (= 0 @clicked)
          (do
            (swap! clicked inc)
            :switch-right)
          :switch-left)))))

(defn- loop-call [warden init-state]
  (loop [st init-state
         x  1]
    (let [{:keys [action state]} (warden st)]
      (cl-format *out* "~%~5d. ~12a ~{~6a~}" x action state)
      (if (= action  :continue)
        (recur state (inc x))
        action))))

(defn- create-prisoners [x]
  (cons
   (create-leader x)
   (take (dec x) (repeatedly create-gang))))

(defn play-game [num-of-prisioners]
  (let [warden (create-warden (create-prisoners num-of-prisioners))]
    (loop-call warden [false false])))
