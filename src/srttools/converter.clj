(ns srttools.converter)

(def lyrics "[00:09.13]
[00:12.34]詞：謝小娜 曲：謝小娜
[00:16.45]製作人：常石磊
[00:20.42]
[00:28.06]
[00:37.58]小時候總愛抬頭 看白雲朵朵
[00:45.32]哼著我最愛的angel
[00:53.10]天上白雲一朵朵 都住著angel
[01:01.46]為每個相信童話的孩子 守候
[02:37.36][01:10.65]
[02:38.55][01:11.68]我最愛的那首歌 最愛的angel
[02:46.96][01:20.37]我到什麼時候 才能遇見我的angel
[02:53.91][01:27.25]我最愛的那首歌 最愛的angel
[03:02.34][01:35.93]我不是王子 也會擁有我的angel
[01:47.58]
[02:04.76]每次受傷的時候 天上的angel
[02:12.08]會默默地為我流淚
[02:18.29]
[02:19.80]每下一場小雨都 像一個angel
[02:28.15]陪傷心的人等著陽光 出現時候
[03:18.07]當你學會勇敢就會遇見你的angel
[03:31.88]")

(defn lrc->srt [offset lrc]
  (letfn [(parse-time [t]
            (let [[_ m s f] (re-matches #"(\d{2}):(\d{2}).(\d{2})" t)
                  m (Integer. m)
                  s (Integer. s)
                  f (Integer. f)]
              (+ (* 6000 m) (* 100 s) f)))

          (format-time [t]
            (let [f (mod t 100)
                  m (quot t 6000)
                  s (quot (- t f (* 6000 m)) 100)
                  ]
              (with-out-str (cl-format *out* "00:~2,'0d:~2,'0d.~2,'0d0" m s f))))

          (parse [i]
            (let [ts (->> (re-seq #"\[(\d{2}:\d{2}\.\d{2})\]" i)
                          (map (comp parse-time second)))
                  s  (->> (re-matches #"\[.*\](.*)" i)
                          last)]
              [s ts]))]
    (let [offset (* 100 offset)
          ls     (->> lrc
                      clojure.string/split-lines
                      (map parse)
                      (mapcat (fn [[s ts]] (map (fn [t] [(+ offset t) s]) ts)))
                      (sort-by first))]
      (with-out-str
        (->> ls
             (partition 2 1)
             (remove (fn [[[_ st] _]] (empty? st)))
             (drop 2)
             (map-indexed (fn [i [[s st] [e _]]]
                            (clojure.pprint/cl-format *out* "~A~%~A --> ~A~%~A~%~%"
                                                      (inc i)
                                                      (format-time s)
                                                      (format-time (- e 10))
                                                      st)))
             doall)))))

(->> lyrics
     (lrc->srt 5)
     (spit "angle.srt"))
