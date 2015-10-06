(defproject my-rand-prj "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :jvm-opts ["-Xmx3g" "-server"]
  :profiles {:dev {:dependencies [[midje "1.6.3"]]
                   :plugins [[lein-midje "3.1.3"]]}}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [clj-http "1.1.2"]
                 [org.clojure/data.json "0.2.6"]
                 [org.clojure/data.xml "0.0.8"]
                 [org.clojure/math.combinatorics "0.1.1"]
                                        ;[com.mcdermottroe.apple/osxkeychain "1.0"]
                 [co.paralleluniverse/pulsar "0.7.3"]
                 [com.googlecode.concurrent-trees/concurrent-trees "2.4.0"]
                 [net.mikera/core.matrix "0.36.1"]
                 [clatrix "0.5.0"]
                 [reaver "0.1.1"]
                 [criterium "0.4.3"]
                 [com.google.guava/guava "17.0"]
                 [ch.qos.logback/logback-classic "1.1.3"]
                 [org.clojure/tools.logging "0.3.1"]]
  :java-agents [[co.paralleluniverse/quasar-core "0.7.3"]])
