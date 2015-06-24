(defproject my-rand-prj "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :jvm-opts ["-Xmx3g" "-server"]
  :dependencies [[org.clojure/clojure "1.7.0-RC1"]
                 [clj-http "1.1.2"]
                 [org.clojure/data.json "0.2.6"]
                 [org.clojure/data.xml "0.0.8"]
                 [org.clojure/math.combinatorics "0.1.1"]
                 [com.mcdermottroe.apple/osxkeychain "1.0"]
                 [criterium "0.4.3"]])
