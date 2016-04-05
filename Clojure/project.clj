(defproject
  FantasyOptimizer "1.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/data.csv "0.1.3"]
                 [org.clojure/math.combinatorics "0.1.1"]
                 [enlive "1.1.6"]
                 [org.jacop/jacop "3.2"]
                 [com.novemberain/monger "3.0.2"]
                 [org.slf4j/slf4j-nop "1.7.12"]
                 [org.clojure/data.json "0.2.6"]
                 [com.cemerick/url "0.1.1"]
                 [incanter "1.2.3-SNAPSHOT"]
                 [clj-time "0.11.0"]
                 [clj-http "2.1.0"]

                 [http-kit "2.1.18"]
                 [compojure "1.4.0"]
                 [hiccup "1.0.5"]
                 [com.cemerick/url "0.1.1"]
                 ]
  :main rz.web
  :uberjar-name "fantasy_optimizer.jar"
  :jvm-opts ["-Xmx2g"  "-XX:-OmitStackTraceInFastThrow"]

  :profiles {:remote-debug {:jvm-opts ["-agentlib:jdwp=transport=dt_socket,server=y,suspend=n,address=5005"]}
             :uberjar      {:aot [rz.web]}}

  )

