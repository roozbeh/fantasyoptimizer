(defproject
  FantasyOptimizer "1.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/data.csv "0.1.3"]
                 [org.clojure/math.combinatorics "0.1.1"]
                 [enlive "1.1.6"]
                 [org.jacop/jacop "3.2"]
                ]

  :main rz.optimizer
  :uberjar-name "fantasy_optimizer.jar"
  :jvm-opts ["-Xmx2g" "-Djava.awt.headless=true" "-XX:-OmitStackTraceInFastThrow"]

  :profiles {:remote-debug {:jvm-opts ["-agentlib:jdwp=transport=dt_socket,server=y,suspend=n,address=5005"]}
             :uberjar      {:aot [rz.optimizer]}}
  )
