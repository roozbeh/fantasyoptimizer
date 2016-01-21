(defproject
  FantasyOptimizer "1.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/data.csv "0.1.3"]
                 [org.clojure/math.combinatorics "0.1.1"]
                 [enlive "1.1.6"]
                ]

  :main rz.optimizer
  :jvm-opts ["-Xmx2g" "-Djava.awt.headless=true" "-XX:-OmitStackTraceInFastThrow"]
  )
