(ns rz.optimizer
  (:require [clojure.java.io :as io]
            [clojure.data.csv :as csv]
            [clojure.pprint :as pp]
            [clojure.string :as string]
            [clojure.math.combinatorics :as combo]
            [net.cgrand.enlive-html :as html]
            [rz.projection :as proj]
            [rz.data :as data]
            [clojure.java.shell :as shell]
            [rz.optimizers.genetic :as genetic]
            [rz.optimizers.cpplex :as cpplex]
            )
  (:gen-class))

(defonce players-data (data/add-rotowires-projection (data/init-players-data)))



(defn- get-player
  [name]
  (first (filter (fn [p] (re-find (re-pattern (str name ".*")) (:name p))) players-data)))

(defn get-team
  []
  {
   :PG1 (get-player "Brandon Knight")
   :PG2 (get-player "Jeff Teague")
   :SG1 (get-player "Devin Booker")
   :SG2 (get-player "Kyle Korver")
   :SF1 (get-player "LeBron James")
   :SF2 (get-player "P.J. Tucker")
   :PF1 (get-player "Anthony Davis")
   :PF2 (get-player "Kevin Love")
   :C (get-player "Marc Gasol")})

(comment
  (print-team (get-team))
  )


(defn -main
  [& args]
  (genetic/optimize-lineup)
  )


;(defn magic-series
;  [N]
;  (with-store (store)
;              (let [L (vec (for [i (range N)]
;                             (int-var (str i) 0 N)))] ; initialize L to be a vector of vars
;                (doseq [i (range N)]
;                  (constrain! ($= ($occurrences L i)
;                                  (nth L i)))) ; L[i] = # of times i occurs in L
;
;                ; This is a redundant constraint, i.e. a constraint that doesn't change the feasibility of the problem
;                ; but makes the solving faster: summation(i=0..N | i * L[i]) = N. (Think about it!)
;                (constrain! ($= ($weighted-sum L (range N)) N))
;
;                (let [solved (solve!)]
;                  (map solved (map str (range 0 N)))))))




