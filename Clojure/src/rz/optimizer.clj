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
            [rz.optimizers.constants :as c]
            [rz.optimizers.coinmp :as coinmp]
            [rz.scrap.rotogrinder :as rotoscrap]
            [rz.model.linear :as linear]
            [rz.model.svm :as svm]
            [rz.optimizers.utils :as utils])
  (:gen-class))


(defn- optimize-fanduel-lineups
  []
  (rotoscrap/ingest-data c/*fanduel*)
  (let [db (utils/get-db)
        players-data (data/init-players-data-fanduel)
        coefs (linear/create-model db c/*fanduel*)
        players-proj (linear/add-linear-projection db players-data coefs c/*fanduel*)]
    (pp/pprint (map #(list (:Name %) (:my-projection %)) players-proj))
    (coinmp/lpsolve-solve-fanduel players-proj)
    )
  ;(svm/create-svm-model (utils/get-db))
  ;(coinmp/)
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




