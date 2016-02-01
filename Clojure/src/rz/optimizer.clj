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
            [rz.optimizers.constants :as c]
            [rz.optimizers.coinmp :as coinmp]
            [rz.scrap.rotogrinder :as rotoscrap]
            [rz.model.linear :as linear]
            [rz.model.svm :as svm]
            [rz.model.model :as model]
            [rz.optimizers.utils :as utils]
            [monger.collection :as mc])
  (:gen-class))

(defn- force-db-update
  []
  (rotoscrap/ingest-data (data/init-players-data-fanduel)
                         :force-update true))

(defn- optimize-fanduel-lineups
  []
  (let [db (utils/get-db)
        players-data (data/init-players-data-fanduel)
        players-data (data/remove-injured players-data)
        _   (rotoscrap/ingest-data players-data)
        coefs (linear/create-model db c/*fanduel*)
        players-proj (linear/add-linear-projection db players-data coefs c/*fanduel*)
        ]
    (coinmp/lpsolve-solve-fanduel players-proj :linear-projection)))

(defn- optimize-fanduel-lineups-svm
  []
  (let [db (utils/get-db)
        players-data (data/init-players-data-fanduel)
        players-data (data/remove-injured players-data)
        _   (rotoscrap/ingest-data players-data)
        _ (svm/create-svm-model db c/*fanduel*)
        coefs (linear/create-model db c/*fanduel*)
        players-proj (svm/predict-players db players-data c/*fanduel*)
        players-proj (linear/add-linear-projection db players-proj coefs c/*fanduel*)]
    (coinmp/lpsolve-solve-fanduel players-proj  :svm-projection)))

(defn- optimize-draftking-lineups
  []
  (let [db (utils/get-db)
        players-data (data/init-players-data-draftking2)
        players-data (data/remove-injured players-data)
        _   (rotoscrap/ingest-data players-data)
        coefs (linear/create-model db c/*draftking* (map :Name players-data))
        players-proj (linear/add-linear-projection db players-data coefs c/*draftking*)]
    (data/save-solutions
      (coinmp/lpsolve-solve-draftkings players-proj  :linear-projection)
      c/*draftking*)))

(defn- optimize-draftking-lineups-svm
  []
  (let [db (utils/get-db)
        players-data (data/init-players-data-draftking)
        players-data (data/remove-injured players-data)
        ;players-data (data/filter-high-sd players-data db)
        _   (rotoscrap/ingest-data players-data)
        o (svm/create-svm-model db c/*draftking*)
        _ (println o)
        coefs (linear/create-model db c/*draftking*)
        players-proj (svm/predict-players db players-data c/*draftking*)
        players-proj (linear/add-linear-projection db players-proj coefs c/*draftking*)]
    (coinmp/lpsolve-solve-draftkings players-proj  :svm-projection)))


;TODO: to timestamp database, to make sure old data is not being regressioned!

(defn save-projections-dk
  []
  (let [db (utils/get-db)
        players-data (data/init-players-data-draftking)
        players-data (data/remove-injured players-data)
        _   (rotoscrap/ingest-data players-data)
        o (svm/create-svm-model db c/*draftking*)
        _ (println o)
        coefs (linear/create-model db c/*draftking*)
        players-proj (svm/predict-players db players-data c/*draftking*)
        players-proj (linear/add-linear-projection db players-proj coefs c/*draftking*)
        players-proj (data/add-rotowires-projection players-proj c/*draftking*)]
    (data/save-projections db players-proj)))

(defn -main
  [& args]

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




