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
            [rz.scrap.espn :as espn]
            [rz.model.model :as model]
            [rz.model.linear :as linear]
            [rz.model.svm :as svm]
            [rz.model.logistic :as logistic]
            [rz.optimizers.utils :as utils]
            [incanter.stats :refer :all]
            [monger.collection :as mc])
  (:gen-class))

(defn- force-espn-update
  []
  (espn/ingest-data (data/init-players-data-draftking2)
                         :force-update true))

(defn- force-rotogrinders-update
  []
  (rotoscrap/ingest-data (data/init-players-data-draftking2)
                    :force-update true))

(defn ingest-data
  [players-data]
  (cond
    (= c/*active-database* :rotogrinder) (rotoscrap/ingest-data players-data)
    (= c/*active-database* :espn) (espn/ingest-data players-data)
    true (assert false (str "Unknown database " c/*active-database*))))

(defn- optimize-fanduel-lineups
  []
  (let [db (utils/get-db)
        players-data (data/init-players-data-fanduel)
        players-data (data/remove-injured players-data)
        player-names (map :Name players-data)
        _  (ingest-data players-data)
        coefs (linear/create-model db c/*fanduel* player-names)
        players-proj (linear/add-linear-projection db players-data coefs c/*fanduel*)
        ]
    (coinmp/lpsolve-solve-fanduel players-proj :linear-projection)))

(defn- optimize-fanduel-lineups-svm
  []
  (let [db (utils/get-db)
        players-data (data/init-players-data-fanduel)
        players-data (data/remove-injured players-data)
        player-names (map :Name players-data)
        _  (ingest-data players-data)
        o (svm/create-model db c/*fanduel* player-names)
        _ (println o)
        coefs (linear/create-model db c/*fanduel* player-names)
        players-proj (svm/predict-players db players-data c/*fanduel*)
        players-proj (linear/add-linear-projection db players-proj coefs c/*fanduel*)]
    (coinmp/lpsolve-solve-fanduel players-proj  :svm-projection)))

(defn- optimize-draftking-lineups
  []
  (let [db (utils/get-db)
        players-data (data/init-players-data-draftking2)
        players-data (data/remove-injured players-data)
        player-names (map :Name players-data)
        _  (ingest-data players-data)
        coefs (linear/create-model db c/*draftking* player-names)
        players-proj (linear/add-linear-projection db players-data coefs c/*draftking*)]
    (data/save-solutions
      (coinmp/lpsolve-solve-draftkings players-proj  :linear-projection)
      c/*draftking*)
    ;players-proj
    ))

(defn- optimize-draftking-lineups-svm
  []
  (let [db (utils/get-db)
        players-data (data/init-players-data-draftking2)
        players-data (data/remove-injured players-data)
        player-names (map :Name players-data)
        _  (ingest-data players-data)
        o (svm/create-model db c/*draftking* player-names)
        _ (println o)
        coefs (linear/create-model db c/*draftking* player-names)
        players-proj (svm/predict-players db players-data c/*draftking*)
        players-proj (linear/add-linear-projection db players-proj coefs c/*draftking*)]
    (coinmp/lpsolve-solve-draftkings players-proj  :svm-projection)))


(defn- optimize-draftking-lineups-avg
  []
  (let [db (utils/get-db)
        players-data (data/init-players-data-draftking2)
        players-data (data/remove-injured players-data)
        player-names (map :Name players-data)
        _  (ingest-data players-data)
        coefs (linear/create-model db c/*draftking* player-names)
        players-proj (linear/add-linear-projection db players-data coefs c/*draftking*)
        players-proj (data/add-rotowires-projection players-proj c/*draftking*)
        ;players-proj (rotoscrap/add-rotogrinders-projection players-proj c/*draftking*)
        players-proj (map (fn [{:keys [Name linear-projection roto-wire-projection] :as p}]
                            (assoc p :avg-proj (mean [linear-projection
                                                      (utils/nil->zero roto-wire-projection)])))
                          players-proj)]
    (data/save-solutions
      (coinmp/lpsolve-solve-draftkings players-proj  :avg-proj)
      c/*draftking*)))

;TODO: to timestamp database, to make sure old data is not being regressioned!

(defn save-projections-dk
  []
  (let [db (utils/get-db)
        players-data (data/init-players-data-draftking2)
        players-data (data/remove-injured players-data)
        _   (rotoscrap/ingest-data players-data)
        player-names (map :Name players-data)
        o (svm/create-model db c/*draftking* player-names)
        _ (println o)
        coefs (linear/create-model db c/*draftking* player-names)
        players-proj (svm/predict-players db players-data c/*draftking*)
        players-proj (linear/add-linear-projection db players-proj coefs c/*draftking*)
        players-proj (data/add-rotowires-projection players-proj c/*draftking*)]
    (data/save-projections db players-proj "02032015")))


(defn get-team
  []
  (let [db (utils/get-db)
        get-player-info (fn [name] (mc/find-one-as-map db c/*collection* {:Name name}))]
    (map get-player-info
         ["George Hill"
          "Victor Oladipo"
          "Aaron Gordon"
          "Kevin Love"
          "Lavoy Allen"
          "Jeremy Lin"
          "Michael Kidd-Gilchrist"
          "Russell Westbrook"]
         )))


(defn -main
  [& args]

  )

(defn data-for-player
  [Name]
  (let [db (utils/get-db)
        players-data (data/init-players-data-draftking2)
        pinfo (first (filter #(= Name (:Name %)) players-data))
        db-player (mc/find-one-as-map db c/*collection* {:Name Name})
        ]
    (model/predict-data-from-events pinfo db-player c/*draftking* :database :rotogrinder)))

; force-espn-update
; force-rotogrinders-update
; save-projections-dk
(comment
  (def db (utils/get-db))
  (def players (data/init-players-data-draftking2))
  (defn sqr [x] (* x x))

  (def players-data (data/remove-injured (data/init-players-data-draftking2)))
  (def player-names (map :Name players-data))
  (def espn-proj (let [coefs (linear/create-model db c/*draftking* player-names)
                       players-proj (linear/add-linear-projection db players-data coefs c/*draftking*)]
                   (map (fn [{:keys [Name linear-projection]}]
                          (let [{:keys [projections]}
                                (mc/find-one-as-map (utils/get-db) c/*collection* {:Name Name})]
                            [linear-projection
                             Name
                             (:last-actual projections)]))
                        players-proj)))


  (def espn-svm (let [o (svm/create-model db c/*draftking* player-names)
                      players-proj (svm/predict-players db players-data c/*draftking*)]
                   (map (fn [{:keys [Name svm-projection]}]
                          (let [{:keys [projections]}
                                (mc/find-one-as-map (utils/get-db) c/*collection* {:Name Name})]
                            [svm-projection
                             Name
                             (:last-actual projections)]))
                        players-proj)))


  (def pproj (filter #(some? (first %)) pproj))
  (println "linear error: " (reduce + (map (comp sqr -)
                                           (map #(nth % 0) pproj)
                                           (map last pproj))))


  (def high-errors (map first
                        (filter #(> (second %) 30)
                           (map #(list (second %)
                                       (abs (- (first %) (last %)))) pproj))))

  (def players-data (data/init-players-data-draftking2))
  (map
    (fn [Name]
      (let [pinfo (first (filter #(= Name (:Name %)) players-data))
            db-player (mc/find-one-as-map db c/*collection* {:Name Name})
            ]
        (model/predict-data-from-events pinfo db-player c/*draftking* :database c/*active-database*)))
    high-errors)

    )
