(ns rz.optimizer
  (:require [rz.data :as data]
            [rz.optimizers.constants :as c]
            [rz.optimizers.nba :as nba]
            [rz.optimizers.nhl :as nhl]
            [rz.scrap.rotogrinder :as rotoscrap]
            [rz.scrap.espn :as espn]
            [rz.model.model :as model]
            [rz.model.linear :as linear]
            [rz.model.svm :as svm]
            [rz.model.xgboost :as xgboost]
            [rz.model.random-forest :as rtree]
            [rz.optimizers.utils :as utils]
            [incanter.stats :refer :all]
            [monger.operators :refer :all])
  (:gen-class))

(defonce db (utils/get-db))

(defn- force-espn-update
  []
  (espn/ingest-data (data/init-players-data-draftking2)
                         :force-update true))

(defn- force-rotogrinders-update
  []
  (rotoscrap/ingest-data (data/init-players-data-draftking2)
                    :force-update true))

(defn- optimize-nba-draftkings-rtree
  [prod-run & {:keys [ranking] :or {ranking true}}]
  (binding [c/*active-sport* c/*nba*
            c/*active-database* :espn
            c/*collection* c/*nba*
            c/*solution-cnt* 2
            c/*max-common-players-draftkings* 4]
    (if prod-run
      (force-espn-update))
    (let [db (utils/get-db)
          players-data
          (data/filter-roto-suckers
                         (data/filter-suckers db
                                              (data/remove-injured
                                                (data/init-players-data-draftking2)))
                      )
          player-names (map :Name players-data)
          o (rtree/create-model db c/*draftking* player-names)       ;nil -> all players in training TODO
          coefs (linear/create-model db c/*draftking* player-names)
          players-proj (rtree/predict-players db players-data c/*draftking*)
          players-proj (model/rank-players players-proj :rtree-projection)
          players-proj (linear/add-linear-projection db players-proj coefs c/*draftking*)]
      (data/save-projections db players-proj)
      (data/save-solutions
        (nba/lpsolve-solve-draftkings players-proj  (if ranking
                                                      :rank
                                                      :rtree-projection))
        c/*draftking*))))


(defn- optimize-nba-draftkings-highrisk
  [prod-run]
  (binding [c/*active-sport* c/*nba*
            c/*active-database* :espn
            c/*collection* c/*nba*
            c/*solution-cnt* 16
            c/*max-common-players-draftkings* 7
            c/*average-games-count* 6]
    (if prod-run
      (force-espn-update))
    (let [db (utils/get-db)
          players-data
            (data/filter-roto-suckers
              (data/filter-suckers db
                               (data/remove-injured
                                 (data/init-players-data-draftking2)))
      )
      ;    players-data (filter #(and (not (= (:Name %) "Kris Humphries")))
      ;                         players-data)
          players-proj (model/add-max-last-score db players-data c/*draftking*)]
      (data/save-solutions
        (nba/lpsolve-solve-draftkings players-proj :max-score)
        c/*draftking*))))

(defn- optimize-draftking-lineups-xgboost
  [prod-run]
  (binding [c/*active-sport* c/*nba*
            c/*active-database* :espn
            c/*collection* c/*nba*
            c/*solution-cnt* 5
            c/*max-common-players-draftkings* 7
            c/*average-games-count* 6]
    (if prod-run
      (force-espn-update))
    (let [db (utils/get-db)
          players-data (data/filter-suckers db
                                          (data/filter-roto-suckers
                                            (data/remove-injured
                                              (data/init-players-data-draftking2))
                                            ))
          player-names (map :Name players-data)
          o (xgboost/create-model db c/*draftking* player-names)
          coefs (linear/create-model db c/*draftking* player-names)
          players-proj (xgboost/predict-players db players-data c/*draftking*)
          players-proj (linear/add-linear-projection db players-proj coefs c/*draftking*)]
      (data/save-solutions
        (nba/lpsolve-solve-draftkings players-proj  :xg-projection)
        c/*draftking*))))

(defn- optimize-nhl-draftkings-rtree
  [prod-run & {:keys [ranking] :or {ranking true}}]
  (if prod-run
    (force-rotogrinders-update))
  (binding [c/*active-sport* c/*nhl*
            c/*active-database* :rotogrinder
            c/*collection* c/*nhl*
            c/*max-common-players-draftkings* 8]
    (let [db (utils/get-db)
          players-data (data/remove-injured
                         (data/filter-roto-suckers
                           (data/filter-unknown db
                                                (data/init-players-data-draftking2))))
          player-names (map :Name players-data)
          o (rtree/create-model db c/*draftking* player-names)
          coefs (linear/create-model db c/*draftking* player-names)
          players-proj (rtree/predict-players db players-data c/*draftking*)
          players-proj (model/rank-players players-proj :rtree-projection)
          players-proj (linear/add-linear-projection db players-proj coefs c/*draftking*)]
      (data/dump-projs players-proj)
      (data/save-projections db players-proj)
      (data/save-solutions
        (nhl/lpsolve-solve-draftkings players-proj  (if ranking
                                                      :rank
                                                      :rtree-projection))
        c/*draftking*))))
