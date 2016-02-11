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
            [rz.model.random-forest :as rtree]
            [rz.model.logistic :as logistic]
            [rz.optimizers.utils :as utils]
            [incanter.stats :refer :all]
            [monger.collection :as mc]
            [monger.operators :refer :all])
  (:gen-class))

(defn- force-espn-update
  []
  (espn/ingest-data (data/init-players-data-fanduel)
                         :force-update true))

(defn- force-rotogrinders-update
  []
  (rotoscrap/ingest-data (data/init-players-data-fanduel)
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
    (coinmp/lpsolve-solve-fanduel players-proj  :svm-projection)
    )
  )

(defn- optimize-fanduel-rtree
  []
  (let [db (utils/get-db)
        players-data (data/filter-suckers db
                                          (data/remove-injured
                                            (data/init-players-data-fanduel)))
        player-names (map :Name players-data)
        o (rtree/create-model db c/*fanduel* player-names)
        coefs (linear/create-model db c/*fanduel* player-names)
        players-proj (rtree/predict-players db players-data c/*fanduel*)
        players-proj (linear/add-linear-projection db players-proj coefs c/*fanduel*)
        ]
    (data/save-solutions
      (coinmp/lpsolve-solve-fanduel players-proj  :rtree-projection)
      c/*fanduel*)))


(defn- optimize-draftking-lineups
  []
  (let [db (utils/get-db)
        players-data (data/filter-suckers db
                                          (data/remove-injured
                                            (data/init-players-data-draftking2)))
        player-names (map :Name players-data)
        ;_  (ingest-data players-data)
        coefs (linear/create-model db c/*draftking* player-names)
        players-proj (linear/add-linear-projection db players-data coefs c/*draftking*)
        ]
    (data/save-solutions
      (coinmp/lpsolve-solve-draftkings players-proj  :linear-projection)
      c/*draftking*)
    ))

(defn- optimize-draftking-lineups-svm
  []
  (let [db (utils/get-db)
        players-data (data/filter-suckers db
                                          (data/remove-injured
                                            (data/init-players-data-draftking2)))
        player-names (map :Name players-data)
        ;_  (ingest-data players-data)
        o (svm/create-model db c/*draftking* player-names)
        _ (println o)
        coefs (linear/create-model db c/*draftking* player-names)
        players-proj (svm/predict-players db players-data c/*draftking*)
        players-proj (linear/add-linear-projection db players-proj coefs c/*draftking*)]
    ;(coinmp/lpsolve-solve-draftkings players-proj  :svm-projection)
    ;(map (fn [{:keys [Name svm-projection]}] [Name svm-projection]) players-proj)
    (data/save-solutions
      (coinmp/lpsolve-solve-draftkings players-proj  :svm-projection)
      c/*draftking*)

    ))

(defn- optimize-draftkings-rtree
  []
  (let [db (utils/get-db)
        players-data (data/filter-suckers db
                                          (data/remove-injured
                                            (data/init-players-data-draftking2)))
        player-names (map :Name players-data)
        o (rtree/create-model db c/*draftking* player-names)
        coefs (linear/create-model db c/*draftking* player-names)
        players-proj (rtree/predict-players db players-data c/*draftking*)
        players-proj (linear/add-linear-projection db players-proj coefs c/*draftking*)
        ]
    (data/save-solutions
      (coinmp/lpsolve-solve-draftkings players-proj  :rtree-projection)
      c/*draftking*)))


(defn- optimize-draftking-lineups-avg
  []
  (let [db (utils/get-db)
        players-data (data/filter-suckers db
                                          (data/remove-injured
                                            (data/init-players-data-draftking2)))
        player-names (map :Name players-data)
        _  (ingest-data players-data)
        coefs (linear/create-model db c/*draftking* player-names)
        players-proj (linear/add-linear-projection db players-data coefs c/*draftking*)
        players-proj (data/add-rotowires-projection players-proj c/*draftking*)
        players-proj (rotoscrap/add-rotogrinders-projection db players-proj c/*draftking*)
        players-proj (map (fn [{:keys [Name linear-projection roto-wire-projection rotogrinders-projection] :as p}]
                            (assoc p :avg-proj (mean [linear-projection
                                                      (utils/nil->zero roto-wire-projection)
                                                      (utils/nil->zero rotogrinders-projection)])))
                          players-proj)]
    (data/save-projections db players-proj)
    (data/save-solutions
      (coinmp/lpsolve-solve-draftkings players-proj  :avg-proj)
      c/*draftking*)))

;TODO: to timestamp database, to make sure old data is not being regressioned!

(defn save-actual
  [date-str]
  (let [db (utils/get-db)
        players-data (data/init-players-data-draftking2)
        player-names (map :Name players-data)]
    (data/save-actual db player-names date-str)))

(defn save-projections-dk
  []
  (let [db (utils/get-db)
        players-data (data/init-players-data-draftking2)
        players-data (data/remove-injured players-data)
        _   (rotoscrap/ingest-data players-data)
        player-names (map :Name players-data)
        coefs (linear/create-model db c/*draftking* player-names)
        players-proj (linear/add-linear-projection db players-data coefs c/*draftking*)
        players-proj (rotoscrap/add-rotogrinders-projection db players-proj c/*draftking*)
        players-proj (data/add-rotowires-projection players-proj c/*draftking*)]
    (data/save-projections db players-proj)))


(defn get-team
  []
  (let [db (utils/get-db)
        {:keys [actual]} (mc/find-one-as-map db c/*collection*{:type :actual :date "2/5/16"})
        ;get-player-info (fn [name] (mc/find-one-as-map db c/*collection* {:Name name}))
        get-player-info (fn [name] (first (filter #(= (first %) name) actual)))
        team (map get-player-info
                  [
                   "Greg Monroe"
                   "Nikola Jokic"
                   "LaMarcus Aldridge"
                   "Emmanuel Mudiay"
                   "Raul Neto"
                   "Giannis Antetokounmpo"
                   "Gordon Hayward"
                   "Khris Middleton"
                   ]
                  )]
    (println (str "sum is " (apply + (map (comp utils/nil->zero second) team))))
    team))


(defn -main
  [& args]

  )

(defn data-for-player
  [Name]
  (let [db (utils/get-db)
        players-data (data/init-players-data-draftking2)
        pinfo (first (filter #(= Name (:Name %)) players-data))
        db-player (mc/find-one-as-map db c/*collection* {:Name Name})]
    (model/predict-data-from-events pinfo db-player c/*draftking* :database :rotogrinder)))

(defn asses-linear
  []
  (let [db (utils/get-db)
        sqr (fn [x] (* x x))
        players-data (data/filter-suckers db
                       (data/remove-injured
                          (data/init-players-data-draftking2)))
        player-names (map :Name players-data)
        coefs (linear/create-model db c/*draftking* player-names)
        players-proj (linear/add-linear-projection db players-data coefs c/*draftking*)
        {:keys [actual]} (mc/find-one-as-map db c/*collection*{:type :actual :date "2/5/16"})
        liear-proj (map (fn [{:keys [Name linear-projection]}]
                          (let [[_ projection] (first (filter #(= Name (first %)) actual))]
                            [linear-projection Name (utils/nil->zero projection)]))
                        players-proj)]
    (println (str "database: " c/*active-database*))
    (println (str "max iteration: " c/*max-iterations*))
    (println "linear error: " (mean (map (comp sqr -)
                                             (map #(nth % 0) liear-proj)
                                             (map last liear-proj))))
    ;liear-proj
    ))


; force-espn-update
; force-rotogrinders-update
; save-projections-dk
(comment
  (def db (utils/get-db))
  (defn sqr [x] (* x x))
  (def players (data/init-players-data-draftking2))
  (def players-data (data/remove-injured (data/init-players-data-draftking2)))
  (def player-names (map :Name players-data))

  (data/save-actual db player-names "2/5/16")


  (def espn-proj (let [coefs (linear/create-model db c/*draftking* player-names)
                       players-proj (linear/add-linear-projection db players-data coefs c/*draftking*)
                       {:keys [actual]} (mc/find-one-as-map db c/*collection*{:type :actual :date "2/4/16"})]
                   (map (fn [{:keys [Name linear-projection]}]
                          (let [[_ projection] (first (filter #(= Name (first %)) actual))]
                            [linear-projection
                             Name
                             projection]))
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
