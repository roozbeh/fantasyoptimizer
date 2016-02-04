(ns rz.model.linear
  (:require [monger.core :as mg]
            [monger.collection :as mc]
            [monger.operators :refer :all]
            [clojure.string :as string]
            [clojure.pprint :as pp]
            [rz.optimizers.constants :as c]
            [incanter.stats :refer :all]
            [incanter.charts :as charts]
            [incanter.core :refer :all]
            [rz.optimizers.utils :as utils]
            [clojure.java.shell :as shell]
            [rz.model.model :as model]))


(defn create-array-for-regression
  [data]
  (map (fn [{:keys [pts-current last-home-event-mins
                    last-home-event-pts avg-last-away-games
                    event-cnt last-salary cur-salary avg-salary
                    last-event-mins last-event-pts
                    avg-last-games current-home
                    isPG isSG isSF isPF isC
                    last-away-event-mins last-away-event-pts
                    avg-last-home-games
                    all-events all-scores
                    ] :as d}]
         [
          ;espn
          avg-last-games
          last-event-mins
          avg-last-home-games
          avg-last-away-games
          (nth (reverse all-scores) 1)

          ;rotogrinder
          ;avg-last-games
          ;last-home-event-mins
          ;avg-last-away-games
          ;event-cnt
          ;avg-salary
          ;last-salary
          ;(utils/bool->int current-home)

          ;last-home-event-pts

          (utils/nil->zero pts-current)])
       data))

(defn create-model
  ([db contest-provider player-names]
   (let [points (create-array-for-regression
                  (model/prepare-data db contest-provider player-names
                                      :database c/*active-database*))
         ;points (take 10 points)
        {:keys [coefs f-prob t-probs mse r-square]}
        (linear-model (map last points) (map #(take (dec (count (first points))) %) points))]
    (println (str "f-prob: " f-prob ", mse: " mse ", R^2: " r-square))
    (println (str "input data size: " (count points)))
    (println "t-probs")
    (pp/pprint t-probs)
    (println "coefs")
    (pp/pprint coefs)
    coefs))
  ([db contest-provider]
   (create-model db contest-provider nil)))

(defn linear-proj
  [player pinfo coefs ftps-keyword]
  (let [
        {:keys [last-home-event-mins last-home-event-pts avg-last-away-games event-cnt
                last-salary cur-salary avg-salary last-event-pts
                last-event-mins avg-last-home-games all-scores
                avg-last-games current-home] :as d}
        (model/predict-data-from-events pinfo player ftps-keyword
                                        :database c/*active-database*)]
    (+ (nth coefs 0)
       (* (nth coefs 1) avg-last-games)
       (* (nth coefs 2) last-event-mins)
       (* (nth coefs 3) avg-last-home-games)
       (* (nth coefs 4) avg-last-away-games)
       (* (nth coefs 5) (nth (reverse all-scores) 1))


       ;(* (nth coefs 1) avg-last-games)
       ;(* (nth coefs 2) last-home-event-mins)
       ;(* (nth coefs 3) avg-last-away-games)
       ;(* (nth coefs 4) event-cnt)
       ;(* (nth coefs 5) avg-salary)
       ;(* (nth coefs 6) last-salary)
       ;(* (nth coefs 7) (utils/bool->int current-home))

       ;(* (nth coefs 1) avg-last-games)
       ;(* (nth coefs 2) last-home-event-mins)
       ;(* (nth coefs 3) event-cnt)
       ;(* (nth coefs 4) cur-salary)
       ;(* (nth coefs 5) avg-salary)

       ;(* (nth coefs 1) last-home-event-pts)
       ;(* (nth coefs 2) last-home-event-mins)
       ;(* (nth coefs 3) avg-last-away-games)
       ;(* (nth coefs 4) event-cnt)
       ;(* (nth coefs 5) last-salary)
       ;(* (nth coefs 6) cur-salary)
       ;(* (nth coefs 7) avg-salary)

       )))

(defn add-linear-projection
  [db players-data coefs contest-provider]
  (doall
    (map (fn [{:keys [Name] :as pinfo}]
           (assoc pinfo :linear-projection
                        (linear-proj
                          (mc/find-one-as-map db c/*collection* {:Name Name})
                          pinfo
                          coefs
                          (model/get-point-function contest-provider))))
         players-data)))

(defn draw-data
  [db contest-provider coefs]
  (let [ftps-keyword (if (= contest-provider c/*fanduel*) :fanduel-fpts :draftking-fpts)
        points (create-array-for-regression (model/prepare-data db contest-provider))
        real-vals (map last points)
        projection (map #(+ (nth coefs 0)
                    (* (nth coefs 1) (nth % 0))
                    (* (nth coefs 2) (nth % 1))
                    (* (nth coefs 3) (nth % 2))
                    (* (nth coefs 4) (nth % 3))) points)]
    (view (charts/scatter-plot real-vals projection :legend true))))



; -------------- across projections ------------------
(defn load-yesterday-proj-and-actual
  [db players-data]
  (let [names (map :Name players-data)
        db-players (mc/find-maps db c/*collection* {:name { $in names } })]
    (map #(assoc % :actual (read-string (:actual %)))
         (filter #(and (some? %) (some? (:actual %)))
                 (map (fn [{:keys [projections Name rotogrinder-events]}]
                        (assoc (:01302015 projections) :Name Name
                                                   :actual (:draftking-fpts
                                                             (first
                                                               (filter #(= (:game-date %) "1/29/16")
                                                                       rotogrinder-events)))))
                      db-players)))))


(defn prepare-projections-data
  [data]
  (map (fn [{:keys [actual linear-projection svm-projection roto-wire-projection]}]
         [
          linear-projection
          svm-projection
          roto-wire-projection
          actual])
       (filter #(some? (:roto-wire-projection %)) data)))

(defn create-cross-proj-model
  [db players-data]
  (let [points (prepare-projections-data
                 (load-yesterday-proj-and-actual db players-data))
        {:keys [coefs f-prob t-probs mse r-square]}
          (linear-model (map last points) (map #(take (dec (count (first points))) %) points))]
    (println (str "f-prob: " f-prob ", mse: " mse ", R^2: " r-square))
    (println "t-probs")
    (pp/pprint t-probs)
    (println "coefs")
    (pp/pprint coefs)
    coefs))

(defn draw-proj-data
  [db players-data]
  (let [points (prepare-projections-data
                 (load-yesterday-proj-and-actual db players-data))
        coefs (create-cross-proj-model db players-data)
        real-vals (map last points)
        ;projection (map #(+ (nth coefs 0)
        ;                    (* (nth coefs 1) (nth % 0))
        ;                    (* (nth coefs 2) (nth % 1))
        ;                    ) points)
        ;linear (map #(nth % 0) points)
        ;svm (map #(nth % 1) points)
        roto (map #(nth % 2) points)
        ]
    (doto
      (charts/scatter-plot real-vals roto :legend true)
      (charts/add-function (fn [n] n) -5 80)
      view "linear")))
