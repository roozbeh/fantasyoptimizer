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
            [rz.model.model :as model]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io]))


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
                    season-salary last-event-points Name experience C G F
                    is-top is_win
                    ] :as d}]
         ;(println (str Name " -> " C " - " G " - " F))
         (if (= :espn c/*active-database*)
           [;espn
            last-event-mins                                 ;OK 2.062967785287917E-4
            avg-last-home-games                             ;OK 2.6898705485223218E-11
            avg-last-away-games                             ;OK 7.215051357323254E-5
            season-salary                                   ;OK 4.266601842450868E-5
            ;C                                               ;OK 0.007628189945130481
            (nth (reverse all-scores) 1)                    ;~ 0.08884594435811466
            ;last-event-points                               ;BAD 0.13423766210192656
            ;(utils/bool->int current-home)                  ;BAD 0.16421015361380098)
            ;avg-last-games                                  ;BAD 0.9926613836842875
            is-top                                          ;BAD 0.3040758717420722
            last-event-pts                                  ;BAD 0.7107637646273752
            experience                                      ;BAD 0.3321079046177915
            ;is_win


            (utils/nil->zero pts-current)]
           [;rotogrinder
            avg-last-games
            last-home-event-mins
            avg-last-away-games
            event-cnt
            avg-salary
            last-salary
            (utils/bool->int current-home)

            (utils/nil->zero pts-current)]))
       data))

(defn create-model
  ([db contest-provider player-names]
   (let [points (create-array-for-regression
                  (model/prepare-data db contest-provider player-names
                                      :iteration-max c/*max-iterations*
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
                avg-last-games current-home season-salary
                experience C G F is-top last-event-points] :as d}
        (model/predict-data-from-events pinfo player ftps-keyword
                                        :database c/*active-database*)
        points (butlast
                 (first
                   (create-array-for-regression
                     [(model/predict-data-from-events pinfo player ftps-keyword
                                                      :database c/*active-database*)])))]
    (loop [points (cons 1 points)
           coefs coefs
           sum 0]
      (assert (= (count points) (count coefs)))
      (if (empty? coefs)
        sum
        (recur (rest points) (rest coefs) (+ sum (* (first points) (first coefs))))))))
    ;(pp/pprint points )
    ;
    ;(if (= :espn c/*active-database*)
    ;  (+ (nth coefs 0)
    ;     (* (nth coefs 1) last-event-mins)
    ;     (* (nth coefs 2) avg-last-home-games)
    ;     (* (nth coefs 3) avg-last-away-games)
    ;     (* (nth coefs 4) (nth (reverse all-scores) 1))
    ;     (* (nth coefs 5) experience)
    ;     (* (nth coefs 6) last-event-points)
    ;     (* (nth coefs 7) last-event-pts)
    ;     (* (nth coefs 8) avg-last-games)
    ;     (* (nth coefs 9) season-salary)
    ;     (* (nth coefs 10) C)
    ;     (* (nth coefs 11) is-top)
    ;     (* (nth coefs 22) (utils/bool->int current-home))
    ;
    ;
    ;     )
    ;  (+ (nth coefs 0)
    ;     (* (nth coefs 1) avg-last-games)
    ;     (* (nth coefs 2) last-home-event-mins)
    ;     (* (nth coefs 3) avg-last-away-games)
    ;     (* (nth coefs 4) event-cnt)
    ;     (* (nth coefs 5) avg-salary)
    ;     (* (nth coefs 6) last-salary)
    ;     (* (nth coefs 7) (utils/bool->int current-home))
    ;     )
    ;
    ;   ;(* (nth coefs 1) avg-last-games)
    ;   ;(* (nth coefs 2) last-home-event-mins)
    ;   ;(* (nth coefs 3) event-cnt)
    ;   ;(* (nth coefs 4) cur-salary)
    ;   ;(* (nth coefs 5) avg-salary)
    ;
    ;   ;(* (nth coefs 1) last-home-event-pts)
    ;   ;(* (nth coefs 2) last-home-event-mins)
    ;   ;(* (nth coefs 3) avg-last-away-games)
    ;   ;(* (nth coefs 4) event-cnt)
    ;   ;(* (nth coefs 5) last-salary)
    ;   ;(* (nth coefs 6) cur-salary)
    ;   ;(* (nth coefs 7) avg-salary)
    ;
    ;   )))


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



; -------------- across projections ------------------
;(defn load-yesterday-proj-and-actual
;  [db players-data]
;  (let [names (map :Name players-data)
;        db-players (mc/find-maps db c/*collection* {:name { $in names } })]
;    (map #(assoc % :actual (read-string (:actual %)))
;         (filter #(and (some? %) (some? (:actual %)))
;                 (map (fn [{:keys [projections Name rotogrinder-events]}]
;                        (assoc (:01302015 projections) :Name Name
;                                                   :actual (:draftking-fpts
;                                                             (first
;                                                               (filter #(= (:game-date %) "1/29/16")
;                                                                       rotogrinder-events)))))
;                      db-players)))))
;
;
(defn prepare-projections-data
  [db]
  (let [proj  (:proj (mc/find-one-as-map db c/*collection* {:type :projections :date "02062016"}))
        actual (:actual (mc/find-one-as-map db c/*collection* {:type :actual :date "2/6/16"}))
        ]
    (map (fn [{:keys [Name linear-projection rotogrinders-projection roto-wire-projection]}]
                  (let [a (filter #(= Name (first %)) actual)
                        actual (if (empty? a)
                                 0
                                 (second (first a)))]
                    [
                     linear-projection
                     (utils/nil->zero rotogrinders-projection)
                     (utils/nil->zero roto-wire-projection)
                     (utils/nil->zero actual)]))
                proj)))

(defn create-cross-proj-model
  [db]
  (let [points (prepare-projections-data db)
        {:keys [coefs f-prob t-probs mse r-square]}
          (linear-model (map last points) (map #(take (dec (count (first points))) %) points))]
    (println (str "f-prob: " f-prob ", mse: " mse ", R^2: " r-square))
    (println "t-probs")
    (pp/pprint t-probs)
    (println "coefs")
    (pp/pprint coefs)
    coefs))

(defn draw-proj-data
  [db]
  (let [points (prepare-projections-data db)
        real-vals (map last points)
        mine (map #(nth % 0) points)
        ]
    (doto
      (charts/scatter-plot real-vals mine :legend true)
      (charts/add-function (fn [n] n) -5 80)
      view)))
;
;(defn draw-data
;  [db contest-provider coefs]
;  (let [ftps-keyword (if (= contest-provider c/*fanduel*) :fanduel-fpts :draftking-fpts)
;        points (create-array-for-regression (model/prepare-data db contest-provider))
;        real-vals (map last points)
;        projection (map #(+ (nth coefs 0)
;                            (* (nth coefs 1) (nth % 0))
;                            (* (nth coefs 2) (nth % 1))
;                            (* (nth coefs 3) (nth % 2))
;                            (* (nth coefs 4) (nth % 3))) points)]
;    (view (charts/scatter-plot real-vals projection :legend true))))
;
