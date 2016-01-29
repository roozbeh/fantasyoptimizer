(ns rz.model.model
  (:require [rz.optimizers.constants :as c]
            [monger.collection :as mc]
            [monger.operators :refer :all]
            [rz.optimizers.utils :as utils]))


(defn get-point-function
  [contest-provider]
  (if (= contest-provider c/*fanduel*)
    :fanduel-fpts
    :draftking-fpts))


(defn get-salary-function
  [contest-provider]
  (if (= contest-provider c/*fanduel*)
    :fd-salary
    :dk-salary))


(defn data-from-events
  [Name event-current events contest-provider]
  (let [ftps-keyword (get-point-function contest-provider)
        salary-keyword (get-salary-function contest-provider)
        event-last (last events)
        home-events (filter #(= true (:home-game %)) events)
        away-events (filter #(= false (:home-game %)) events)
        last-home-event (last home-events)
        last-away-event (last away-events)

        all-scores (map (comp utils/nil->zero ftps-keyword) events)
        home-scores (map (comp utils/nil->zero ftps-keyword) home-events)
        away-scores (map (comp utils/nil->zero ftps-keyword) away-events)


        avg-last-games (utils/array->mean (take-last c/*average-games-count* all-scores))
        avg-last-home-games (utils/array->mean (take-last c/*average-games-count* home-scores))
        avg-last-away-games (utils/array->mean (take-last c/*average-games-count* away-scores))

        ;avg-last-games7 (utils/array->mean (take-last c/*average-games-count7* all-scores))
        ;avg-last-home-games7 (utils/array->mean (take-last c/*average-games-count7* home-scores))
        ;avg-last-away-games7 (utils/array->mean (take-last c/*average-games-count7* away-scores))
        ]
    {:Name Name
     :last-event-mins (utils/nil->zero2 (:mins event-last))
     :last-event-pts (utils/nil->zero (ftps-keyword event-last))

     :last-home-event-mins (utils/nil->zero2 (:mins last-home-event))
     :last-home-event-pts (utils/nil->zero (ftps-keyword last-home-event))

     :last-away-event-mins (utils/nil->zero2 (:mins last-away-event))
     :last-away-event-pts (utils/nil->zero (ftps-keyword last-away-event))

     :avg-last-games avg-last-games
     :avg-last-home-games avg-last-home-games
     :avg-last-away-games avg-last-away-games

     ;:avg-last-games7 avg-last-games7
     ;:avg-last-home-games7 avg-last-home-games7
     ;:avg-last-away-games7 avg-last-away-games7

     :current-home (get event-current :home-game -1)
     :event-cnt (count events)

     :home-events home-events
     ;:away-events away-events

     :team-name (:team-name event-current)
     :opp-name (:opp-name event-current)

     ;label -> only for train
     :pts-current (get event-current ftps-keyword -1)

     :last-salary (get event-last salary-keyword 0)
     :cur-salary  (if (nil? (salary-keyword event-current))
                    (:Salary event-current)
                    (salary-keyword event-current))
     :avg-salary (utils/array->mean (map salary-keyword events))
     }))

(defn train-data-from-events
  [Name sorted-events contest-provider]
  (let [event-current (last sorted-events)
        events (butlast sorted-events)]
    (data-from-events Name event-current events contest-provider)))

(defn predict-data-from-events
  [{:keys [Name Salary IsHome]} sorted-events contest-provider]
  (data-from-events Name
                    {:home-game IsHome
                     :Salary Salary
                     (get-point-function contest-provider) "-1"}
                    sorted-events
                    contest-provider))

(defn prepare-data-for-regression-recursive
  [db contest-provider]
  (flatten
    (map
      (fn [{:keys [Name rotogrinder-events teamAbbrev]}]
        (let [sorted-events (sort-by :game-epoch rotogrinder-events)
              butlast-events (butlast sorted-events)
              home-events (filter #(= true (:home-game %)) butlast-events)
              away-events (filter #(= false (:home-game %)) butlast-events)
              iterations-cnt (- (min (count home-events) (count away-events)) c/*average-games-count*)
              ;iterations-cnt (min 3 iterations-cnt)
              ]
          (loop [iteration 0
                 events sorted-events
                 result []]
            (if (> iteration iterations-cnt)
              result
              (recur (inc iteration) (butlast events) (conj result (train-data-from-events Name events contest-provider)))))))
      (mc/find-maps db c/*collection* {:rotogrinder-events { $exists true $not {$size 0} } }))))


(defn filter-23
  [players]
  (filter #(and (not (= -1 (:pts-current %))))
          players))


(defn prepare-data
  [db contest-provider]
  (filter-23
    (prepare-data-for-regression-recursive db contest-provider)))


