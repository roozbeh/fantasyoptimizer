(ns rz.model.model
  (:require [rz.optimizers.constants :as c]
            [monger.collection :as mc]
            [monger.operators :refer :all]
            [rz.optimizers.utils :as utils]))


(defn data-from-events
  [Name event-current events ftps-keyword]
  (let [event-last (last events)
        home-events (filter #(= true (:home-game %)) events)
        away-events (filter #(= false (:home-game %)) events)
        last-home-event (last home-events)
        last-away-event (last away-events)
        avg-last-games (utils/array->mean (take-last c/*average-games-count*
                                                     (map (comp utils/nil->zero ftps-keyword)
                                                          events)))

        avg-last-home-games (utils/array->mean (take-last c/*average-games-count*
                                                          (map (comp utils/nil->zero ftps-keyword)
                                                               home-events)))

        avg-last-away-games (utils/array->mean (take-last c/*average-games-count*
                                                          (map (comp utils/nil->zero ftps-keyword)
                                                               away-events)))]
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

     :current-home (get event-current :home-game -1)
     :event-cnt (count events)

     :home-events home-events
     :away-events away-events

     :team-name (:team-name event-current)
     :opp-name (:opp-name event-current)

     ;label -> only for train
     :pts-current (get event-current ftps-keyword -1)
     }))

(defn train-data-from-events
  [Name sorted-events ftps-keyword]
  (let [event-current (last sorted-events)
        events (butlast sorted-events)]
    (data-from-events Name event-current events ftps-keyword)))

(defn predict-data-from-events
  [Name sorted-events ftps-keyword]
  (data-from-events Name {:home-game -1 ftps-keyword "-1"} sorted-events ftps-keyword))

(defn prepare-data-for-regression-recursive
  [db ftps-keyword]
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
              (recur (inc iteration) (butlast events) (conj result (train-data-from-events Name events ftps-keyword)))))))
      (mc/find-maps db c/*collection* {:rotogrinder-events { $exists true $not {$size 0} } }))))


(defn filter-23
  [players]
  (filter #(and (not (= -1 (:pts-current %))))
          players))


(defn get-point-function
  [contest-provider]
  (if (= contest-provider c/*fanduel*)
    :fanduel-fpts
    :draftking-fpts))

(defn prepare-data
  [db contest-provider]
  (filter-23
    (prepare-data-for-regression-recursive db (get-point-function contest-provider))))


