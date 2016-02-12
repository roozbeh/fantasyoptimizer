(ns rz.model.model
  (:require [rz.optimizers.constants :as c]
            [monger.collection :as mc]
            [monger.operators :refer :all]
            [rz.optimizers.utils :as utils]
            [clojure.pprint :as pp]))


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


(defn- data-from-events-rotogrinder
  [{:keys [Name Position]} event-current events contest-provider]
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

     :current-home (get event-current :home-game -1)
     :event-cnt (count events)

     ;:home-events home-events
     ;:away-events away-events
     ;:all-events events

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

(defn- data-from-events-espn
  [{:keys [Name Position espn-data teamAbbrev Team] :as db-player} event-current events contest-provider]
  (let [ftps-keyword (get-point-function contest-provider)
        event-last (last events)
        home-events (filter #(= true (:home-game %)) events)
        away-events (filter #(= false (:home-game %)) events)
        last-home-event (last home-events)
        last-away-event (last away-events)

        filter-zero (fn [es] (filter #(not (= 0 (utils/nil->zero (ftps-keyword %)))) es))
        all-scores (map (comp utils/nil->zero ftps-keyword) (filter-zero events))
        home-scores (map (comp utils/nil->zero ftps-keyword) (filter-zero home-events))
        away-scores (map (comp utils/nil->zero ftps-keyword) (filter-zero away-events))

        avg-last-games (utils/array->mean (take-last c/*average-games-count* all-scores))
        avg-last-home-games (utils/array->mean (take-last c/*average-games-count* home-scores))
        avg-last-away-games (utils/array->mean (take-last c/*average-games-count* away-scores))]
    {:Name Name
     :last-event-mins (utils/nil->zero2 (:mins event-last))
     :last-event-pts (utils/nil->zero (ftps-keyword event-last))
     :last-event-points (utils/nil->zero2 (:points event-last))

     :last-home-event-mins (utils/nil->zero2 (:mins last-home-event))
     :last-home-event-pts (utils/nil->zero (ftps-keyword last-home-event))
     ;
     :last-away-event-mins (utils/nil->zero2 (:mins last-away-event))
     :last-away-event-pts (utils/nil->zero (ftps-keyword last-away-event))

     :non-zero-events (filter-zero events)
     :avg-last-games avg-last-games
     :avg-last-home-games avg-last-home-games
     :avg-last-away-games avg-last-away-games

     :current-home (get event-current :home-game -1)
     :event-cnt (count events)

     :season-salary (get espn-data :salary 500000)
     :experience (get espn-data :experience 1)
     :is-top (if (:leaderboard-rank espn-data)
               (utils/bool->int (< (:leaderboard-rank espn-data) 1))
               0)

     :G (utils/bool->int (or (= "PG" Position) (= "SG" Position)))
     :F (utils/bool->int (or (= "PF" Position) (= "SF" Position)))
     :C (utils/bool->int (= "C" Position))

     :home-events home-events
     :away-events away-events
     :home-scores home-scores
     :away-scores away-scores
     :all-scores (if (or (empty? all-scores)
                         (< (count all-scores) 2))
                   [0 0 0 0]
                   all-scores)

     :team-name (or teamAbbrev Team)
     :opp-name (:opp-team event-current)

     :is_win (if (= "W" (:match-status event-current)) 1 0)

     ;label -> only for train
     :pts-current (get event-current ftps-keyword -1)

     ;:last-salary (get event-last salary-keyword 0)
     ;:cur-salary  (if (nil? (salary-keyword event-current))
     ;               (:Salary event-current)
     ;               (salary-keyword event-current))
     ;:avg-salary (utils/array->mean (map salary-keyword events))
     }))



(defn- data-from-events
  [p current-event events cp database]
  (cond
    (= database :rotogrinder) (data-from-events-rotogrinder p current-event events cp)
    (= database :espn) (data-from-events-espn p current-event events cp)
    true (assert false (str "Unknown database " database))))

(defn- train-data-from-events
  [db-player sorted-events contest-provider database]
  (let [event-current (last sorted-events)
        events (butlast sorted-events)]
    (data-from-events db-player event-current events contest-provider database)))


(defn get-events-from-player
  [{:keys [rotogrinder-events espn-data] :as db-player} database]
  (cond
    (= database :rotogrinder) (sort-by :game-epoch rotogrinder-events)
    (= database :espn) (sort-by :game-epoch (:events espn-data))
    true (assert false (str "Unknown database " database))))


(defn predict-data-from-events
  [{:keys [Salary IsHome opp-team] :as pinfo} db-player contest-provider
   & {:keys [database] :or {database :rotogrinder}}]
  (let [sorted-events (get-events-from-player db-player database)]
    ;(pp/pprint pinfo)
    (data-from-events db-player
                    {:home-game IsHome
                     :Salary Salary
                     :opp-team opp-team
                     (get-point-function contest-provider) "-1"}
                    sorted-events
                    contest-provider
                    database)))

(defn load-players
  [db player-names]
  (if (nil? player-names)
    (mc/find-maps db c/*collection* {:rotogrinder-events { $exists true $not {$size 0} } })
    (mc/find-maps db c/*collection* {:Name {$in player-names}})))

(defn- prepare-data-for-regression-recursive
  [db contest-provider player-names iteration-max use-last database]
  (flatten
    (map
      (fn [{:keys [Name  teamAbbrev] :as db-player}]
        (let [
              all-sorted-events (get-events-from-player db-player database)

              ; BECAREFUL BECAREFUL BECAREFUL
              ;all-sorted-events (filter #(not (= (:game-date %) c/*filter-date*))
              ;                          all-sorted-events)
              ; BECAREFUL BECAREFUL BECAREFUL
              _ (if (nil? all-sorted-events)
                  (println (str "WARNING: no data for " Name)))
              sorted-events (if use-last all-sorted-events (butlast all-sorted-events))
              butlast-events (butlast sorted-events)
              home-events (filter #(= true (:home-game %)) butlast-events)
              away-events (filter #(= false (:home-game %)) butlast-events)
              iterations-cnt (- (min (count home-events) (count away-events)) c/*average-games-count*)
              ;iterations-cnt 1
              iterations-cnt (min iterations-cnt iteration-max)

              ]
          (loop [iteration 0
                 events sorted-events
                 result []]
            (if (> iteration iterations-cnt)
              result
              (recur (inc iteration)
                     (butlast events)
                     (conj result (train-data-from-events db-player events contest-provider database)))))))
      (load-players db player-names))))


(defn filter-23
  [players]
  (filter (fn [{:keys [pts-current cur-salary]}]
            (and (not (= -1 pts-current))
                 (not (= 0 cur-salary) )))
          players))


(defn prepare-data
  ([db contest-provider player-names & {:keys [iteration-max use-last database]
                                        :or {iteration-max 1000 use-last true database :rotogrinder}}]
   (filter-23
     (prepare-data-for-regression-recursive db contest-provider player-names
                                            iteration-max use-last database)))
  ([db contest-provider ]
   (prepare-data db contest-provider nil {})))


