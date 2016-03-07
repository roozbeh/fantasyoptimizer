(ns rz.optimizers.report
  (:require [clojure.pprint :as pp]
            [monger.collection :as mc]
            [rz.optimizers.constants :as c]
            [rz.scrap.espn :as espn]
            [rz.scrap.rotogrinder :as rotogrinder]
            [incanter.stats :refer :all]
            [rz.optimizers.utils :as utils]))

(defn calc-totals
  [stated-team]
  (let
    [tkeys (filter #(not (contains? #{:name :Pos :home? :injury :Avg :StdDev :Min :Max :Team} %))
                   (keys (first stated-team)))]
    (assoc (apply array-map
                  (flatten
                    (for [key tkeys]
                      (let [total (reduce +
                                          (map #(if (nil? (get % key))
                                                 0
                                                 (if (string? (get % key))
                                                   (read-string (get % key))
                                                   (if number?
                                                     (get % key)
                                                     0)))
                                               stated-team))]
                        [key (if (float? total)
                               (format "%2.2f" total)
                               total)]))))
      :name "Total")))


(defn calc-team-stats
  [db team]
  (map (fn [{:keys [Name IsHome Position Salary roto-wire-projection FPPG injury
                    my-projection TeamAbbrev GameInfo
                    linear-projection svm-projection rotogrinders-projection rtree-projection]
             :as pinfo}]
         (let [db-player (mc/find-one-as-map db c/*collection* {:Name Name})
               events (sort-by :game-epoch (cond
                                             (= c/*active-database* :rotogrinder) (:rotogrinder-events db-player)
                                             (= c/*active-database* :espn) (:events (:espn-data db-player))))
               last-event (last events)
               scores (map (comp utils/nil->zero :draftking-fpts) events)
               scores (if (empty? scores) [0] scores)
               ]
           {:name       Name
            :Pos        Position
            :Sal        Salary
            :FPPG       (cond
                          (= c/*active-database* :espn) (format "%2.2f" (utils/array->mean (map :draftking-fpts (:events (:espn-data db-player)))))
                          (= c/*active-database* :rotogrinder) (format "%2.2f" (utils/array->mean (map :draftking-fpts (:rotogrinder-events db-player)))))
            :LinProj    (if (nil? linear-projection) 0 (format "%2.2f" linear-projection))
            :SVMProj    (if (nil? svm-projection) 0 svm-projection)
            :TreeProj   (if (nil? rtree-projection) 0 rtree-projection)
            :Roto       (if (nil? roto-wire-projection) "0" roto-wire-projection)
            :XG         (get pinfo :xg-projection "0")
            :Grndr      (if (nil? rotogrinders-projection) "0" rotogrinders-projection)
            :Last       (:draftking-fpts last-event)
            :home?      (if IsHome "HOME" "")
            :Team TeamAbbrev
            ;:Avg (mean scores)
            :StdDev     (format "%2.2f" (sd scores))
            :MinSc      (apply min scores)
            :MaxSc      (apply max scores)
            :minutes    (:mins (last events))
            ;:real-fd (espn/get-player-score-memo Name :fanduel-fpts "Wed 2/10/2016")
            ;:real-dk (espn/get-player-score-memo Name :draftking-fpts "Thu 3/3/2016")
            ;:real-dk (or (rotogrinder/get-player-score-memo Name :draftking-fpts "2016-02-26")
            ;             (rotogrinder/get-player-score-memo Name :draftking-fpts "2016-02-25"))
            ;(str "G " (:game-date last-event))

            ;      (str (:draftking-fpts last-event) " " (if (:home-game last-event) "H" "A"))
            ;:Home (if (some? (re-find (re-pattern (str "@" teamAbbrev)) GameInfo)) "YES " "")
            :injury     injury
            }))
       (sort-by :Position team)))


(defn print-team2
  [db team]
  (let [stated-team (calc-team-stats db team)
        column [:home? :Pos :name :FPPG :minutes :StdDev :Last :LinProj :injury :Sal]
        column [:home? :Pos :name :Last :LinProj :injury :Sal :Team ]
        add-column (fn [column kw kwd2]
                     (if (some? (kw (first team)))
                       (conj column kwd2)
                       column))
        column (add-column column :svm-projection :SVMProj)
        column (add-column column :rotogrinders-projection :Grndr)
        column (add-column column :roto-wire-projection :Roto)
        column (add-column column :rtree-projection :TreeProj)
        column (add-column column :xg-projection :XG)
        ;column (conj column :real-dk)
        ]
    (pp/print-table
      column
      (concat []
              stated-team
              [(calc-totals stated-team)]))))

(defn print-team
  [team]
  (print-team2 (vals team)))

