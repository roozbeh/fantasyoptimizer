(ns rz.optimizers.utils
  (:require [clojure.pprint :as pp]
            [net.cgrand.enlive-html :as html]
            [rz.optimizers.constants :as c]
            [monger.core :as mg]
            [incanter.stats :refer :all]
            [monger.collection :as mc]))

(defn get-db  []
  (mg/get-db (mg/connect) c/*db-name*))

(defn calc-team-stats
  [team]
  (map (fn [{:keys [Name IsHome Position Salary roto-wire-projection FPPG injury my-projection teamAbbrev GameInfo
                    linear-projection svm-projection]}]
         (let [db-player (mc/find-one-as-map (get-db) c/*collection* {:Name Name})
               events (sort-by :game-epoch (:rotogrinder-events db-player))
               last-event (last events)
               by-last (first (take-last 2 events))
               scores (map (comp read-string :draftking-fpts) events)
               scores (if (empty? scores) [0] scores)
               ]
           {:name Name
            :Pos Position
            :Sal Salary
            :FPPG FPPG
            :LinProj (if (nil? linear-projection) 0 (format "%2.2f" linear-projection))
            :SVMProj (if (nil? svm-projection) 0 svm-projection)
            :Roto roto-wire-projection
            :Last (:draftking-fpts last-event)
            :home? (if IsHome "HOME" "")
            ;:Avg (mean scores)
            :StdDev (format "%2.2f" (sd scores))
            :Min (apply min scores)
            :Max  (apply max scores)
            ;(str "G " (:game-date last-event))

            ;      (str (:draftking-fpts last-event) " " (if (:home-game last-event) "H" "A"))
            ;:Home (if (some? (re-find (re-pattern (str "@" teamAbbrev)) GameInfo)) "YES " "")
            :injury injury
            }))
       (sort-by :Position team)))



(defn calc-totals
  [stated-team]
  (let
    [tkeys (filter #(not (contains? #{:name :Pos :home? :injury :Avg :StdDev :Min :Max} %))
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

(defn print-team2
  [team]
  (let [stated-team (calc-team-stats team)]
    (pp/print-table
        (concat stated-team
                [(calc-totals stated-team)]))))

(defn print-team
  [team]
  (print-team2 (vals team)))


;(defn print-team-metrics
;  [team]
;  (println (str "Team Salary: " (reduce + (map :Salary (vals team)))))
;  (println (str "Team Sum FPPG: " (reduce + (map :FPPG (vals team)))))
;  (println (str "Team Sum Projection: " (reduce + (map :roto-wire-projection (vals team)))))
;  )

(defn fetch-url [url]
  (html/html-resource (java.net.URL. url)))



(defn nil->zero
  [x]
  (if (nil? x)
    0
    (read-string x)))

(defn array->mean
  [x]
  (if (not (empty? x))
    (mean x)
    0))

(defn nil->zero2
  [x]
  (if (nil? x)
    0
    x))

(defn bool->int
  [x]
  (if x 1 0))

