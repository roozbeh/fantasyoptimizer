(ns rz.optimizers.utils
  (:require [clojure.pprint :as pp]
            [net.cgrand.enlive-html :as html]
            [rz.optimizers.constants :as c]
            [monger.core :as mg]
            [incanter.stats :refer :all]
            [monger.collection :as mc]))

(defn- get-total
  [team]
  [
   {:name "Total"
    :Position ""
    :Salary (reduce + (map :Salary team))
    :FPPG (reduce + (map :FPPG team))
    :Proj (reduce + (map :my-projection team))
    :Value (/ (reduce + (map :my-projection team)) (count team))
    }
   ])

(defn get-db  []
  (mg/get-db (mg/connect) c/*db-name*))

(defn print-team2
  [team]
  (pp/print-table
    (map (fn [{:keys [name Position Salary roto-wire-projection roto-wire-value FPPG injury my-projection teamAbbrev GameInfo]}]
           {:name name
            :Pos Position
            :Sal Salary
            :FPPG FPPG
            :Proj (format "%02.2f" my-projection)
            ;:Roto roto-wire-projection
            :LastGame (:draftking-fpts
                        (last (sort-by :game-epoch
                                       (:rotogrinder-events (mc/find-one-as-map (get-db) c/*collection* {:Name name})))))
            :Home (if (some? (re-find (re-pattern (str "@" teamAbbrev)) GameInfo)) "YES " "")
            :injury injury
            } )
         team))
  (pp/print-table (get-total team)))

(defn print-team
  [team]
  (print-team2 (vals team)))


(defn print-team-metrics
  [team]
  (println (str "Team Salary: " (reduce + (map :Salary (vals team)))))
  (println (str "Team Sum FPPG: " (reduce + (map :FPPG (vals team)))))
  (println (str "Team Sum Projection: " (reduce + (map :roto-wire-projection (vals team)))))
  )

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

