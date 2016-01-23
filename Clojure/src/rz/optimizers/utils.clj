(ns rz.optimizers.utils
  (:require [clojure.pprint :as pp]))

(defn- get-total
  [team]
  [
   {:name "Total"
    :Position ""
    :Salary (reduce + (map :Salary team))
    :FPPG (reduce + (map :FPPG team))
    :Proj (reduce + (map :roto-wire-projection team))
    :Value (/ (reduce + (map :roto-wire-value team)) (count team))
    }
   ])

(defn print-team2
  [team]
  (pp/print-table
    (map (fn [{:keys [name Position Salary roto-wire-projection roto-wire-value FPPG injury]}]
           {:name name
            :Pos Position
            :Sal Salary
            :FPPG FPPG
            :Proj roto-wire-projection
            :value roto-wire-value
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

