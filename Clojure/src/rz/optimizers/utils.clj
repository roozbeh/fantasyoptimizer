(ns rz.optimizers.utils
  (:require [clojure.pprint :as pp]
            [net.cgrand.enlive-html :as html]
            [rz.optimizers.constants :as c]
            [monger.core :as mg]
            [incanter.stats :refer :all]
            [monger.collection :as mc]))

(defn nil->zero
  [x]
  (if (nil? x)
    0
    (if (string? x)
      (read-string x)
      x)))

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


(defn get-db  []
  (mg/get-db (mg/connect) c/*db-name*))



;(defn print-team-metrics
;  [team]
;  (println (str "Team Salary: " (reduce + (map :Salary (vals team)))))
;  (println (str "Team Sum FPPG: " (reduce + (map :FPPG (vals team)))))
;  (println (str "Team Sum Projection: " (reduce + (map :roto-wire-projection (vals team)))))
;  )

(defn fetch-url [url]
  (html/html-resource (java.net.URL. url)))



