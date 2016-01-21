(ns rz.data
  (:require [clojure.java.io :as io]
            [clojure.data.csv :as csv]
            [clojure.pprint :as pp]
            [clojure.math.combinatorics :as combo]
            [net.cgrand.enlive-html :as html]
            [rz.projection :as proj]
             ))

(def players-csv "fantasy_players_nba_jan_21.csv")
(def projections-csv "projections.csv")
;(read-json-data "src/server/fantasy_players.csv")
;(util/read-csv-with-reader players-csv)


(defn- fix-pdata-keywords
  [pdatas]
  (map (fn [p]
         (assoc p :name   (str ((keyword "First Name") p) " " ((keyword "Last Name") p))
                  :injury ((keyword "Injury Indicator") p)
                  :Salary (read-string (:Salary p))
                  :FPPG (read-string (:FPPG p))))
       pdatas))


(defn init-players-data
  []
  (let [data (with-open [in-file (io/reader players-csv)]
               (doall
                 (csv/read-csv in-file)))
        header (first data)
        label-rec (fn [r] (zipmap (map keyword header) r))]
    (fix-pdata-keywords (map label-rec (rest data)))))

(defn init-projection-data
  []
  (let [data (with-open [in-file (io/reader projections-csv)]
               (doall
                 (csv/read-csv in-file)))
        header (first data)
        label-rec (fn [r] (zipmap (map keyword header) r))]
    (map label-rec (rest data))))

(defn add-projection
  [player-data projection-data]
  (map (fn [p]
         (let [rname (re-pattern (str ((keyword "First Name") p) ".*" ((keyword "Last Name") p) ".*"))
               projection (filter (fn [pd] (re-find rname (:Player pd))) projection-data)]
           (if (empty? projection)
             (do
               (println (str "ERROR Could not find projection for " rname))
               (assoc p :projection (str (* (read-string (:FPPG p)) 0.5))))
             (do
               (assoc p :projection (:Value (first projection)))))))
       player-data))


(defn add-rotowires-projection
  [players-data]
  (let [rotowires-data (proj/get-rotowires-projections)]
    (map (fn [p]
           (let [projection (filter (fn [pd] (re-find (re-pattern  (str (:name p) ".*"))
                                                      (:title pd)))  rotowires-data)]
             (if (empty? projection)
                (do
                  (println (str "ERROR Could not find projection for " (:name p)))
                  (assoc p
                    :roto-wire-projection (:FPPG p)
                    :roto-wire-value (/ (* 1000 (:FPPG p)) (:Salary p)))
                  )
                (do
                  (assoc p
                    :roto-wire-projection (read-string (:points (first projection)))
                    :roto-wire-value (read-string (:value (first projection))))))))
         players-data)))

; WARNING: run every time globally
(def nba-team {
           :PG 2
           :SG 2
           :SF 2
           :PF 2
           :C 1
           })

(defn role-cnt
  [body role]
  (count
    (filter #(= (:Position %) role) body)))

(defn roles-cnt
  [body]
  {
   :PG (role-cnt body "PG")
   :SG (role-cnt body "SG" )
   :SF (role-cnt body "SF")
   :PF (role-cnt body "PF")
   :C  (role-cnt body "C")
   })

