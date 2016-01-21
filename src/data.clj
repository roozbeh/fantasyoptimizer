(ns data
  (:require [clojure.java.io :as io]
            [clojure.data.csv :as csv]
            [clojure.pprint :as pp]
            [clojure.math.combinatorics :as combo]
            [ga.core :as ga]
             ))

(def players-csv "fantasy_players_jan_20.csv")
(def projections-csv "projections.csv")
;(read-json-data "src/server/fantasy_players.csv")
;(util/read-csv-with-reader players-csv)


(defn init-players-data
  []
  (let [data (with-open [in-file (io/reader players-csv)]
               (doall
                 (csv/read-csv in-file)))
        header (first data)
        label-rec (fn [r] (zipmap (map keyword header) r))]
    (map label-rec (rest data))))

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

; WARNING: run every time globally
(def players-data (add-projection (init-players-data) (init-projection-data)))

(def team {
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

(defn get-random-player
  [pos]
  (rand-nth (filter #(and (= (:Position %) pos) (not (= "O" ((keyword "Injury Indicator") %)))) players-data)))

(defn init-value
  []
  {
   :PG1 (get-random-player "PG")
   :PG2 (get-random-player "PG")
   :SG1 (get-random-player "SG")
   :SG2 (get-random-player "SG")
   :SF1 (get-random-player "SF")
   :SF2 (get-random-player "SF")
   :PF1 (get-random-player "PF")
   :PF2 (get-random-player "PF")
   :C (get-random-player "C")
   }
  ;(take *genom-size* (repeatedly #(char (ga/rand-in-range 32 126))))
  )

(defn mutator
  [tplayer]
  (let [[tpos player] tplayer
        {:keys [Position]} player]
    {tpos (get-random-player Position)}))
;(defn mutator
;  [c]
;  (if c
;    (let [new-val ((if (> (rand) 0.5) + -)
;                    (rand-int 5) (int c))]
;      (char (cond
;              (> new-val 126) 126
;              (< new-val 32) 32
;              :else new-val)))
;    (char (ga/rand-in-range 32 126))))

(def ^:dynamic *team-salary* 60000 )
(def ^:dynamic *salary-penalty* -1000 )
(def ^:dynamic *equal-penalty* -1000 )

(defn fitness
  [team]
  (let [
        ;fantasy-score (reduce + (map (comp read-string :FPPG)  (vals team)))
        fantasy-score (reduce + (map (comp read-string :projection)  (vals team)))
        team-salary (reduce + (map (comp read-string :Salary)  (vals team)))
        equalness (if (= (:Id (:PG1 team)) (:Id (:PG2 team))) 1 0)
        equalness (if (= (:Id (:SG1 team)) (:Id (:SG2 team))) (inc equalness) equalness)
        equalness (if (= (:Id (:SF1 team)) (:Id (:SF2 team))) (inc equalness) equalness)
        equalness (if (= (:Id (:PF1 team)) (:Id (:PF2 team))) (inc equalness) equalness)
        ]
    (+ fantasy-score
       (if (> team-salary *team-salary*) *salary-penalty* 0)
       (* equalness *equal-penalty*))
    )
  )

(defn print-team
  [team]
  (pp/pprint
    (map (fn [[role p]]
           {:name (str ((keyword "First Name") p) " " ((keyword "Last Name") p))
            :Pos (:Position p)
            :Sal (:Salary p)
            ;:Pts (:FPPG p)
            :Proj (:projection p)
            ;:Injury (str ((keyword "Injury Indicator") p) " - " ((keyword "Injury Details") p))
            } )
       team
         )))


(defn print-team-salary
  [team]
  (println (str "Team: " (reduce + (map (comp read-string :Salary)  (vals team))))))

(defn evolve-string
  []
  (let [best-yet (ga/evolve2 1000 0.01 init-value mutator fitness 100)
        best-team (:value best-yet)]
    (println (str "Fitness: " (fitness best-team)))
    (print-team best-team)
    (print-team-salary best-team)

    )
)