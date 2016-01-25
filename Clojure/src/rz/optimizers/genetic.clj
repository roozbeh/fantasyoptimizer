(ns rz.optimizers.genetic
  (:require [ga.core :as ga]
            [rz.data :as data]
            [rz.optimizers.constants :as constaints]
            [rz.optimizers.utils :as utils]))


(defonce players-data (data/add-rotowires-projection (data/init-players-data) constaints/*fanduel*))

(def ^:dynamic *salary-penalty* -1000000 )
(def ^:dynamic *equal-penalty* -1000000 )

(def ^:dynamic *score-weight* 30)
(def ^:dynamic *value-weight* 5)
(def ^:dynamic *salary-weight* 0.1)


(defn get-random-player
  ([pos]
   (rand-nth (filter #(and (= (:Position %) pos) (not (= "O" ((keyword "Injury Indicator") %)))) players-data)))
  ([pos player oplayer]
   (loop [player player]
     (let [rplayer (get-random-player pos)]
       (if (and
             (not (= (:Id player) (:Id rplayer)))
             (not (= (:Id oplayer) (:Id rplayer))))
         rplayer
         (recur player))))))

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
   :C (get-random-player "C")})

(defn- get-mirror-position
  [pos]
  (case pos
    :PG1 :PG2
    :PG2 :PG1
    :SG1 :SG2
    :SG2 :SG1
    :SF1 :SF2
    :SF2 :SF1
    :PF1 :PF2
    :PF2 :PF1
    :C nil))

(defn- other-team-player
  [team pos]
  (if (get-mirror-position pos)
    (get team (get-mirror-position pos))
    nil))

(defn mutator
  [tplayer team]
  (let [[tpos player] tplayer
        {:keys [Position]} player
        oplayer (other-team-player team tpos)]
    {tpos (get-random-player Position player oplayer)}))

(defn- get-equalness
  [team]
  (+
    (if (= (:Id (:PG1 team)) (:Id (:PG2 team))) 1 0)
    (if (= (:Id (:SG1 team)) (:Id (:SG2 team))) 1 0)
    (if (= (:Id (:SF1 team)) (:Id (:SF2 team))) 1 0)
    (if (= (:Id (:PF1 team)) (:Id (:PF2 team))) 1 0)))



(defn fitness
  [team]
  (let [
        ;fantasy-score (reduce + (map (comp read-string :FPPG)  (vals team)))
        fantasy-score (reduce + (map :roto-wire-projection (vals team)))
        fantasy-value (reduce + (map :roto-wire-value (vals team)))
        team-salary (reduce + (map :Salary (vals team)))
        salary-full (- team-salary constaints/*team-salary*)]
    (+
      fantasy-score
      ; (* *score-weight* fantasy-score)
      ;(* *value-weight* fantasy-value)
      ;(* *salary-weight* salary-full)

      (if (> team-salary constaints/*team-salary*) *salary-penalty* 0)
      (* (get-equalness team) *equal-penalty*))
    )
  )

(defn optimize-lineup
  []
  (let [best-yet (ga/evolve2 10000 0.01 init-value mutator fitness 100000)
        best-team (:value best-yet)]
    (println (str "Fitness: " (fitness best-team)))
    (utils/print-team best-team)
    (utils/print-team-metrics best-team)))
