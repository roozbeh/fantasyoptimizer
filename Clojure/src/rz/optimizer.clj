(ns rz.optimizer
  (:require [clojure.java.io :as io]
            [clojure.data.csv :as csv]
            [clojure.pprint :as pp]
            [clojure.math.combinatorics :as combo]
            [ga.core :as ga]
            [net.cgrand.enlive-html :as html]
            [rz.projection :as proj]
            [rz.data :as data]
            [clocop.constraints :as cs]
            [clocop.core :as clocop]
            )
  (:gen-class))

(def ^:dynamic *team-salary* 60000 )
(def ^:dynamic *salary-penalty* -1000000 )
(def ^:dynamic *equal-penalty* -1000000 )

(def ^:dynamic *score-weight* 30)
(def ^:dynamic *value-weight* 5)
(def ^:dynamic *salary-weight* 0.1)

(defonce players-data (data/add-rotowires-projection (data/init-players-data)))

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
        salary-full (- team-salary *team-salary* )]
    (+
      fantasy-score
       ; (* *score-weight* fantasy-score)
       ;(* *value-weight* fantasy-value)
       ;(* *salary-weight* salary-full)

       (if (> team-salary *team-salary*) *salary-penalty* 0)
       (* (get-equalness team) *equal-penalty*))
    )
  )

(defn- get-total
  [team]
  [
   {:name "Total"
    :Position ""
    :Salary (reduce + (map :Salary (vals team)))
    :FPPG (reduce + (map :FPPG (vals team)))
    :Proj (reduce + (map :roto-wire-projection (vals team)))
    :Value (/ (reduce + (map :roto-wire-value (vals team))) (count team))
    }
    ])

(defn print-team
  [team]
  (pp/print-table
    (map (fn [[role {:keys [name Position Salary roto-wire-projection roto-wire-value FPPG injury]}]]
           {:name name
            :Pos Position
            :Sal Salary
            :FPPG FPPG
            :Proj roto-wire-projection
            :value roto-wire-value
            :injury injury
            } )
         team
         ))
  (pp/print-table (get-total team)))


(defn print-team-metrics
  [team]
  (println (str "Team Salary: " (reduce + (map :Salary (vals team)))))
  (println (str "Team Sum FPPG: " (reduce + (map :FPPG (vals team)))))
  (println (str "Team Sum Projection: " (reduce + (map :roto-wire-projection (vals team)))))
  )

(defn optimize-lineup
  []
  (let [best-yet (ga/evolve2 10000 0.01 init-value mutator fitness 100000)
        best-team (:value best-yet)]
    (println (str "Fitness: " (fitness best-team)))
    (print-team best-team)
    (print-team-metrics best-team)))





(defn- get-player
  [name]
  (first (filter (fn [p] (re-find (re-pattern (str name ".*")) (:name p))) players-data)))

(defn get-team
  []
  {
   :PG1 (get-player "Brandon Knight")
   :PG2 (get-player "Jeff Teague")
   :SG1 (get-player "Devin Booker")
   :SG2 (get-player "Kyle Korver")
   :SF1 (get-player "LeBron James")
   :SF2 (get-player "P.J. Tucker")
   :PF1 (get-player "Anthony Davis")
   :PF2 (get-player "Kevin Love")
   :C (get-player "Marc Gasol")})

(comment
  (print-team (get-team))
  )


(defn -main
  [& args]
  (optimize-lineup))


;(defn magic-series
;  [N]
;  (with-store (store)
;              (let [L (vec (for [i (range N)]
;                             (int-var (str i) 0 N)))] ; initialize L to be a vector of vars
;                (doseq [i (range N)]
;                  (constrain! ($= ($occurrences L i)
;                                  (nth L i)))) ; L[i] = # of times i occurs in L
;
;                ; This is a redundant constraint, i.e. a constraint that doesn't change the feasibility of the problem
;                ; but makes the solving faster: summation(i=0..N | i * L[i]) = N. (Think about it!)
;                (constrain! ($= ($weighted-sum L (range N)) N))
;
;                (let [solved (solve!)]
;                  (map solved (map str (range 0 N)))))))

(defn player-desc
  [p]
  (str (:Position p) " " (:name p) " " (:Salary p)))

(defn role-vars
  [role L]
  (loop [index 0
         players players-data
         rL []]
    (if (empty? players)
      rL
      (recur (inc index)
             (rest players)
             (if (= (:Position (first players)) role)
                (conj rL (nth L index))
                rL)))))

(defn- players-scores
  []
  (map #(- (int (* 100 (:roto-wire-projection %)))) players-data))

(defn new-optimize
  []
  (let [N (count players-data)]
    (clocop/with-store (clocop/store)
                (let [L (vec (for [i (range N)]
                               (clocop/int-var (player-desc (nth players-data i)) 0 1)))]
                  (clocop/constrain! (cs/$= 9 (apply cs/$+ L)))

                  (clocop/constrain! (cs/$= 2 (apply cs/$+ (role-vars "PG" L))))
                  (clocop/constrain! (cs/$= 2 (apply cs/$+ (role-vars "SG" L))))
                  (clocop/constrain! (cs/$= 2 (apply cs/$+ (role-vars "SF" L))))
                  (clocop/constrain! (cs/$= 2 (apply cs/$+ (role-vars "PF" L))))



                  (clocop/constrain! (cs/$>= *team-salary*
                                             (cs/$weighted-sum L (map :Salary players-data))))

                  (println "Starting solver . . .")
                  (let [solved (clocop/solve!
                                 :minimize (cs/$weighted-sum L (players-scores))
                                 :timeout 1800
                                 :log? true)]
                    (keys (filter #(= (val %) 1) solved))
                     )))))
