(ns rz.optimizers.clocop
  (:require [clocop.constraints :as cs]
            [clocop.core :as clocop]
            [rz.data :as data]
            [rz.optimizers.constants :as constaints]))

(defonce players-data (data/add-rotowires-projection (data/init-players-data)))

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



                         (clocop/constrain! (cs/$>= constaints/*team-salary*
                                                    (cs/$weighted-sum L (map :Salary players-data))))

                         (println "Starting solver . . .")
                         (let [solved (clocop/solve!
                                        :minimize (cs/$weighted-sum L (players-scores))
                                        :timeout 1800
                                        :log? true)]
                           (keys (filter #(= (val %) 1) solved))
                           )))))
