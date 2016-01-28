(ns rz.model.singular-linear
  (:require [rz.optimizers.constants :as c]
            [rz.optimizers.utils :as utils]
            [monger.operators :refer :all]
            [incanter.stats :refer :all]
            [incanter.core  :refer :all]
            [monger.collection :as mc]
            [clojure.pprint :as pp]
            [incanter.charts :as charts]))


(def db (utils/get-db))
(def player (mc/find-one-as-map db c/*collection* {:Name "Andrew Wiggins"}))

(defn get-feature-set
  [player]
  (let [{:keys [rotogrinder-events]} player
        sorted-events (sort-by :game-epoch rotogrinder-events)
        butlast-events (butlast sorted-events)
        home-events (filter #(= true (:home-game %)) butlast-events)
        away-events (filter #(= false (:home-game %)) butlast-events)
        iterations-cnt (dec (count butlast-events))
        ;iterations-cnt (- (min (count home-events) (count away-events)) c/*average-games-count*)
        get-pts (comp utils/nil->zero :fanduel-fpts)
        tdata-from-events (fn [history current]
                            (let [home-events (take-last c/*average-games-count*
                                                         (filter #(= true (:home-game %)) history))
                                  away-events (take-last c/*average-games-count*
                                                         (filter #(= false (:home-game %)) history))
                                  last-events (take-last c/*average-games-count* (map get-pts history))]
                              [
                               (utils/bool->int (:home-game current))
                               (utils/array->mean (map get-pts home-events))
                               ;(utils/bool->int (:home-game (last history)))
                               ;
                               ;(utils/array->mean (map get-pts history))
                               ;(utils/array->mean (map get-pts away-events))
                               ;(get-pts (last history))
                               ;(utils/array->mean last-events)
                               ;(utils/nil->zero2 (:mins (last history)))

                               (get-pts current)])
                              )]
    (loop [iteration 0
           events sorted-events
           result []]
      (if (> iteration iterations-cnt)
        result
        (recur (inc iteration) (butlast events) (conj result (tdata-from-events (butlast events) (last events)))))
      )

    ))


(defn create-player-model
  [player]
  (let [points (get-feature-set player)
        {:keys [coefs f-prob t-probs mse r-square]}
        (linear-model (map last points) (map #(take (dec (count (first points))) %) points))]
    (println (str "f-prob: " f-prob ", mse: " mse ", R^2: " r-square))
    (println "t-probs")
    (pp/pprint t-probs)
    (println "coefs")
    (pp/pprint coefs)
    coefs))

(defn draw-data
  [player]
  (let [points (get-feature-set player)
        coefs (create-player-model player)
        real-vals (map last points)
        projection (map #(loop [result 0 coef-a coefs point-a (cons 1 (butlast %))]
                          (if (empty? coef-a)
                            result
                            (recur (+ result (* (first coef-a) (first point-a)))
                                   (rest coef-a)
                                   (rest point-a))))
                        points)]
    (view (charts/scatter-plot real-vals projection :legend true))
    ;projection
    ))