(ns rz.model.linear
  (:require [monger.core :as mg]
            [monger.collection :as mc]
            [clojure.string :as string]
            [clojure.pprint :as pp]
            [rz.optimizers.constants :as c]
            ;[incanter.interpolation :refer :all]
            ))


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

(def ^:dynamic *average-games-count* 10)
(defn prepare-data-for-regression
  [db]
  (doall
    (map
      (fn [{:keys [Name rotogrinder-events]}]
        (let [event-23 (first (filter #(= "1/23/16" (:game-date %)) rotogrinder-events))
              event-last (first (take-last 2 (sort-by :game-epoch rotogrinder-events)))
              same-home-event (first
                                (take-last 2 (sort-by :game-epoch
                                                    (filter #(= (:home-game event-23) (:home-game %)) rotogrinder-events))))
              avg-last-games (array->mean (take *average-games-count*
                                                  (map (comp nil->zero :draftking-fpts)
                                                       (butlast (sort-by :game-epoch rotogrinder-events)))))
              avg-last-games-same (array->mean
                                      (take *average-games-count*
                                            (map (comp nil->zero :draftking-fpts)
                                                 (butlast (sort-by :game-epoch
                                                                   (filter #(= (:home-game event-23) (:home-game %))
                                                                           rotogrinder-events))))))]
          {:Name Name
           :home-23 (get event-23 :home-game -1)
           :pts-23 (get event-23 :draftking-fpts -1)
           :home-last (get event-last :home-game -1)
           :pts-last (get event-last :draftking-fpts -1)
           :home-same-home (get same-home-event :home-game -1)
           :pts-same-home (get same-home-event :draftking-fpts -1)
           :avg-games-pts avg-last-games
           :avg-games-pts-same avg-last-games-same
           }))
      (mc/find-maps db c/*collection* {}))))


(defn filter-23
  [players]
  (filter #(not (= -1 (:pts-23 %)))
          players))


(defn bool->int
  [x]
  (if x 1 0))

(defn create-array-for-regression
  [data]
  (map (fn [{:keys [Name home-23 home-last pts-last pts-23 home-same-home pts-same-home avg-games-pts avg-games-pts-same] :as d}]
         [
          ;(nil->zero pts-same-home)
          ;(nil->zero pts-last)
          avg-games-pts
          ;avg-games-pts-same
          (bool->int home-23)
          (nil->zero pts-23)])
       data))


(defn create-model
  []
  (let [points (create-array-for-regression (filter-23 (prepare-data-for-regression db)))
        {:keys [coefs f-prob t-probs]} (linear-model (map last points) (map #(take 2 %) points))]
    (println "f-prob")
    (pp/pprint f-prob)
    (println "t-probs")
    (pp/pprint t-probs)
    (println "coefs")
    (pp/pprint coefs)
    ))


; Using pts-last home-last home-23
  ;=> nil
  ;(:f-prob res)
  ;=> 1.1102230246251565E-16
  ;(:t-probs res)
  ;=> (7.340898253915817E-5 0.0 0.565767303406564 0.9332111795361318)
  ;(:coefs res)
  ;=> (7.4023312427253245 0.6158201067168376 1.1135389401649505 0.16273591723927439)

;Using   pts-last (= home-last home-23)
;  (:f-prob res)
;  => 1.1102230246251565E-16
;  (:t-probs res)
;  => (0.0023359408890313293 0.0 0.10855689344123265)
;  (:coefs res)
;  => (6.028908174029574 0.6066211621519323 3.1061597544245245)
;

;Using   pts-same-home-last pts-last
;(create-model)
;f-prob
;1.1102230246251565E-16
;t-probs
;(5.863357113833345E-6 0.02924995756798454 0.0543618130635426)
;coefs
;(7.271416941964365 0.3489688773013597 0.3125652299871504)
;=> nil
