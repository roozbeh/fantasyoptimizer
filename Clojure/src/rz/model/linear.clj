(ns rz.model.linear
  (:require [monger.core :as mg]
            [monger.collection :as mc]
            [clojure.string :as string]
            [clojure.pprint :as pp]
            [rz.optimizers.constants :as c]
            [incanter.stats :refer :all]
            [rz.optimizers.utils :as utils]
            ))





(defn prepare-data-for-regression
  [db]
  (doall
    (map
      (fn [{:keys [Name rotogrinder-events teamAbbrev]}]
        (let [
              ;event-23 (first (filter #(= "1/23/16" (:game-date %)) rotogrinder-events))
              event-23 (last (sort-by :game-epoch rotogrinder-events))
              event-last (first (take-last 2 (sort-by :game-epoch rotogrinder-events)))
              same-home-event (first
                                (take-last 2 (sort-by :game-epoch
                                                    (filter #(= (:home-game event-23) (:home-game %)) rotogrinder-events))))
              avg-last-games (array->mean (take c/*average-games-count*
                                                  (map (comp nil->zero :draftking-fpts)
                                                       (butlast (sort-by :game-epoch rotogrinder-events)))))
              avg-last-games-same (array->mean
                                      (take c/*average-games-count*
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
           :team teamAbbrev
           :last-mins (nil->zero2 (:mins event-last))
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
  (map (fn [{:keys [Name home-23 home-last pts-last pts-23 home-same-home pts-same-home avg-games-pts
                    avg-games-pts-same team last-mins ] :as d}]
         [
          ;Name
          (nil->zero pts-same-home)
          ;(nil->zero pts-last)
          avg-games-pts
          ;avg-games-pts-same
          ;(bool->int home-23)
          last-mins
          (nil->zero pts-23)])
       data))


(defn create-model
  [db]
  (let [points (create-array-for-regression (filter-23 (prepare-data-for-regression db)))
        ;points (take 10 points)
        {:keys [coefs f-prob t-probs mse]} (linear-model (map last points) (map #(take 3 %) points))]
    (println (str "f-prob: " f-prob ", mse: " mse))
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


;(nil->zero pts-same-home)
;avg-games-pts
;f-prob
;1.1102230246251565E-16
;t-probs
;(0.03669788580371924 1.5024297053134461E-6 1.5585395214401387E-5)
;coefs
;(3.6397526591668594 0.3972861212063936 0.4668909583139573)


;(nil->zero pts-last)
;avg-games-pts-same
;f-prob
;1.1102230246251565E-16
;t-probs
;(0.06306100755158917 8.662927110547614E-6 1.518576000059113E-6)
;coefs
;(3.200219697399106 0.37004687710858475 0.5224217425481577)
;=> nil


;(nil->zero pts-last) avg-games-pts-same last-mins
;t-probs
;(0.660394276942686 0.034551317865409104 9.496754261872908E-6 0.031113606043747133)
;coefs
;(0.8859043630724841 0.21453203536554621 0.47082481896006056 0.279957487050174)


;(nil->zero pts-same-home) avg-games-pts-same last-mins
;f-prob: 1.1102230246251565E-16, mse: 105.84230100275667
;t-probs
;(0.6863555748118519
;  0.00892484545071448
;  7.68947279286003E-5
;  0.01231147116575082)
;coefs
;(0.7996943555639291
;  0.25292699886717784
;  0.4068511947197373
;  0.29838682660863114)

;(nil->zero pts-same-home) avg-games-pts-same last-mins
;(create-model db)
;f-prob: 1.1102230246251565E-16, mse: 105.84230100275667
;t-probs
;(0.6863555748118519
;  0.00892484545071448
;  7.68947279286003E-5
;  0.01231147116575082)
;coefs
;(0.7996943555639291
;  0.25292699886717784
;  0.4068511947197373
;  0.29838682660863114)


;(nil->zero pts-same-home) avg-games-pts last-mins
;(create-model db)
;f-prob: 1.1102230246251565E-16, mse: 99.70907704751458
;t-probs
;(0.9471246781571976
;  0.023732945210597878
;  1.7119639039719914E-13
;  0.006996301913531955)
;coefs
;(-0.08447465571860846
;  0.1622856937216861
;  0.5987724591707235
;  0.22011035581444016)
; proj = -0.0845 + 0.1623*(nil->zero pts-same-home) + 0.5988*avg-games-pts + 0.2201 * last-mins
