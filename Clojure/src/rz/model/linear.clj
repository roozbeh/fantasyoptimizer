(ns rz.model.linear
  (:require [monger.core :as mg]
            [monger.collection :as mc]
            [monger.operators :refer :all]
            [clojure.string :as string]
            [clojure.pprint :as pp]
            [rz.optimizers.constants :as c]
            [incanter.stats :refer :all]
            [incanter.charts :as charts]
            [incanter.core :refer :all]
            [rz.optimizers.utils :as utils]
            [clojure.java.shell :as shell]
            [rz.model.model :as model]))


(defn create-array-for-regression
  [data]
  (map (fn [{:keys [pts-current last-event-mins last-event-pts last-home-event-mins
                    last-home-event-pts last-away-event-mins last-away-event-pts
                    avg-last-games avg-last-home-games avg-last-away-games avg-last-away-games
                    current-home event-cnt  home-events away-events
                    team-name opp-name last-salary cur-salary avg-salary] :as d}]
         [
          last-home-event-pts
          last-home-event-mins
          avg-last-away-games
          event-cnt
          cur-salary
          avg-salary
          last-salary

          (utils/nil->zero pts-current)])
       data))

(defn create-model
  [db contest-provider]
  (let [points (create-array-for-regression (model/prepare-data db contest-provider))
        ;points (take 10 points)
        {:keys [coefs f-prob t-probs mse r-square]}
        (linear-model (map last points) (map #(take (dec (count (first points))) %) points))]
    (println (str "f-prob: " f-prob ", mse: " mse ", R^2: " r-square))
    (println "t-probs")
    (pp/pprint t-probs)
    (println "coefs")
    (pp/pprint coefs)
    coefs))


(defn linear-proj
  [player pinfo coefs ftps-keyword]
  (let [{:keys [rotogrinder-events]} player
        sorted-events (sort-by :game-epoch rotogrinder-events)
        {:keys [last-home-event-mins last-home-event-pts avg-last-away-games event-cnt
                last-salary cur-salary avg-salary] :as d}
        (model/predict-data-from-events pinfo sorted-events ftps-keyword)]
    (+ (nth coefs 0)
       (* (nth coefs 1) last-home-event-pts)
       (* (nth coefs 2) last-home-event-mins)
       (* (nth coefs 3) avg-last-away-games)
       (* (nth coefs 4) event-cnt)
       (* (nth coefs 5) last-salary)
       (* (nth coefs 6) cur-salary)
       (* (nth coefs 7) avg-salary))))

(defn add-linear-projection
  [db players-data coefs contest-provider]
  (doall
    (map (fn [{:keys [Name] :as pinfo}]
           (assoc pinfo :linear-projection
                        (linear-proj
                          (mc/find-one-as-map db c/*collection* {:Name Name})
                          pinfo
                          coefs
                          (model/get-point-function contest-provider))))
         players-data)))

(defn draw-data
  [db contest-provider coefs]
  (let [ftps-keyword (if (= contest-provider c/*fanduel*) :fanduel-fpts :draftking-fpts)
        points (create-array-for-regression (model/prepare-data db contest-provider))
        real-vals (map last points)
        projection (map #(+ (nth coefs 0)
                    (* (nth coefs 1) (nth % 0))
                    (* (nth coefs 2) (nth % 1))
                    (* (nth coefs 3) (nth % 2))
                    (* (nth coefs 4) (nth % 3))) points)]
    (view (charts/scatter-plot real-vals projection :legend true))))

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


;avg-games-pts avg-games-pts-same (bool->int home-23)
;(create-model db)
;f-prob: 1.9554548980060815E-8, mse: 37.8719925645785
;t-probs
;(0.007042632598883047
;  2.8873167766674257E-5
;  0.02746031258908288
;  0.014404379055926064)
;coefs
;(4.941424337488041
;  1.2113201966280724
;  -0.5860032390414668
;  -4.372757353697194)
; proj = -4.9414 + 1.2113*avg-games-pts + -0.5860*avg-games-pts-same +  -4.3727 * (bool->int home-23)
