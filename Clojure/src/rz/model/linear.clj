(ns rz.model.linear
  (:require [monger.core :as mg]
            [monger.collection :as mc]
            [monger.operators :refer :all]
            [clojure.string :as string]
            [clojure.pprint :as pp]
            [rz.optimizers.constants :as c]
            [incanter.stats :refer :all]
            [incanter.charts :as charts]
            [incanter.core  :refer :all]
            [rz.optimizers.utils :as utils]
            [clojure.java.shell :as shell]
            ))


;(defn prepare-data-for-regression
;  [db]
;  (doall
;    (map
;      (fn [{:keys [Name rotogrinder-events teamAbbrev]}]
;        (let [
;              ;event-23 (first (filter #(= "1/23/16" (:game-date %)) rotogrinder-events))
;              event-23 (last (sort-by :game-epoch rotogrinder-events))
;              event-last (first (take-last 2 (sort-by :game-epoch rotogrinder-events)))
;              same-home-event (first
;                                (take-last 2 (sort-by :game-epoch
;                                                    (filter #(= (:home-game event-23) (:home-game %)) rotogrinder-events))))
;              avg-last-games (utils/array->mean (take c/*average-games-count*
;                                                  (map (comp utils/nil->zero :draftking-fpts)
;                                                       (butlast (sort-by :game-epoch rotogrinder-events)))))
;              avg-last-games-same (utils/array->mean
;                                      (take c/*average-games-count*
;                                            (map (comp utils/nil->zero :draftking-fpts)
;                                                 (butlast (sort-by :game-epoch
;                                                                   (filter #(= (:home-game event-23) (:home-game %))
;                                                                           rotogrinder-events))))))]
;          {:Name Name
;           :home-23 (get event-23 :home-game -1)
;           :pts-23 (get event-23 :draftking-fpts -1)
;           :home-last (get event-last :home-game -1)
;           :pts-last (get event-last :draftking-fpts -1)
;           :home-same-home (get same-home-event :home-game -1)
;           :pts-same-home (get same-home-event :draftking-fpts -1)
;           :avg-games-pts avg-last-games
;           :avg-games-pts-same avg-last-games-same
;           :team teamAbbrev
;           :last-mins (utils/nil->zero2 (:mins event-last))
;           }))
;      (mc/find-maps db c/*collection* {}))))
;
;
;(defn filter-23
;  [players]
;  (filter #(and (not (= -1 (:pts-23 %)))
;                (not (= 0 (utils/nil->zero (:pts-23 %))))
;                (not (= 0 (utils/nil->zero (:pts-same-home %))))
;                (not (= 0 (utils/nil->zero (:pts-last %))))
;                (not (<= 10 (utils/nil->zero (:pts-last %))))
;                )
;          players))
;
;
;(defn create-array-for-regression
;  [data]
;  (map (fn [{:keys [Name home-23 home-last pts-last pts-23 home-same-home pts-same-home avg-games-pts
;                    avg-games-pts-same team last-mins ] :as d}]
;         [
;          ;Name
;          ;(utils/nil->zero pts-same-home)
;          ;(utils/nil->zero pts-last)
;          avg-games-pts
;          avg-games-pts-same
;          (utils/bool->int home-23)
;          ;last-mins
;          ;(+ -0.0845
;          ;   (* 0.1623 (utils/nil->zero pts-same-home))
;          ;   (* 0.5988 avg-games-pts)
;          ;   (* 0.2201 last-mins))
;          (utils/nil->zero pts-23)])
;       data))
;(nil->zero pts-same-home) avg-games-pts last-mins

(defn data-from-events
  [Name event-current events ftps-keyword]
  (let [event-last (last events)
        home-events (filter #(= true (:home-game %)) events)
        away-events (filter #(= false (:home-game %)) events)
        last-home-event (last home-events)
        last-away-event (last away-events)
        avg-last-games (utils/array->mean (take-last c/*average-games-count*
                                                (map (comp utils/nil->zero ftps-keyword)
                                                     events)))

        avg-last-home-games (utils/array->mean (take-last c/*average-games-count*
                                                     (map (comp utils/nil->zero ftps-keyword)
                                                          home-events)))

        avg-last-away-games (utils/array->mean (take-last c/*average-games-count*
                                                     (map (comp utils/nil->zero ftps-keyword)
                                                          away-events)))]
    {:Name Name
     :last-event-mins (utils/nil->zero2 (:mins event-last))
     :last-event-pts (utils/nil->zero (ftps-keyword event-last))

     :last-home-event-mins (utils/nil->zero2 (:mins last-home-event))
     :last-home-event-pts (utils/nil->zero (ftps-keyword last-home-event))

     :last-away-event-mins (utils/nil->zero2 (:mins last-away-event))
     :last-away-event-pts (utils/nil->zero (ftps-keyword last-away-event))

     :avg-last-games avg-last-games
     :avg-last-home-games avg-last-home-games
     :avg-last-away-games avg-last-away-games

     :current-home (get event-current :home-game -1)
     :event-cnt (count events)

     :home-events home-events
     :away-events away-events

     ;label -> only for train
     :pts-current (get event-current ftps-keyword -1)
     }))

(defn train-data-from-events
  [Name sorted-events ftps-keyword]
  (let [event-current (last sorted-events)
        events (butlast sorted-events)]
    (data-from-events Name event-current events ftps-keyword)))

(defn predict-data-from-events
  [Name sorted-events ftps-keyword]
  (data-from-events Name {:home-game -1 ftps-keyword -1} sorted-events ftps-keyword))

(defn prepare-data-for-regression-recursive
  [db ftps-keyword]
  (flatten
    (map
      (fn [{:keys [Name rotogrinder-events teamAbbrev]}]
        (let [sorted-events (sort-by :game-epoch rotogrinder-events)
              butlast-events (butlast sorted-events)
              home-events (filter #(= true (:home-game %)) butlast-events)
              away-events (filter #(= false (:home-game %)) butlast-events)
              iterations-cnt (- (min (count home-events) (count away-events)) c/*average-games-count*)
              ;iterations-cnt (max 3 iterations-cnt)
              ]
          (loop [iteration 0
                 events sorted-events
                 result []]
            (if (> iteration iterations-cnt)
              result
              (recur (inc iteration) (butlast events) (conj result (train-data-from-events Name events ftps-keyword)))))))
      (mc/find-maps db c/*collection* {:rotogrinder-events { $exists true $not {$size 0} } }))))


(defn filter-23
  [players]
  (filter #(and (not (= -1 (:pts-current %))))
          players))

(defn create-array-for-regression
  [data]
  (map (fn [{:keys [pts-current last-event-mins last-event-pts last-home-event-mins
                    last-home-event-pts last-away-event-mins last-away-event-pts
                    avg-last-games avg-last-home-games avg-last-away-games avg-last-away-games
                    current-home event-cnt  home-events away-events] :as d}]
         [
          last-home-event-pts
          last-home-event-mins
          avg-last-away-games
          event-cnt

          ;last-event-pts
          ;avg-last-home-games
          ;avg-last-games
          ;last-away-event-pts
          ;last-away-event-mins
          ;last-event-mins
          ;(utils/bool->int current-home)

          (utils/nil->zero pts-current)])
       data))

(defn create-model
  [db contest-provider]
  (let [ftps-keyword (if (= contest-provider c/*fanduel*) :fanduel-fpts :draftking-fpts)
        points (create-array-for-regression (filter-23 (prepare-data-for-regression-recursive db ftps-keyword)))
        ;points (take 10 points)
        {:keys [coefs f-prob t-probs mse r-square] :as out} (linear-model (map last points) (map #(take (dec (count (first points))) %) points))]
    (println (str "f-prob: " f-prob ", mse: " mse ", R^2: " r-square))
    (println "t-probs")
    (pp/pprint t-probs)
    (println "coefs")
    (pp/pprint coefs)
    coefs))


(defn get-projection-for-player
  [player coefs ftps-keyword]
  (let [{:keys [Name teamAbbrev GameInfo rotogrinder-events]} player
        sorted-events (sort-by :game-epoch rotogrinder-events)
        {:keys [pts-current last-event-mins last-event-pts last-home-event-mins
                last-home-event-pts last-away-event-mins last-away-event-pts
                avg-last-games avg-last-home-games avg-last-away-games avg-last-away-games
                current-home event-cnt  home-events away-events] :as d}
        (predict-data-from-events Name sorted-events ftps-keyword)]
    (+ (nth coefs 0)
       (* (nth coefs 1) last-home-event-pts)
       (* (nth coefs 2) last-home-event-mins)
       (* (nth coefs 3) avg-last-away-games)
       (* (nth coefs 4) event-cnt))))

(defn add-linear-projection
  [db players-data coefs contest-provider]
  (let [ftps-keyword (if (= contest-provider c/*fanduel*) :fanduel-fpts :draftking-fpts)]
    (doall
      (map (fn [{:keys [Name] :as pinfo}]
             (assoc pinfo :my-projection
                          (get-projection-for-player
                            (mc/find-one-as-map db c/*collection* {:Name Name})
                            coefs
                            ftps-keyword)))
           players-data))))


(defn draw-data
  [db contest-provider coefs]
  (let [ftps-keyword (if (= contest-provider c/*fanduel*) :fanduel-fpts :draftking-fpts)
        points (create-array-for-regression (filter-23 (prepare-data-for-regression-recursive db ftps-keyword)))
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
