(ns rz.model.random-forest
  (:require [rz.model.model :as model]
            [rz.optimizers.constants :as c]
            [clojure.java.shell :as shell]
            [clojure.java.io :as io]
            [clojure.data.csv :as csv]
            [rz.optimizers.utils :as utils]
            [monger.collection :as mc]
            [clojure.string :as string]))

(def ^:dynamic *rtree-train-file*  "../rtree_train.csv")
(def ^:dynamic *rtree-model-file*  "../rtree_model.xml")
(def ^:dynamic *rtree-predict-file*  "../rtree_predict.csv")
(def ^:dynamic *rtree-output-file*  "../rtree_output.csv")

(def ^:dynamic *rtree-bin*  "../Regression/random_forest/rtree_reg")

(defn create-array-for-regression
  [data]
  (map (fn [{:keys [pts-current last-home-event-mins
                    last-home-event-pts avg-last-away-games
                    event-cnt last-salary cur-salary avg-salary
                    last-event-mins last-event-pts
                    avg-last-games current-home
                    isPG isSG isSF isPF isC
                    last-away-event-mins last-away-event-pts
                    avg-last-home-games
                    all-events all-scores
                    season-salary last-event-points Name experience C G F
                    is-top is_win non-zero-events
                    opp-name team-name
                    home-events away-events home-scores away-scores

                    ] :as d}]
         ;(println (str Name " -> " C " - " G " - " F))
         (if (= :espn c/*active-database*)
           [;originals
            ;last-event-mins                                 ;OK 2.062967785287917E-4
            ;avg-last-home-games                             ;OK 2.6898705485223218E-11
            ;avg-last-away-games                             ;OK 7.215051357323254E-5
            ;season-salary                                   ;OK 4.266601842450868E-5
            ;C
            ;G
            ;F
            ;(nth (reverse all-scores) 1)                    ;~ 0.08884594435811466
            ;(nth (reverse all-scores) 2)                    ;~ 0.08884594435811466
            ;(nth (reverse all-scores) 3)                    ;~ 0.08884594435811466
            ;last-event-points                               ;BAD 0.13423766210192656
            ;(utils/bool->int current-home)                  ;BAD 0.16421015361380098)
            ;avg-last-games                                  ;BAD 0.9926613836842875
            ;is-top                                          ;BAD 0.3040758717420722
            ;last-event-pts                                  ;BAD 0.7107637646273752
            ;experience
            ;originals - end

            last-event-mins                                 ;3.9
            avg-last-home-games                             ;8.6
            avg-last-away-games                             ;8.9
            season-salary                                   ;6.2
            C                                               ;4.0
            G                                               ;4.0
            F                                               ;4.0
            (nth (reverse all-scores) 0)                    ;5.8
            (nth (reverse all-scores) 1)                    ;5.8
            ;(nth (reverse all-scores) 2)                    ;6.9
            ;(nth (reverse all-scores) 3)                    ;6.3
            ;(nth (reverse all-scores) 4)                    ;5.8
            ;(nth (reverse all-scores) 5)                    ;6.9
            ;(nth (reverse all-scores) 6)                    ;6.3
            last-event-points                               ;5.9
            (utils/bool->int current-home)                  ;3.9
            avg-last-games                                  ;14.8
            is-top                                          ;4.0
            last-event-pts                                  ;8.7
            experience                                      ;4.3

            team-name
            opp-name
            ;
            (:home-game (nth (reverse home-events) 0))
            (:opp-team (nth (reverse home-events) 0))
            (nth (reverse home-scores) 0)
            (:home-game (nth (reverse home-events) 1))
            (:opp-team (nth (reverse home-events) 1))
            (nth (reverse home-scores) 1)
            (:home-game (nth (reverse home-events) 2))
            (:opp-team (nth (reverse home-events) 2))
            (nth (reverse home-scores) 2)

            (:home-game (nth (reverse away-events) 0))
            (:opp-team (nth (reverse away-events) 0))
            (nth (reverse away-scores) 0)
            (:home-game (nth (reverse away-events) 1))
            (:opp-team (nth (reverse away-events) 1))
            (nth (reverse away-scores) 1)
            (:home-game (nth (reverse away-events) 2))
            (:opp-team (nth (reverse away-events) 2))
            (nth (reverse away-scores) 2)
            ;

            ;;+8 features - last
            (:points (last non-zero-events))
            (:rebounds (last non-zero-events))
            (:assists (last non-zero-events))
            (:steals (last non-zero-events))
            (:blocks (last non-zero-events))
            (:turnover (last non-zero-events))
            (:three-PM (last non-zero-events))

            ;;+12 features - last
            ;(:FGM-FGA (last non-zero-events))
            ;(:FieldGoalPercentage (last non-zero-events))
            ;(:three-PA (last non-zero-events))
            ;(:three-p-percentage (last non-zero-events))
            ;(:FTM (last non-zero-events))
            ;(:FTA (last non-zero-events))
            ;(:FTP (last non-zero-events))
            ;(:fouls (last non-zero-events))
            ;(:day-cntr (last (butlast non-zero-events)))
            ;(:opp-team (last (butlast non-zero-events)))
            ;
            ;;+8 features - bylast
            ;(:points (last (butlast non-zero-events)))
            ;(:rebounds (last (butlast non-zero-events)))
            ;(:assists (last (butlast non-zero-events)))
            ;(:steals (last (butlast non-zero-events)))
            ;(:blocks (last (butlast non-zero-events)))
            ;(:turnover (last (butlast non-zero-events)))
            ;(:three-PM (last (butlast non-zero-events)))
            ;;+12 features - bylast
            ;(:FGM-FGA (last (butlast non-zero-events)))
            ;(:FieldGoalPercentage (last (butlast non-zero-events)))
            ;(:three-PA (last (butlast non-zero-events)))
            ;(:three-p-percentage (last (butlast non-zero-events)))
            ;(:FTM (last (butlast non-zero-events)))
            ;(:FTA (last (butlast non-zero-events)))
            ;(:FTP (last (butlast non-zero-events)))
            ;(:fouls (last (butlast non-zero-events)))
            ;(:day-cntr (last (butlast non-zero-events)))
            ;(:opp-team (last (butlast non-zero-events)))
            ;opp-name

            (utils/nil->zero pts-current)]
           [;rotogrinder
            avg-last-games
            last-home-event-mins
            avg-last-away-games
            event-cnt
            avg-salary
            last-salary
            (utils/bool->int current-home)

            (utils/nil->zero pts-current)]))
       data))


(defn write-for-svm
  [data-set outfile]
  (with-open [w (clojure.java.io/writer outfile)]
    (doall
      (for [v data-set]
        (do
          (.write w (str (last v) " "))
          (loop [data (butlast v)
                 index 1]
            (if (empty? data)
              (.write w "\n")
              (do
                (.write w (str index ":" (first data) " "))
                (recur (rest data) (inc index)))))))))
  (println (str "Data writen in " outfile ", count: " (count data-set))))


(defn create-model
  [db contest-provider player-names]
  (let [points (create-array-for-regression
                 (model/prepare-data db contest-provider player-names
                                     :iteration-max c/*max-iterations*
                                     :database c/*active-database*))]
    (with-open [out-file (io/writer *rtree-train-file*)]
      (csv/write-csv out-file points))

    (println "Creating Random Forest . . .")
    (println )
    (let [{:keys [exit out err]} (shell/sh *rtree-bin*
                                           "-data" *rtree-train-file*
                                           "-save" *rtree-model-file*)]
      (if (= 0 exit)
        (println (str "Train successful: " out))
        (throw (Exception. (str "RTREE train failed: " err)))))))

(defn predict-players
  [db players-data contest-provider]
  (let [fpts-func (model/get-point-function contest-provider)
        feature-vector (create-array-for-regression
                         (map (fn [{:keys [Name] :as pinfo}]
                                (let [db-player (mc/find-one-as-map db c/*collection* {:Name Name})]
                                  (model/predict-data-from-events pinfo
                                                                  db-player
                                                                  fpts-func
                                                                  :database c/*active-database*)))
                              players-data))]

    (println (str "features: " (count feature-vector)))
    (with-open [out-file (io/writer *rtree-predict-file*)]
      (csv/write-csv out-file feature-vector))

    (println "Calling Random Forest for help . . .")
    (println (str *rtree-bin* " "
                                           "-load" *rtree-model-file* " "
                                           "-data" *rtree-predict-file* " "
                                           "-output" *rtree-output-file*))
    (let [{:keys [exit out err]} (shell/sh *rtree-bin*
                                           "-load" *rtree-model-file*
                                           "-data" *rtree-predict-file*
                                           "-output" *rtree-output-file*)]
      (if (= 0 exit)
        (println (str "Test was successful: " out))
        (throw (Exception. (str "RTREE TEST failed: " err)))))
    (map (fn [player score]
           (assoc player :rtree-projection score))
         players-data
         (map read-string (string/split-lines (slurp *rtree-output-file*))))))
