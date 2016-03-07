(ns rz.model.random-forest
  (:require [rz.model.model :as model]
            [rz.optimizers.constants :as c]
            [clojure.java.shell :as shell]
            [clojure.java.io :as io]
            [clojure.data.csv :as csv]
            [rz.optimizers.utils :as utils]
            [monger.collection :as mc]
            [clojure.string :as string]
            [clojure.pprint :as pp]))

(def ^:dynamic *rtree-train-file*  "../rtree_train.csv")
(def ^:dynamic *rtree-test-file*  "../rtree_test.csv")
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
                    pos
                    ] :as d}]
         (if (= :espn c/*active-database*)
           [;espn
            last-event-mins                                 ;0
            avg-last-home-games                             ;1
            avg-last-away-games                             ;2
            season-salary                                   ;3
            C                                               ;4
            G                                               ;5
            F                                               ;6
            (nth (reverse all-scores) 0)                    ;7
            (nth (reverse all-scores) 1)                    ;8
            last-event-points                               ;9
            (utils/bool->int current-home)                  ;10
            avg-last-games                                  ;14.8
            is-top                                          ;4.0
            last-event-pts                                  ;8.7
            experience                                      ;4.3
            team-name
            opp-name
            (:opp-team (nth (reverse home-events) 0))
            (nth (reverse home-scores) 0)
            (:opp-team (nth (reverse home-events) 1))
            (nth (reverse home-scores) 1)
            (:opp-team (nth (reverse home-events) 2))
            (nth (reverse home-scores) 2)
            (:opp-team (nth (reverse away-events) 0))
            (nth (reverse away-scores) 0)
            (:opp-team (nth (reverse away-events) 1))
            (nth (reverse away-scores) 1)
            (:opp-team (nth (reverse away-events) 2))
            (nth (reverse away-scores) 2)
            (:points (last non-zero-events))
            (:rebounds (last non-zero-events))
            (:assists (last non-zero-events))
            (:steals (last non-zero-events))
            (:blocks (last non-zero-events))
            (:turnover (last non-zero-events))
            (:three-PM (last non-zero-events))

            (utils/nil->zero pts-current)]
           [;rotogrinder
            last-home-event-mins                            ;0
            avg-last-away-games                             ;1
            event-cnt                                       ;2
            avg-salary                                      ;3
            last-salary                                     ;4
            (utils/bool->int current-home)                  ;5
            team-name                                       ;6
            opp-name                                        ;7
            pos                                             ;8
            cur-salary                                      ;9
            (utils/nil->zero (:G (last all-events)))        ;10
            (utils/nil->zero (:SOG (last all-events)))      ;11


            avg-last-games                                  ;0

            (nth (reverse all-scores) 0)                    ;11
            (nth (reverse all-scores) 1)                    ;12
            (count home-events)
            (count away-events)

            (if (> (count home-events) 0)
              (:opp-team (nth (reverse home-events) 0))      ;13
              0)
            (if (> (count home-events) 0)
              (nth (reverse home-scores) 0)      ;13
              0)

            (if (> (count away-events) 0)
              (:opp-team (nth (reverse away-events) 0))      ;13
              0)
            (if (> (count away-scores) 0)
              (nth (reverse away-scores) 0)      ;13
              0)

            (utils/nil->zero (:mins (last all-events)))
            (utils/nil->zero (:GP (last all-events)))
            (utils/nil->zero (:PTS (last all-events)))
            (utils/nil->zero (:SHG (last all-events)))
            (utils/nil->zero (:A (last all-events)))
            (utils/nil->zero (:PIM (last all-events)))
            (utils/nil->zero (:PPA (last all-events)))
            (utils/nil->zero (:PPG (last all-events)))


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


(defn precision-check
  [db player-names contest-provider]
  (let [train-set (create-array-for-regression
                    (model/prepare-data db contest-provider player-names
                                        :use-last false
                                        :iteration-max c/*max-iterations*
                                        :database c/*active-database*))
        test-set (create-array-for-regression
                   (model/prepare-data db contest-provider player-names
                                       :use-last true :iteration-max 1
                                       :database c/*active-database*))]
    (with-open [out-file (io/writer *rtree-train-file*)]
      (csv/write-csv out-file train-set))
    (with-open [out-file (io/writer *rtree-test-file*)]
      (csv/write-csv out-file test-set))

    (println "Testing Random Forest . . .")
    (println (str *rtree-bin* " "
              "-data" " " *rtree-train-file* " "
              "-test" " " *rtree-test-file* " "
              "-save" " " *rtree-model-file* " "
              "-precision"))
    (let [{:keys [exit out err]} (shell/sh *rtree-bin*
                                           "-data" *rtree-train-file*
                                           "-test" *rtree-test-file*
                                           "-save" *rtree-model-file*
                                           "-precision")]
      (if (= 0 exit)
        (println (str "Precision Check successful: " out))
        (throw (Exception. (str "RTREE train failed: " out)))))))
