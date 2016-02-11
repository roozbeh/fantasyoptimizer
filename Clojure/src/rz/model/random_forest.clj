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
                    is-top is_win
                    ] :as d}]
         ;(println (str Name " -> " C " - " G " - " F))
         (if (= :espn c/*active-database*)
           [;espn
            last-event-mins                                 ;OK 2.062967785287917E-4
            avg-last-home-games                             ;OK 2.6898705485223218E-11
            avg-last-away-games                             ;OK 7.215051357323254E-5
            season-salary                                   ;OK 4.266601842450868E-5
            C
            G
            F
            (nth (reverse all-scores) 1)                    ;~ 0.08884594435811466
            (nth (reverse all-scores) 2)                    ;~ 0.08884594435811466
            (nth (reverse all-scores) 3)                    ;~ 0.08884594435811466

            last-event-points                               ;BAD 0.13423766210192656
            (utils/bool->int current-home)                  ;BAD 0.16421015361380098)
            avg-last-games                                  ;BAD 0.9926613836842875
            is-top                                          ;BAD 0.3040758717420722
            last-event-pts                                  ;BAD 0.7107637646273752
            experience                                      ;BAD 0.3321079046177915


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

    (with-open [out-file (io/writer *rtree-predict-file*)]
      (csv/write-csv out-file feature-vector))

    (println "Calling Random Forest for help . . .")
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
