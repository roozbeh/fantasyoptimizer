(ns rz.model.xgboost
  (:require [rz.optimizers.utils :as utils]
            [rz.optimizers.constants :as c]
            [rz.model.svm :as svm]
            [clojure.java.shell :as shell]
            [rz.model.model :as model]
            [monger.collection :as mc]
            [clojure.string :as string]))

(def ^:dynamic *xgboost-model-file*  "0020.model")
(def ^:dynamic *xgboost-config-file*  "../Regression/xgboost/mushroom.conf")
(def ^:dynamic *xgboost-bin*  "../Regression/xgboost/xgboost")
(def ^:dynamic *xgboost-out*  "pred.txt")


(defn create-array-for-regression
  [data ftps-keyword]
  (map (fn [{:keys [pts-current last-event-mins last-event-pts last-home-event-mins
                    last-home-event-pts last-away-event-mins last-away-event-pts
                    avg-last-games avg-last-home-games avg-last-away-games avg-last-away-games
                    current-home event-cnt  home-events away-events
                    team-name opp-name last-salary cur-salary avg-salary
                    all-events all-scores season-salary experience C is-top Name
                    last-event-points C G F home-scores away-scores non-zero-events
                    ] :as d}]
         (if (= :espn c/*active-database*)
           [;espn
            ;last-event-mins                                 ;OK 2.062967785287917E-4
            ;season-salary                                   ;OK 4.266601842450868E-5
            ;avg-last-home-games                             ;OK 2.6898705485223218E-11
            ;avg-last-away-games                             ;OK 7.215051357323254E-5
            ;last-event-pts
            ;(nth (reverse all-scores) 1)                    ;~ 0.08884594435811466
            ;is-top                                          ;BAD 0.3040758717420722
            ;avg-last-games                                  ;BAD 0.9926613836842875
            ;last-event-points                               ;BAD 0.13423766210192656

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
            ;team-name
            ;opp-name
            ;(:opp-team (nth (reverse home-events) 0))
            (nth (reverse home-scores) 0)
            (:mins (nth (reverse home-events) 0))
            ;(:opp-team (nth (reverse home-events) 1))
            (nth (reverse home-scores) 1)
            (:mins (nth (reverse home-events) 1))
            ;(:opp-team (nth (reverse home-events) 2))
            (nth (reverse home-scores) 2)
            (:mins (nth (reverse home-events) 1))
            ;(:opp-team (nth (reverse away-events) 0))
            (nth (reverse away-scores) 0)
            (:mins (nth (reverse away-events) 1))
            ;(:opp-team (nth (reverse away-events) 1))
            (nth (reverse away-scores) 1)
            (:mins (nth (reverse away-events) 1))
            ;(:opp-team (nth (reverse away-events) 2))
            (nth (reverse away-scores) 2)
            (:mins (nth (reverse away-events) 2))
            ;(:opp-team (nth (reverse away-events) 2))

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
            avg-last-games
            last-home-event-mins
            avg-last-away-games
            event-cnt
            (utils/bool->int current-home)
            last-home-event-pts
            avg-last-home-games
            last-event-mins
            (count home-events)
            last-salary
            cur-salary
            avg-salary
            last-event-pts
            last-away-event-pts
            last-away-event-mins

            (utils/nil->zero pts-current)]))
       data))



(defn- create-xgboost-model
  [train-set test-set]
  (svm/write-for-svm train-set svm/*svm-train-file-unscaled*)
  (svm/write-for-svm test-set svm/*svm-test-file-unscaled*)
  (let [{:keys [out]} (shell/sh svm/*svm-scale-bin*
                                "-s" svm/*svm-scaling-parameters*
                                svm/*svm-train-file-unscaled*)]
    (spit svm/*svm-train-file* out))
  (let [{:keys [out]} (shell/sh svm/*svm-scale-bin*
                                "-r" svm/*svm-scaling-parameters*
                                svm/*svm-test-file-unscaled*)]
    (spit svm/*svm-test-file* out))
  (let [{:keys [exit out err]} (shell/sh *xgboost-bin*
                                         *xgboost-config-file*)]
    (if (= 0 exit)
      (println (str "Train successful"))
      (throw (Exception. (str "XGBOOST train failed: " err))))))

(defn estimate-xgboos-model-accuracy
  [db contest-provider player-names]
  (let [ftps-keyword (model/get-point-function contest-provider)
        train-set (svm/create-array-for-regression
                    (model/prepare-data db contest-provider player-names
                                        :use-last false
                                        :iteration-max c/*max-iterations*
                                        :database c/*active-database*)
                    ftps-keyword)
        test-set (create-array-for-regression
                   (model/prepare-data db contest-provider player-names
                                       :use-last true :iteration-max 1
                                       :database c/*active-database*)
                   ftps-keyword)]
    (create-xgboost-model train-set test-set)))

(defn create-model
  [db contest-provider player-names]
  (let [ftps-keyword (model/get-point-function contest-provider)
        points (create-array-for-regression
                 (model/prepare-data db contest-provider player-names
                                     :iteration-max c/*max-iterations*
                                     :database c/*active-database*)
                 ftps-keyword)
        test-set points
        train-set points]
    (create-xgboost-model train-set test-set)))


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
                              players-data)
                         fpts-func)]
    (svm/write-for-svm feature-vector svm/*svm-test-file-unscaled*)
    (let [{:keys [out]} (shell/sh svm/*svm-scale-bin*
                                  "-r" svm/*svm-scaling-parameters*
                                  svm/*svm-test-file-unscaled*)]
      (spit svm/*svm-test-file* out))
    (let [{:keys [exit out err]} (shell/sh *xgboost-bin*
                                           *xgboost-config-file*
                                           "task=pred"
                                           (str "model_in=" *xgboost-model-file*))]
      (if (= 0 exit)
        (println (str "Test was successful: " out))
        (throw (Exception. (str "SVN TEST failed: " err)))))
    (map (fn [player score]
           (println (str "projection for " (:Name player) " is " score))
           (assoc player :xg-projection score))
         players-data
         (map read-string (string/split-lines (slurp *xgboost-out*))))))
