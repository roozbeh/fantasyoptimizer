(ns rz.model.svm
  (:require [clojure.java.shell :as shell]
            [rz.optimizers.utils :as utils]
            [rz.model.model :as model]
            [clojure.string :as string]
            [monger.collection :as mc]
            [rz.optimizers.constants :as c]
            [clojure.pprint :as pp]))

(def ^:dynamic *svm-train-file-unscaled*  "../svm_train_file_unscaled")
(def ^:dynamic *svm-test-file-unscaled*  "../svm_test_file_unscaled")
(def ^:dynamic *svm-predict-file-unscaled*  "../svm_predict_file_unscaled")
(def ^:dynamic *svm-train-file*  "../svm_train_file")
(def ^:dynamic *svm-test-file*  "../svm_test_file")
(def ^:dynamic *svm-predict-file*  "../svm_predict_file")
(def ^:dynamic *svm-model-file*  "../svm_model_file")
(def ^:dynamic *svm-test-output-file*  "../svm_output_file")
(def ^:dynamic *svm-scaling-parameters*  "../svm_scaling_parameters")


(def ^:dynamic *svm-train-bin*  "../Regression/libsvm/svm-train")
(def ^:dynamic *svm-test-bin*  "../Regression/libsvm/svm-predict")
(def ^:dynamic *svm-scale-bin*  "../Regression/libsvm/svm-scale")



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


(defn- create-svm-model
  [train-set test-set]
  (write-for-svm train-set *svm-train-file-unscaled*)
  (write-for-svm test-set *svm-test-file-unscaled*)
  (let [{:keys [out]} (shell/sh *svm-scale-bin*
                                "-s" *svm-scaling-parameters*
                                *svm-train-file-unscaled*)]
    (spit *svm-train-file* out))
  (let [{:keys [out]} (shell/sh *svm-scale-bin*
                                "-r" *svm-scaling-parameters*
                                *svm-test-file-unscaled*)]
    (spit *svm-test-file* out))
  (let [{:keys [exit out err]} (shell/sh *svm-train-bin*
                                         "-s" "3" "-p" "0.0001" "-t" "0" "-h" "0"
                                         "-c" "0.03125" "-g" "0.0078125"
                                         *svm-train-file*
                                         *svm-model-file*)]
    (if (= 0 exit)
      (println (str "Train successful: " out))
      (throw (Exception. (str "SVN train failed: " err)))))
  (let [{:keys [exit out err]} (shell/sh *svm-test-bin*
                                         *svm-test-file*
                                         *svm-model-file*
                                         *svm-test-output-file*)]
    (if (= 0 exit)
      (println (str "Test was successful: " out))
      (throw (Exception. (str "SVN TEST failed: " err))))))

(defn estimate-svm-model-accuracy
  [db contest-provider player-names]
  (let [ftps-keyword (model/get-point-function contest-provider)
        train-set (create-array-for-regression
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
    (create-svm-model train-set test-set)))


(defn create-model
  [db contest-provider player-names]
  (let [ftps-keyword (model/get-point-function contest-provider)
        points (create-array-for-regression
                 (model/prepare-data db contest-provider player-names
                                     :iteration-max c/*max-iterations*
                                     :database c/*active-database*)
                 ftps-keyword)
        ;;[test-set train-set ] (split-at (* (double (count points)) 0.2) points)
        ;train-set (repeatedly (* (double (count points)) 0.8) #(rand-nth points))
        ;test-set (repeatedly (* (double (count points)) 0.2) #(rand-nth points))
        test-set points
        train-set points
        ]
    (create-svm-model train-set test-set)))


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
    ;(pp/pprint (first feature-vector))
    (write-for-svm feature-vector *svm-predict-file-unscaled*)
    (let [{:keys [out]} (shell/sh *svm-scale-bin*
                                  "-r" *svm-scaling-parameters*
                                  *svm-predict-file-unscaled*)]
      (spit *svm-predict-file* out))
    (let [{:keys [exit out err]} (shell/sh *svm-test-bin*
                                           *svm-predict-file*
                                           *svm-model-file*
                                           *svm-test-output-file*)]
      (if (= 0 exit)
        (println (str "Test was successful: " out))
        (throw (Exception. (str "SVN TEST failed: " err)))))
    (map (fn [player score]
           (assoc player :svm-projection score))
         players-data
         (map read-string (string/split-lines (slurp *svm-test-output-file*))))))

