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


(def wins
  {
   "ATL" 27
   "BKN" 12
   "BOS" 26
   "CHA" 22
   "CHI" 25
   "CLE" 32
   "DAL" 26
   "DEN" 17
   "DET" 25
   "GSW" 42
   "HOU" 25
   "IND" 23
   "LAC" 30
   "LAL" 9
   "MEM" 26
   "MIA" 25
   "MIL" 20
   "MIN" 14
   "NOP" 16
   "NYK" 22
   "OKC" 35
   "ORL" 20
   "PHI" 7
   "PHO" 14
   "POR" 21
   "SAC" 20
   "SAS" 39
   "TBL" 30
   "UTA" 20
   "WAS" 20
   })

(def loss {
           "ATL" 20
           "BKN" 34
           "BOS" 21
           "CHA" 24
           "CHI" 19
           "CLE" 12
           "DAL" 22
           "DEN" 29
           "DET" 21
           "GSW" 4
           "HOU" 23
           "IND" 22
           "LAC" 16
           "LAL" 38
           "MEM" 20
           "MIA" 21
           "MIL" 27
           "MIN" 33
           "NOP" 28
           "NYK" 25
           "OKC" 13
           "ORL" 24
           "PHI" 40
           "PHO" 33
           "POR" 26
           "SAC" 25
           "SAS" 7
           "TBL" 15
           "UTA" 25
           "WAS" 23
           })

(defn create-array-for-regression
  [data ftps-keyword]
  (map (fn [{:keys [pts-current last-event-mins last-event-pts last-home-event-mins
                    last-home-event-pts last-away-event-mins last-away-event-pts
                    avg-last-games avg-last-home-games avg-last-away-games avg-last-away-games
                    current-home event-cnt  home-events away-events
                    team-name opp-name last-salary cur-salary avg-salary
                    all-events all-scores season-salary experience C is-top Name
                    last-event-points
                    ] :as d}]
         (if (= :espn c/*active-database*)
            [;espn
             last-event-mins                                 ;OK 2.062967785287917E-4
             season-salary                                   ;OK 4.266601842450868E-5
             avg-last-home-games                             ;OK 2.6898705485223218E-11
             avg-last-away-games                             ;OK 7.215051357323254E-5
             last-event-pts
             (nth (reverse all-scores) 1)                    ;~ 0.08884594435811466
             is-top                                          ;BAD 0.3040758717420722
             avg-last-games                                  ;BAD 0.9926613836842875
             last-event-points                               ;BAD 0.13423766210192656
             ;(utils/bool->int current-home)
             ;last-home-event-mins
             ;experience                                      ;BAD 0.3321079046177915
             ;last-home-event-pts
             ;last-home-event-mins
             ;last-away-event-pts
             ;last-away-event-mins
             ;
             ;C                                               ;OK 0.007628189945130481


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

