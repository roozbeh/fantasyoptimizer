(ns rz.model.svm
  (:require [clojure.java.shell :as shell]
            [rz.optimizers.utils :as utils]
            [rz.model.linear :as linear]))

(def ^:dynamic *svm-train-file-unscaled*  "../svm_train_file_unscaled")
(def ^:dynamic *svm-test-file-unscaled*  "../svm_test_file_unscaled")
(def ^:dynamic *svm-train-file*  "../svm_train_file")
(def ^:dynamic *svm-test-file*  "../svm_test_file")
(def ^:dynamic *svm-model-file*  "../svm_model_file")
(def ^:dynamic *svm-test-output-file*  "../svm_output_file")


(def ^:dynamic *svm-train-bin*  "../Regression/libsvm/svm-train")
(def ^:dynamic *svm-test-bin*  "../Regression/libsvm/svm-predict")
(def ^:dynamic *svm-scale-bin*  "../Regression/libsvm/svm-scale")

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

          last-event-pts
          avg-last-home-games
          avg-last-games
          last-away-event-pts
          last-away-event-mins
          last-event-mins
          (utils/bool->int current-home)



          (utils/nil->zero pts-current)])
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

(defn create-svm-model
  [db]
  (let [points (create-array-for-regression (linear/filter-23 (linear/prepare-data-for-regression-recursive db)))
        [test-set train-set ] (split-at (* (double (count points)) 0.5) points)]
    (write-for-svm train-set *svm-train-file-unscaled*)
    (write-for-svm test-set *svm-test-file-unscaled*)
    ;(let [{:keys [out]} (shell/sh *svm-scale-bin* *svm-train-file-unscaled*)]
    ;  (spit *svm-train-file* out))
    ;(let [{:keys [out]} (shell/sh *svm-scale-bin* *svm-test-file-unscaled*)]
    ;  (spit *svm-test-file* out))
    (shell/sh *svm-train-bin* "-s" "3" "-p" "0.001" "-t" "0" *svm-train-file* *svm-model-file*)
    (shell/sh *svm-test-bin* *svm-test-file* *svm-model-file* *svm-test-output-file*)))
