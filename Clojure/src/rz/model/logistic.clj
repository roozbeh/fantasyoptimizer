(ns rz.model.logistic
  (:require [clojure.pprint :as pp]
            [rz.model.model :as model]
            [rz.optimizers.utils :as utils]
            [clojure.java.io :as io]
            [incanter.core :as incanter]
            ))

; test-data looks like: ({:class 0, :x1 14.053064, :x0 -0.017612} {:class 1, :x1 4.662541, :x0 -1.395634} {:class 0, :x1 6.53862, :x0 -0.752157})
;(def test-data
;  (map #(apply assoc {} (interleave [:x0 :x1 :class] (map clojure.core/read-string (clojure.string/split % #"\t"))))
;       (line-seq (io/reader "machinelearninginaction/Ch05/testSet.txt"))))
;(def data-matrix (incanter/matrix
;                   (partition 3
;                              (interleave (repeat 1) (map :x0 test-data) (map :x1 test-data)))))
;
;(def labels (map :class test-data))

(defn sigmoid [z]
  (/ 1 (+ 1 (Math/exp (- z)))))

(defn weights
  [init c mat labels]
  (let [start-values init
        alpha 0.001
        error (incanter/minus labels
                     (map sigmoid (incanter/mmult mat start-values)))
        stop-values (incanter/plus start-values
                          (incanter/mult alpha
                                         (incanter/mmult (incanter/trans mat)
                                                         error)))]
    (if (> (+ c 1) 500)
      start-values
      (weights stop-values (+ c 1) mat labels))))

;(println (weights (matrix 1 3 1) 1 data-matrix))

(defn create-array-for-regression
  [data]
  (map (fn [{:keys [pts-current last-home-event-mins
                    last-home-event-pts avg-last-away-games
                    event-cnt last-salary cur-salary avg-salary
                    last-event-mins last-event-pts
                    avg-last-games] :as d}]
         [
          ;last-event-pts

          avg-last-games
          last-home-event-mins
          ;avg-last-away-games
          event-cnt
          ;cur-salary
          ;avg-salary
          ;last-salary

          (utils/nil->zero pts-current)])
       data))

(defn create-model
  ([db contest-provider player-names]
   (let [points (create-array-for-regression (model/prepare-data db contest-provider player-names))
         ;points (take 10 points)
         labels (map last points)
         data (map #(take (dec (count (first points))) %) points)
         ]
     (weights (incanter/matrix 1 3 1) 1 data labels)
     )
    )
  ([db contest-provider]
   (create-model db contest-provider nil)))
