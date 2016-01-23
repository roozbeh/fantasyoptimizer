(ns rz.optimizers.cpplex
  (:require [clojure.pprint :as pp]
            [clojure.string :as string]
            [clojure.java.shell :as shell]
            [rz.data :as data]
            [rz.optimizers.constants :as constaints]
            [rz.optimizers.utils :as utils]
            )
  (:gen-class))



(defonce players-data
         (data/add-rotowires-projection
           (filter #(not (= "O" (:injury %))) (data/init-players-data))))

(def ^:dynamic *problem-file*  "/Users/roozbeh/Documents/FantasyOptimizer/fantasy.problem")
(def ^:dynamic *cpplex-solver-binary* "../Solver/cpplex/build/main")


;[METADATA]
;
;name Nome del problema
;vars 10
;
;[VARIABLES]
;
;0       variable_1      0.2
;0       variable_2      0.1
;0       variable_3      1
;-2      variable_4      1
;-4      variable_5      1
;inf     variable_6      inf
;inf     variable_7      inf
;-4      variable_8      1
;4    	variable_9      3
;8     	variable_10     inf
;
;[CONSTRAINTS]
;
;1 2 0 0 1 0 2 3 2 1  > 2
;-2 0 0 0 1 1 1 2 5 1 < 10
;
;[OBJECTIVE]
;
;minimize 0 0 0 0 0 0 0 0 1 1




(defn- write-variables
  [w N]
  (.write w (str "[VARIABLES]\n"))
  (.write w (str "\n"))
  (doall
    (for [i (range N)]
      (.write w (str "0    x_" i  "    1\n"))))
  (.write w (str "\n")))


(defn- write_9_total_constraint
  [w N]
  (doall
    (for [i (range N)]
      (.write w "1 ")))
  (.write w " = 9\n"))


(defn- write_9_total_constraint
  [w N]
  (doall
    (for [i (range N)]
      (.write w "1 ")))
  (.write w " = 9\n"))

(defn- write_role_constraint
  [w N role count]
  (doall
    (for [i (range N)]
      (.write w (if (= role (:Position (nth players-data i)))
                  "1 "
                  "0 "))))
  (.write w (str " = " count "\n")))

(defn- write_salary_constraint
  [w N]
  (doall
    (for [i (range N)]
      (.write w (str (:Salary (nth players-data i)) " "))))
  (.write w (str " < " (+ 60000 constaints/*team-salary*) "\n")))

(defn- write_injury_constraint
  [w N]
  (doall
    (for [i (range N)]
      (.write w (if (= "O" (:injury (nth players-data i)))
                  "1 "
                  "0 "))))
  (.write w (str " = 0"  "\n")))

(defn- write-constraints
  [w N]
  (.write w (str "[CONSTRAINTS]\n"))
  (.write w (str "\n"))
  (write_9_total_constraint w N)
  (write_role_constraint w N "PG" 2)
  (write_role_constraint w N "PF" 2)
  (write_role_constraint w N "SG" 2)
  (write_role_constraint w N "SF" 2)
  ; IMPLY ON (write_role_constraint w N "C" 1)
  (write_salary_constraint w N)
  ;(write_injury_constraint w N)
  (.write w (str "\n")))

(defn- write-meta-data
  [w N]
  (.write w (str "[METADATA]\n"))
  (.write w (str "\n"))
  (.write w (str "name Fantasy Optimizer Problem\n"))
  (.write w (str "vars " N "\n"))
  (.write w (str "\n")))

(defn- write-objective
  [w N]
  (.write w (str "[OBJECTIVE]\n"))
  (.write w (str "\n"))
  (.write w (str "maximize "))
  (doall
    (for [i (range N)]
      (.write w (str (:roto-wire-projection (nth players-data i)) " "))))
  (.write w (str "\n")))

(defn create-probleam-file
  []
  (let [N (count players-data)]
    (with-open [w (clojure.java.io/writer  *problem-file*)]
      (write-meta-data w N)
      (write-variables w N)
      (write-constraints w N)
      (write-objective w N))))

(defn cpplex-solve
  []
  (create-probleam-file)
  (let [{:keys [out err]} (shell/sh *cpplex-solver-binary* *problem-file*)]
    (pp/pprint out)
    (if (= "" err)
      (let [variables (filter (fn [l] (re-find #"x_[0-9]+:\t\t\t1" l)) (string/split-lines out))
            indexes (map #(read-string (second (re-matches #"x_([0-9]+):\t\t\t1" %))) variables)
            team (map #(nth players-data %) indexes)]
        (utils/print-team2 team))
      (println (str "ERROR HAPPENED: \n" err))))


  )


