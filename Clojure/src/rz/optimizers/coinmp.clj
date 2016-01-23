(ns rz.optimizers.coinmp
  (:require [clojure.pprint :as pp]
            [clojure.string :as string]
            [clojure.java.shell :as shell]
            [rz.data :as data]
            [rz.optimizers.constants :as constants]
            [rz.optimizers.utils :as utils]
            )
  (:gen-class))



(def ^:dynamic *mps-file*  "../fantasy.mps")
(def ^:dynamic *coinmp-clp-binary* "../Solver/CoinMP-1.8.3/Clp/src/clp")
(def ^:dynamic *solution-file* "solution.simplex")

(def ^:dynamic *lpsolve-binary*  "../Solver/lp_solve/lp_solve")



(defn- write-meta-data
  [w]
  (.write w (str "NAME          FANTASY                                        \n")))

(defn- write-rows
  [w]
  (.write w (str "ROWS                                                         \n"))
  (.write w (str " N  PTS                                                      \n"))
  (.write w (str " E  TT9                                                      \n"))
  (.write w (str " E  PG2                                                      \n"))
  (.write w (str " E  PF2                                                      \n"))
  (.write w (str " E  SG2                                                      \n"))
  (.write w (str " E  SF2                                                      \n"))
  (.write w (str " L  SAL                                                      \n"))
  )

(defn- write-columns
  [w N players-data]
  (.write w (str "COLUMNS                                                      \n"))

  (.write w (str "    INT1      'MARKER'                 'INTORG'              \n"))
  (doall
    (for [i (range N)]
      (do
        (.write w (str "    "
                       (str "X" (format "%04d" i))
                       "     PTS            "
                       (format "%6.2f" (- (double (:roto-wire-projection (nth players-data i)))))
                       "                         \n"))
        (.write w (str "    "
                       (str "X" (format "%04d" i))
                       "     TT9             "
                       (format "%5d" 1)
                       "                         \n"))
        (doall
          (for [role ["PG" "PF" "SG" "SF"]]
            (.write w (str "    "
                           (str "X" (format "%04d" i))
                           "     " role "2             "
                           (format "%5d" (if (= role (:Position (nth players-data i))) 1 0))
                           "                         \n"))))
        (.write w (str "    "
                       (str "X" (format "%04d" i))
                       "     SAL            "
                       (format "%5d" (:Salary (nth players-data i)))
                       "                         \n"))

        )))
  (.write w (str "    INT1END   'MARKER'                 'INTEND'              \n"))

  )



(defn- write-rhs
  [w N max-salary]
  (.write w (str "RHS                                                          \n"))
  (.write w (str "    RHS1      TT9               9.0                         \n"))
  (.write w (str "    RHS1      PG2               2.0                         \n"))
  (.write w (str "    RHS1      PF2               2.0                         \n"))
  (.write w (str "    RHS1      SG2               2.0                         \n"))
  (.write w (str "    RHS1      SF2               2.0                         \n"))
  (.write w (str "    RHS1      SAL             " max-salary "                         \n"))
  )

(defn- write-bounds
  [w N]
  (.write w (str "BOUNDS                                                       \n"))
  (doall
    (for [i (range N)]
      (do
        (.write w (str " BV  BOUND    "(str "X" (format "%04d" i))"     1                     \n"))))))



(defn create-mps-file
  [players-data max-salary]
  (let [N (count players-data)]
    (with-open [w (clojure.java.io/writer *mps-file*)]
      (write-meta-data w)
      (write-rows w)
      (write-columns w N players-data)
      (write-rhs w N max-salary)
      (write-bounds w N)
      (.write w (str "ENDATA                                                       \n"))
    )))

;(defn coinmp-solve
;  []
;  (create-mps-file)
;  (shell/sh *coinmp-clp-binary* *mps-file* "-primals" "-solution" *solution-file*)
;  (let [out (slurp *solution-file*)]
;    (pp/pprint out)
;    (let [variables (filter (fn [l] (re-find #"  [0-9]+ " l)) (string/split-lines out))
;          indexes (map #(read-string (second (re-matches #"[ ]*([0-9]+).*" %))) variables)
;          team (map #(nth players-data %) indexes)]
;      (utils/print-team2 team))))


(defn lpsolve-solve
  [max-salary players-data]
    (create-mps-file players-data max-salary)
    (let [{:keys [out]} (shell/sh *lpsolve-binary* "-mps" *mps-file*)]
      (let [variables (filter (fn [l] (re-find #"X[0-9][0-9][0-9][0-9][ ]+1" l)) (string/split-lines out))
            indexes (map #(Integer/parseInt (second (re-matches #"X([0-9]+)[ ]*1" %))) variables)
            team (map #(nth players-data %) indexes)]
        (utils/print-team2 team)
        )
      ))


(defn lpsolve-solve-draftkings
  []
  (lpsolve-solve constants/*team-salary-draftkings*
                 (data/add-rotowires-projection (data/init-players-data-draftking) constants/*draftking*)))

(defn lpsolve-solve-fanduel
  []
  (lpsolve-solve constants/*team-salary-fanduel*
                 (data/add-rotowires-projection
                    (filter #(not (= "O" (:injury %))) (data/init-players-data-fanduel))
                    constants/*fanduel*)))
