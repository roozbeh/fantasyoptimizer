(ns rz.optimizers.nhl
  (:require [clojure.pprint :as pp]
            [clojure.string :as string]
            [clojure.java.shell :as shell]
            [rz.data :as data]
            [rz.optimizers.constants :as constants]
            [rz.optimizers.utils :as utils]
            [rz.optimizers.constants :as c]
            [rz.optimizers.report :as report])
  (:gen-class))



(def ^:dynamic *mps-file*  "../fantasy.mps")
(def ^:dynamic *lpsolve-binary*  "../Solver/lp_solve/lp_solve")

(defn- write-meta-data
  [w]
  (.write w (str "NAME          FANTASY                                        \n")))

(defn- write-rows
  ([w context-provider]
   (.write w (str "ROWS                                                         \n"))
   (.write w (str " N  PTS                                                      \n"))
   (.write w (str " E  TT9                                                      \n"))
   (if (= context-provider c/*fanduel*)
     (do
       (println "fanduel NHL not implemented!"))
     (do
       (.write w (str " G  CC2                                                      \n"))
       (.write w (str " G  WW3                                                      \n"))
       (.write w (str " G  DD2                                                      \n"))
       (.write w (str " E  GG1                                                      \n"))))
   (.write w (str " L  SAL                                                      \n")))
  ([w solutions context-provider]
   (write-rows w context-provider)
   (doall
     (for [[row-name variables] solutions]
       (.write w (str " L  " row-name "                                                      \n"))))))

(defn- write-rhs
  [w max-salary solutions context-provider]
  (.write w (str "RHS                                                          \n"))
  (if (= context-provider c/*fanduel*)
    (do
      (println "fanduel is not implemented for NHL")
      )
    (do
      (.write w (str "    RHS1      TT9               9.0                         \n"))
      (.write w (str "    RHS1      CC2               2.0                         \n"))
      (.write w (str "    RHS1      WW3               3.0                         \n"))
      (.write w (str "    RHS1      DD2               2.0                         \n"))
      (.write w (str "    RHS1      GG1               1.0                         \n"))
      ))
  (.write w (str "    RHS1      SAL             " max-salary "                         \n"))
  (doall
    (for [[row-name solution-vars] solutions]
      (.write w (str "    RHS1      "row-name"               " (if (= context-provider c/*fanduel*)
                                                                 c/*max-common-players-fanduel*
                                                                 c/*max-common-players-draftkings*)  "                         \n")))))


(defn- write-columns
  [w N players-data solutions context-provider proj-kwd]
  (.write w (str "COLUMNS                                                      \n"))
  (.write w (str "    INT1      'MARKER'                 'INTORG'              \n"))
  (doall
    (for [i (range N)]
      (do
        (.write w (str "    "
                       (str "X" (format "%04d" i))
                       "     PTS            "
                       (format "%6.2f" (- (double (proj-kwd (nth players-data i)))))
                       "                         \n"))
        (.write w (str "    "
                       (str "X" (format "%04d" i))
                       "     TT9             "
                       (format "%5d" 1)
                       "                         \n"))
        (doall
          (for [[row-name roles] {"CC2" #{"C"} "WW3" #{"RW" "LW"} "DD2" #{"D"} "GG1" #{"G"}}]
            (.write w (str "    "
                           (str "X" (format "%04d" i))
                           "     " row-name "             "
                           (format "%5d" (if (contains? roles (:Position (nth players-data i))) 1 0))
                           "                         \n"))))
        (.write w (str "    "
                       (str "X" (format "%04d" i))
                       "     SAL            "
                       (format "%5d" (:Salary (nth players-data i)))
                       "                         \n"))
        (doall
          (for [[row-name solution-vars] solutions]
            (.write w (str "    "
                           (str "X" (format "%04d" i))
                           "     " row-name "             "
                           (format "%5d" (if (contains? solution-vars (str "X" (format "%04d" i))) 1 0))
                           "                         \n"))))

        )))
  (.write w (str "    INT1END   'MARKER'                 'INTEND'              \n")))




(defn- write-bounds
  [w N]
  (.write w (str "BOUNDS                                                       \n"))
  (doall
    (for [i (range N)]
      (do
        (.write w (str " BV  BOUND    "(str "X" (format "%04d" i))"     1                     \n"))))))



(defn create-mps-file
  [players-data max-salary solutions context-provider proj-keyword]
  (let [N (count players-data)]
    (with-open [w (clojure.java.io/writer *mps-file*)]
      (write-meta-data w)
      (write-rows w solutions context-provider)
      (write-columns w N players-data solutions context-provider proj-keyword)
      (write-rhs w max-salary solutions context-provider)
      (write-bounds w N)
      (.write w (str "ENDATA                                                       \n")))))

(defn lpsolve-solve-multiple
  [max-salary players-data solution-count context-provider proj-keyword]
  (loop [solutions {}
         count 0]
    (if (>= count solution-count)
      solutions
      (do
        (create-mps-file players-data max-salary solutions context-provider proj-keyword)
        (let [{:keys [out]} (shell/sh *lpsolve-binary* "-mps" *mps-file*)
              variables (filter (fn [l] (re-find #"X[0-9][0-9][0-9][0-9][ ]+1" l)) (string/split-lines out))
              solution (set (map (comp first #(clojure.string/split % #" ")) variables))
              solution-row (str "S" (format "%02d" count))]
          (recur (assoc solutions solution-row solution) (inc count)))))))


(defn print-solutions
  [solutions players-data]
  (doall
    (for [[row variables] solutions]
      (let [indexes (map #(Integer/parseInt (second (re-matches #"X([0-9]+)" %))) variables)
            team (map #(nth players-data %) indexes)]
        (report/print-team2 team)
        (map #(list (:Position %) (:NameID %)) team)))))

(defn lpsolve-solve-draftkings
  [player-with-proj proj-keyword]
  (let [players-proj (data/add-rotowires-projection player-with-proj c/*draftking*)]
    (print-solutions
      (lpsolve-solve-multiple constants/*team-salary-draftkings* player-with-proj c/*solution-cnt* c/*draftking* proj-keyword)
      players-proj
    )))

(defn lpsolve-solve-fanduel
  [player-with-proj proj-keyword]
  (print-solutions
    (lpsolve-solve-multiple constants/*team-salary-fanduel* player-with-proj c/*solution-cnt* c/*fanduel* proj-keyword)
    (data/add-rotowires-projection player-with-proj c/*fanduel*)))

