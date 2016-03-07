(ns rz.optimizers.constants)

(def ^:dynamic *team-salary* 60000 )

(def ^:dynamic *team-salary-draftkings* 50000 )
(def ^:dynamic *team-salary-fanduel* 60000 )

(def ^:dynamic *nhl* "nhl")
(def ^:dynamic *nba* "nba")

(def ^:dynamic *active-sport* *nba*)

(defn nhl? [] (= *active-sport* *nhl*))
(defn nba? [] (= *active-sport* *nba*))

(def ^:dynamic *fanduel* "fanduel")
(def ^:dynamic *draftking* "draftking")

(def ^:dynamic *db-name* "fantasy")
(def ^:dynamic *collection* *active-sport*)

(def ^:dynamic *average-games-count* 3)
(def ^:dynamic *average-games-count7* 7)

(def ^:dynamic *active-database*
  (cond
    (nba?) :espn
    (nhl?) :rotogrinder))


(def ^:dynamic *max-iterations* 25)

(def ^:dynamic *filter-date* "Fri 2/5/2016")

(def ^:dynamic *max-common-players-fanduel* 5)
(def ^:dynamic *max-common-players-draftkings*
  (cond
    (nba?) 6
    (nhl?) 8))

(def ^:dynamic *solution-cnt* 5)


