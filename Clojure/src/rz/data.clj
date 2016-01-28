(ns rz.data
  (:require [clojure.java.io :as io]
            [clojure.data.csv :as csv]
            [clojure.pprint :as pp]
            [clojure.math.combinatorics :as combo]
            [net.cgrand.enlive-html :as html]
            [rz.projection :as proj]
            [rz.optimizers.constants :as c]
            [monger.collection :as mc]
            [incanter.stats :refer :all]
            [rz.optimizers.utils :as utils]))

(def players-csv "../data/fd_nba_jan_27.csv")

;(def players-csv "../data/FanDuel-NBA-2016-01-23-14499-players-list.csv")


;(def projections-csv "projections.csv")
;(read-json-data "src/server/fantasy_players.csv")
;(util/read-csv-with-reader players-csv)


(defn- fix-pdata-keywords-fanduel
  [pdatas]
  (map (fn [{:keys [Salary FPPG Team Game] :as p}]
         (assoc p :name   (str ((keyword "First Name") p) " " ((keyword "Last Name") p))
                  :Name   (str ((keyword "First Name") p) " " ((keyword "Last Name") p))
                  :injury ((keyword "Injury Indicator") p)
                  :Salary (read-string Salary)
                  :FPPG (read-string FPPG)
                  :IsHome (some? (re-find (re-pattern (str "@" Team)) Game))))

       pdatas))


(defn- fix-pdata-keywords-draftking
  [pdatas]
  (map (fn [{:keys [Salary AvgPointsPerGame teamAbbrev GameInfo Name] :as p}]
         (assoc p :name   (:Name p)
                  :Name (if (nil? Name)
                          (str ((keyword "First Name") p) " " ((keyword "Last Name") p))
                          Name)
                  :injury ""
                  :Salary (read-string Salary)
                  :FPPG (if AvgPointsPerGame
                          (read-string AvgPointsPerGame)
                          0)
                  :IsHome (if (and (some? GameInfo) (some? teamAbbrev))
                            (some? (re-find (re-pattern (str "@" teamAbbrev)) GameInfo)))))
       pdatas))

(defn init-players-data
  []
  (let [data (with-open [in-file (io/reader players-csv)]
               (doall
                 (csv/read-csv in-file)))
        header (first data)
        label-rec (fn [r] (zipmap (map keyword header) r))]
      (map label-rec (rest data))))


(defn init-players-data-fanduel
  []
  (fix-pdata-keywords-fanduel (init-players-data)))

(defn init-players-data-draftking
  []
  (fix-pdata-keywords-draftking (init-players-data)))

;(defn init-projection-data
;  []
;  (let [data (with-open [in-file (io/reader projections-csv)]
;               (doall
;                 (csv/read-csv in-file)))
;        header (first data)
;        label-rec (fn [r] (zipmap (map keyword header) r))]
;    (map label-rec (rest data))))

(defn add-projection
  [player-data projection-data]
  (map (fn [p]
         (let [rname (re-pattern (str ((keyword "First Name") p) ".*" ((keyword "Last Name") p) ".*"))
               projection (filter (fn [pd] (re-find rname (:Player pd))) projection-data)]
           (if (empty? projection)
             (do
               (println (str "ERROR Could not find projection for " rname))
               (assoc p :projection (str (* (read-string (:FPPG p)) 0.5))))
             (do
               (assoc p :projection (:Value (first projection)))))))
       player-data))


(defn add-rotowires-projection
  [players-data contest-provider]
  (let [rotowires-data (proj/get-rotowires-projections contest-provider)]
    (map (fn [p]
           (let [projection (filter (fn [pd] (re-find (re-pattern  (str (:name p) ".*"))
                                                      (:title pd)))  rotowires-data)]
             (if (empty? projection)
                (do
                  (println (str "ERROR Could not find projection for " (:name p)))
                  (assoc p
                    :roto-wire-projection 0
                    :roto-wire-value (/ (* 1000 (:FPPG p)) (:Salary p)))
                  )
                (do
                  (assoc p
                    :roto-wire-projection (read-string (:points (first projection)))
                    :roto-wire-value (read-string (:value (first projection))))))))
         players-data)))

; WARNING: run every time globally
(def nba-team {
           :PG 2
           :SG 2
           :SF 2
           :PF 2
           :C 1
           })

(defn role-cnt
  [body role]
  (count
    (filter #(= (:Position %) role) body)))

(defn roles-cnt
  [body]
  {
   :PG (role-cnt body "PG")
   :SG (role-cnt body "SG" )
   :SF (role-cnt body "SF")
   :PF (role-cnt body "PF")
   :C  (role-cnt body "C")
   })


(defn get-projection-for-player
  [player]
  (let [{:keys [teamAbbrev GameInfo rotogrinder-events]} player
        is-home-game (some? (re-find (re-pattern (str "@" teamAbbrev)) GameInfo))
        pts-same-home (utils/nil->zero
                        (:draftking-fpts (last (sort-by :game-epoch
                                                        (filter #(= is-home-game (:home-game %)) rotogrinder-events)))))
        avg-games-pts (utils/array->mean
                        (take c/*average-games-count*
                              (map (comp utils/nil->zero :draftking-fpts)
                                   (sort-by :game-epoch rotogrinder-events))))
        ;avg-games-pts-same (utils/array->mean
        ;                     (take c/*average-games-count*
        ;                           (map (comp utils/nil->zero :draftking-fpts)
        ;                                (sort-by :game-epoch
        ;                                         (filter #(= is-home-game (:home-game %))
        ;                                                 rotogrinder-events)))))
        last-mins (utils/nil->zero2 (:mins (last (sort-by :game-epoch rotogrinder-events))))]
    ;proj = -0.0845 + 0.1623*(nil->zero pts-same-home) + 0.5988*avg-games-pts + 0.2201 * last-mins
    (+ -0.0845
       (* 0.1623 pts-same-home)
       (* 0.5988 avg-games-pts)
       (* 0.2201 last-mins))
    ;(assoc pinfo :my-projection (+ -4.9414
    ;                                (* 1.2113 avg-games-pts)
    ;                                (* -0.5860 avg-games-pts-same)
    ;                                (* -4.3727 (utils/bool->int is-home-game))))

    ; proj = -4.9414 + 1.2113*avg-games-pts + -0.5860*avg-games-pts-same +  -4.3727 * (bool->int home-23)
    ))

(defn add-linear-projection
  [db players-data]
  (doall
    (map (fn [{:keys [Name] :as pinfo}]
           (assoc pinfo :my-projection
                        (get-projection-for-player
                          (mc/find-one-as-map db c/*collection* {:Name Name}))))
         players-data)))

(defn get-player-by-name
  [db name]
  (let [player (mc/find-one-as-map db c/*collection* {:Name name})]
    (assoc player :my-projection
                  (get-projection-for-player player))))
