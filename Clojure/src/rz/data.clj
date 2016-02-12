(ns rz.data
  (:require [clojure.java.io :as io]
            [clojure.data.csv :as csv]
            [clojure.pprint :as pp]
            [clojure.math.combinatorics :as combo]
            [net.cgrand.enlive-html :as html]
            [rz.projection :as proj]
            [rz.optimizers.constants :as c]
            [monger.collection :as mc]
            [monger.operators :refer :all]
            [incanter.stats :refer :all]
            [rz.optimizers.utils :as utils]
            [clojure.string :as string]))

(def players-csv-fd "../data/fd_nba_feb_11.csv")
;(def players-csv-dk "../data/dk_nba_jan_30.csv")

(def lineup-csv-dk "../data/dk_nba_linup_feb_11.csv")

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


;(defn- fix-pdata-keywords-draftking
;  [pdatas]
;  (map (fn [{:keys [Salary AvgPointsPerGame teamAbbrev GameInfo Name] :as p}]
;         (assoc p :name   (:Name p)
;                  :Name (if (nil? Name)
;                          (str ((keyword "First Name") p) " " ((keyword "Last Name") p))
;                          Name)
;                  :injury ""
;                  :Salary (read-string Salary)
;                  :FPPG (read-string AvgPointsPerGame)
;                  :IsHome (if (and (some? GameInfo) (some? teamAbbrev))
;                            (some? (re-find (re-pattern (str "@" teamAbbrev)) GameInfo)))))
;       pdatas))

(defn- fix-pdata-keywords-draftking2
  [pdatas]
  (map (fn [{:keys [GameInfo] :as p}]
         (let [TeamAbbrev ((keyword "TeamAbbrev ") p)
               ID ((keyword " ID") p)
               Name ((keyword " Name") p)
               NameID ((keyword "Name + ID") p)
               Salary ((keyword " Salary") p)
               TeamAbbrev ((keyword "TeamAbbrev ") p)
               IsHome (if (and (some? GameInfo) (some? TeamAbbrev))
                        (some? (re-find (re-pattern (str "@" TeamAbbrev)) GameInfo)))
               ]
           (dissoc
           (assoc p
                  :Name Name
                  :NameID NameID
                  :injury ""
                  :Salary (read-string Salary)
                  :ID (read-string ID)
                  :TeamAbbrev TeamAbbrev
                  :opp-team (if IsHome
                              (first (string/split GameInfo #"@"))
                              (first (string/split (second (string/split GameInfo #"@")) #" ")))
                  :IsHome IsHome)
           (keyword " Salary") (keyword " ID") :PF :SF :SG :UTIL :F :C :G :PG (keyword " Name")
           (keyword "") (keyword "Name + ID") (keyword "TeamAbbrev ")
           )))
       pdatas))

(defn- load-csv-data
  [csv-name]
  (let [data (with-open [in-file (io/reader csv-name)]
               (doall
                 (csv/read-csv in-file)))
        header (first data)
        label-rec (fn [r] (zipmap (map keyword header) r))]
      (map label-rec (rest data))))


(defn init-players-data-fanduel
  []
  (fix-pdata-keywords-fanduel (load-csv-data players-csv-fd)))

;(defn init-players-data-draftking
;  []
;  (fix-pdata-keywords-draftking (load-csv-data players-csv-dk)))

(defn init-players-data-draftking2
  []
  (fix-pdata-keywords-draftking2 (load-csv-data lineup-csv-dk)))

(defn remove-injured
  [players-data]
  (let [fd-data (init-players-data-fanduel)]
    (filter (fn [p]
              (let [fd-p (filter #(= (:Name %) (:Name p)) fd-data)]
                (if (empty? fd-p)
                  (do
                    (println (str "Warning: no injury data available for " (:Name p)))
                    true)
                  (do
                    (if (= "O" (:injury (first fd-p)))
                      (println "Removing injured player: " (:Name p)))
                    (not (= "O" (:injury (first fd-p))))))))
            players-data)))

;(defn init-players-data
;  []
;  (let [map-df (init-players-data-fanduel)
;        map-dk (init-players-data-draftking)]
;    (map (fn [{:keys [:Name] :as p-df}]
;           (dissoc (merge p-df (first (filter #(= Name (:Name %)) map-dk)))
;                   :Salary (keyword "")))
;         (filter #(not (= "O" (:injury %))) map-df))))

;(defn init-projection-data
;  []
;  (let [data (with-open [in-file (io/reader projections-csv)]
;               (doall
;                 (csv/read-csv in-file)))
;        header (first data)
;        label-rec (fn [r] (zipmap (map keyword header) r))]
;    (map label-rec (rest data))))

;(defn add-projection
;  [player-data projection-data]
;  (map (fn [p]
;         (let [rname (re-pattern (str ((keyword "First Name") p) ".*" ((keyword "Last Name") p) ".*"))
;               projection (filter (fn [pd] (re-find rname (:Player pd))) projection-data)]
;           (if (empty? projection)
;             (do
;               ;(println (str "ERROR Could not find projection for " rname))
;               (assoc p :projection (str (* (read-string (:FPPG p)) 0.5))))
;             (do
;               (assoc p :projection (:Value (first projection)))))))
;       player-data))

(defn add-rotowires-projection
  [players-data contest-provider]
  (let [rotowires-data (proj/get-rotowires-projections contest-provider)]
    (map (fn [p]
           (let [projection (filter (fn [pd] (re-find (re-pattern  (str (:Name p) ".*"))
                                                      (:title pd)))  rotowires-data)]
             (if (empty? projection)
                p
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

;(defn filter-high-sd
;  [players-data db]
;  (filter (fn [{:keys [Name]}]
;            (let [db-player (mc/find-one-as-map db c/*collection* {:Name Name})
;                  events (:rotogrinder-events db-player)
;                  scores (map (comp utils/nil->zero :draftking-fpts) events)]
;              (< (sd scores) 10)))
;          players-data))


(defn choose-player-for-pos
  [pos players]
  (if (= 1 (count players))
    (first players)
    (let [poses (map first players)]
      (if (some #(= % pos) poses)
        (first (filter #(= (first %) pos) players))
        (cond
          (= pos "G")  (or (choose-player-for-pos "PG" players)
                           (choose-player-for-pos "SG" players))
          (= pos "F")  (or (choose-player-for-pos "PF" players)
                           (choose-player-for-pos "SF" players))
          (= pos "UTIL")  (or (choose-player-for-pos "PF" players)
                              (choose-player-for-pos "SF" players)
                              (choose-player-for-pos "PG" players)
                              (choose-player-for-pos "SG" players)
                              (choose-player-for-pos "C" players)))))))

(defn convert-solution
  [header-array solution]
  (loop [headers header-array
         players solution
         result []]
    (if (empty? headers)
      result
      (let [pos-to-fill (first headers)
            p (choose-player-for-pos pos-to-fill players)]
        (recur (rest headers)
               (filter #(not (= (second %) (second p))) players)
               (conj result (str (second p))))))))

(defn save-solutions
  [solutions contest-provider]
  (let [date-str (.format (java.text.SimpleDateFormat. "MM_dd_yyyy_hh_mm")  (java.util.Date.))
        filename (str "../data/solutions_" contest-provider "_" date-str ".csv")
        header-array ["PG","SG","SF","PF","C","G","F","UTIL"]
        solutions-array (map (partial convert-solution header-array) solutions)
        ]
    (with-open [out-file (io/writer filename)]
      (csv/write-csv out-file [header-array])
      (csv/write-csv out-file solutions-array))
    (println (str "Solutions written in " filename))))


(defn save-projections
  [db players-proj]
  (let [date-str (.format (java.text.SimpleDateFormat. "MMddyyyy")  (java.util.Date.))]
    (mc/remove db c/*collection* {:type :projections :date date-str})
    (mc/insert db c/*collection*
               {:type :projections
                :date date-str
                :proj players-proj})
    (println (str "projection updated for: " date-str))))

(defn save-actual
  [db player-names date-str]
  (mc/remove db c/*collection* {:type :actual :date date-str})
  (mc/insert db c/*collection*
           {:type :actual
            :date date-str
            :actual
                  (map (fn [{:keys [rotogrinder-events Name]}]
                         [Name (:draftking-fpts
                                 (first
                                   (filter #(= date-str (:game-date %))
                                           rotogrinder-events)))])
                       (mc/find-maps db c/*collection* {:Name {$in player-names}}))}))

(defn filter-suckers
  [db players-data]
  (filter (fn [{:keys [Name]}]
            (let [db-player (mc/find-one-as-map db c/*collection* {:Name Name})
                  events (sort-by :game-epoch (:events (:espn-data db-player)))
                  last-point (get (last events) :points 0)
                  before-last-point (get (last (butlast events)) :points 0)
                  before2-last-point (get (last (butlast (butlast events))) :points 0)]
              ;(println (str Name " 1:" last-point " 2:" before-last-point " 3:" before2-last-point))
              (if (or (and (= 0 last-point) (<= before-last-point 4) (= 0 before2-last-point))
                      (and (= 0 last-point) (= before-last-point 0)))
                (do (println (str "Sucker: " Name))
                    false)
                true)))
          players-data))
