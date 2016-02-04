(ns rz.scrap.espn
  (:require [rz.optimizers.utils :as utils]
            [net.cgrand.enlive-html :as html]
            [clojure.string :as string]
            [clojure.pprint :as pp]
            [monger.collection :as mc]
            [rz.optimizers.constants :as c]
            [clj-time.format :as f]
            [rz.scrap.scrap :as scrap])
  (:import (java.text SimpleDateFormat)
           (java.util Date)))


(defn get-teams
  []
  (let [ret (utils/fetch-url "http://espn.go.com/nba/teams")]
    (map (fn [{:keys [content attrs]}]
           (let [{:keys [href]} attrs
                 url-parts (string/split href #"/")
                 team-id (first (take-last 2 url-parts))]
             {
              :TeamName content
              :team-url href
              :team-id team-id
              :roster-url (str "http://espn.go.com/nba/team/roster/_/name/" team-id)
            })
           )
         (html/select ret [:body :.bi]))))

(defn get-players
  []
  (flatten
    (map (fn [{:keys [roster-url] :as team-info}]
           (let [ret (utils/fetch-url roster-url)]
             (map (fn [{:keys [content attrs]}]
                    (let [{:keys [href]} attrs
                          url-parts (string/split href #"_")]
                      {:Name (first content)
                       :PlayerURL href
                       :GameLogUrl (str (first url-parts) "gamelog/_" (second url-parts))
                       :TeamInfo team-info}))
                  (map (comp first :content) (html/select ret [:body :.sortcell])))))
         (get-teams))))

(defn add-year
  [date-str]
  (let [year (if (some? (re-find #" 1[0-9]/" date-str))
               "2015"
               "2016")]
    (str date-str "/" year)))

(defn calc-dk-score
  [{:keys [points rebounds assists steals blocks turnover three-PM]}]
  (let [double-cnt (utils/bool->int (or (>= points 10)
                                        (>= rebounds 10)
                                        (>= assists 10)
                                        (>= blocks 10)
                                        (>= steals 10)))
        double-double (utils/bool->int (>= double-cnt 2))
        triple-double (utils/bool->int (>= double-cnt 3))]
    (+ points
       (* 1.25 rebounds)
       (* 1.5 assists)
       (* 2 steals)
       (* 2 blocks)
       (* 0.5 three-PM)
       (- (* 0.5 turnover))
       (* 1.5 double-double)
       (* 3 triple-double))))

;3-pt FG = 3pts
;2-pt FG = 2pts
;FT = 1pt
;Rebound = 1.2pts
;Assist = 1.5pts
;Block = 2pts
;Steal = 2pts
;Turnover = -1pt
(defn calc-fd-score
  [{:keys [points rebounds assists steals blocks turnover three-PM FTM]}]
  (+ points
     FTM
     (* 1.2 rebounds)
     (* 1.5 assists)
     (* 2 steals)
     (* 2 blocks)
     (- turnover)))

(defn add-score
  [espn]
  (update-in espn [:events]
             (fn [events]
               (map (fn [e]
                      (assoc e
                        :draftking-fpts (calc-dk-score e)
                        :fanduel-fpts (calc-fd-score e)
                        ))
                    events))))

(defn fail-safe-game-log
  [GameLogUrl PlayerURL]
  (try
    (utils/fetch-url GameLogUrl)
    (catch Exception e
      (do
        (println "Could not get data from game log, trying player page")
        (try
          (utils/fetch-url PlayerURL)
          (catch Exception e
            (println "Original URL failed as well")
            []))))))

(defn filter-2016
  [ret]

  (if (and (not-empty ret)
           (some? (re-find #"2015-2016"
                          (-> (html/select ret [:body :.tablehead :.stathead html/first-child ]) first :content first))))
    (html/select ret [:body :.tablehead #{:.oddrow :.evenrow}])
    []))

(defn ingest-player
  [{:keys [GameLogUrl PlayerURL] :as espn-player}]
  (println GameLogUrl)
  {
   :ESPN-data   espn-player
   :ingest-date (.getTime (new Date))
   :events
                ;(filter
                ;  #(not (= (:DATE %) "Wed 2/3"))
                  (map (fn [l]
                       (let [DATE (-> l :content first :content first)
                             [three-PM three-PA] (string/split (-> l :content (nth 6) :content first) #"-")
                             [FTM FTA] (string/split (-> l :content (nth 8) :content first) #"-")]
                         {
                          :DATE                DATE
                          :game-date           (add-year DATE)
                          :game-epoch          (.getTime (.parse (SimpleDateFormat. "EEE MM/dd/yyyy") (add-year DATE)))
                          :home-game           (= "vs" (-> (html/select l [:.game-schedule :.game-location]) first :content first))
                          :opp-team            (or (-> (html/select l [:.team-name :a]) first :content first)
                                                   (-> (html/select l [:.team-name]) first :content first))
                          :match-status        (-> (html/select l [#{:.redfont :.greenfont}]) first :content first)
                          :match-result        (-> l :content (nth 2) :content last :content first)
                          :mins                (-> l :content (nth 3) :content first utils/nil->zero)
                          :FGM-FGA             (-> l :content (nth 4) :content first)
                          :FieldGoalPercentage (-> l :content (nth 5) :content first)
                          :three-PM            (utils/nil->zero three-PM)
                          :three-PA            three-PA
                          :three-p-percentage  (-> l :content (nth 7) :content first)
                          :FTM                 (utils/nil->zero FTM)
                          :FTA                 FTA
                          :FTP                 (-> l :content (nth 9) :content first)
                          :rebounds            (-> l :content (nth 10) :content first utils/nil->zero)
                          :assists             (-> l :content (nth 11) :content first utils/nil->zero)
                          :blocks              (-> l :content (nth 12) :content first utils/nil->zero)
                          :steals              (-> l :content (nth 13) :content first utils/nil->zero)
                          :fouls               (-> l :content (nth 14) :content first)
                          :turnover            (-> l :content (nth 15) :content first utils/nil->zero)
                          :points              (-> l :content (nth 16) :content first utils/nil->zero)}))
                     (filter #(= 17 (count (:content %)))
                             (filter-2016 (fail-safe-game-log GameLogUrl PlayerURL))))
                  ;)
   })

(def espn-name-mapping
  {"Lou Amundson" "Louis Amundson"
   "Joel Embiid" "Joel Embiid"
   "Johnny O'Bryant III" "Johnny O'Bryant"
   "Frank Kaminsky III" "Frank Kaminsky"
   "Otto Porter Jr." "Otto Porter"
   "JaKarr Sampson" "Jakarr Sampson"
   "JJ Hickson" "J.J. Hickson"
   "Dante Exum" "Danté Exum"
   ;"Joe Young" "Joseph Young"
   ;"Bryce Dejean-Jones" "Bryce Jones"
   ;"Patty Mills" "Patrick Mills"
   ;"R.J. Hunter" "RJ Hunter"
   ;"Ish Smith" "Ishmael Smith"
   ;"J.J. Barea" "Jose Juan Barea"
   ;"Bradley Beal" "Brad Beal"
   ;"Kelly Oubre Jr." "Kelly Oubre"
   ;"Devyn Marble" "Roy Devyn Marble"
   })


(defn map-espn-names
  [Name]
  (get espn-name-mapping Name Name))

(defn create-espn-playears
  []
  (let [db (utils/get-db)
        players (get-players)]
    (mc/insert db c/*collection* {:type :espn-data
                                  :players players})))

(defn ingest-one-player-code
  [db Name]
  (scrap/add-data-to-player db
                            Name
                            :espn-data (add-score
                                         (ingest-player (first
                                                          (filter #(= (:Name %) Name)
                                                                  (get-espn-players db)))))))

(defn get-espn-players
  [db]
  (let [data (mc/find-one-as-map db c/*collection* {:type :espn-data})]
    (assert data)
    (assert (:players data))
    (:players data)))

(defn ingest-data
  [players-data & {:keys [force-update] :or {force-update false}}]
  (println (str "ingest-data, force update: " force-update))
  (let [db (utils/get-db)
        players (get-espn-players db)]
    (scrap/create-players db players-data)
    (println "Players updated: "
             (count
               (map (fn [{:keys [Name] :as espn-player}]
                      (let [db-player (mc/find-one-as-map db c/*collection* {:Name (map-espn-names Name)})]
                        (if (nil? db-player)
                          (println (str "ERROR: Could not load player in database: '" (map-espn-names Name) "'"))
                          (if (or force-update
                                  (nil? (:espn-data db-player))
                                  (empty? (:events (:espn-data db-player))))
                            (do
                              (println (str "Loading data for " Name))
                              (scrap/add-data-to-player db (map-espn-names Name)
                                                        :espn-data (add-score (ingest-player espn-player))))))))
                    players)))))

(comment
  ;player url: "http://espn.go.com/nba/player/gamelog/_/id/4240/avery-bradley"
  ;team url:
  )
