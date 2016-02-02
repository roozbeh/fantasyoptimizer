(ns rz.scrap.espn
  (:require [rz.optimizers.utils :as utils]
            [net.cgrand.enlive-html :as html]
            [clojure.string :as string]
            [clojure.pprint :as pp]
            [monger.collection :as mc]
            [rz.optimizers.constants :as c]
            [clj-time.format :as f])
  (:import (java.text SimpleDateFormat)))


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
  (let [year (if (some? (re-find #" 1[0-10]/" date-str))
               "2015"
               "2016")]
    (str date-str "/" year)))

(defn ingest-player
  [db {:keys [Name GameLogUrl TeamInfo] :as espn-player}]
  (let [db-player (mc/find-one-as-map db c/*collection* {:Name "Jeff Withey"})
        ret (utils/fetch-url GameLogUrl)
        header (second (html/select ret [:body :.tablehead :tr]))]
    {
     :ESPN-data espn-player
     :events    (map (fn [l]
                       (let [DATE (-> l :content first :content first)
                             [three-PM three-PA] (string/split  (-> l :content (nth 6) :content first ) #"-")
                             [FTM FTA] (string/split  (-> l :content (nth 8) :content first ) #"-")]
                         {
                          :DATE       DATE
                          :game-date  (.parse (java.text.SimpleDateFormat. "EEE MM/dd/yyyy") (add-year DATE))
                          :game-epoch (.getTime (.parse (SimpleDateFormat. "EEE MM/dd/yyyy") (add-year DATE)))
                          :opp-team (-> (html/select l [:.team-name :a]) first :content first )
                          :match-status (-> (html/select l [#{:.redfont :.greenfont}]) first :content first )
                          :match-result (-> l :content (nth 2) :content last :content first)
                          :mins (-> l :content (nth 3) :content first)
                          :FGM-FGA (-> l :content (nth 4) :content first)
                          :FieldGoalPercentage (-> l :content (nth 5) :content first)
                          :three-PM three-PM
                          :three-PA three-PA
                          :three-p-percentage (-> l :content (nth 7) :content first)
                          :FTM FTM
                          :FTA FTA
                          :FTP (-> l :content (nth 9) :content first)
                          :rebounds (-> l :content (nth 10) :content first)
                          :assists (-> l :content (nth 11) :content first)
                          :blocks (-> l :content (nth 12) :content first)
                          :steals (-> l :content (nth 13) :content first)
                          :fouls (-> l :content (nth 14) :content first)
                          :turnover (-> l :content (nth 15) :content first)
                          :points (-> l :content (nth 16) :content first)}))
                     (filter #(= 17 (count (:content %)))
                             (html/select ret [:body :.tablehead #{:.oddrow :.evenrow}])))}))


(comment
  ;player url: "http://espn.go.com/nba/player/gamelog/_/id/4240/avery-bradley"
  ;team url:
  )
