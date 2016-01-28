(ns rz.scrap.rotogrinder
  (:require [monger.core :as mg]
            [monger.collection :as mc]
            [rz.data :as data]
            [rz.optimizers.utils :as utils]
            [net.cgrand.enlive-html :as html]
            [clojure.data.json :as json]
            [cemerick.url :as url]
            [clojure.string :as string]
            [clojure.pprint :as pp]
            [rz.optimizers.constants :as c]
            )
  (:import [com.mongodb MongoOptions ServerAddress]))
(import java.text.SimpleDateFormat)


;draftkng -> data-side-id = 20

(def ^:dynamic *rotogrind-draftking-id* "20")
(def ^:dynamic *rotogrind-fanduel-id* "2")

(defn add-first-list
  [players]
  (map (fn [p]
         (let [[first last] (string/split (:Name p) #" ")]
           (assoc p :first-name first
                    :last-name last)))
       players))

(defn create-players
  [db players-data]
  (doall
    (map (fn [p]
           (if (empty? (mc/find-maps db c/*collection* {:Name (:Name p)}))
             (mc/insert db c/*collection* p)
             ;(println (str "Player already exists: " (:Name p)))
             ))
        players-data)))


(defn init-players
  [db content-provider]
  (create-players db (add-first-list
                       (if (= content-provider c/*draftking*)
                         (data/init-players-data-draftking)
                         (data/init-players-data-fanduel)))))

(defn get-rotogrinder-id-name
  [db]
  (doall
    (map (fn [p]
         (println (str "Getting data for " (:Name p) ))
         (try
           (let [url (str "https://rotogrinders.com/players/autocomplete?term=" (url/url-encode (:Name p)) "&sport=")
                 _ (println (str "Requesting " url))
                 ret (utils/fetch-url url)
                 data (first (json/read-str (first (:content (first (:content (first ret))))) :key-fn keyword))
                 _ (pp/pprint data)
                 ]
             (mc/update db c/*collection* {:Name (:Name p)}
                        (assoc p  :rotogrinder-id (:id data)
                                  :rotogrinder-name (:name data))))
           (catch Exception e (println (str "ERROR: Could not find info for " (:Name p))))))
       (mc/find-maps db c/*collection* {:rotogrinder-id nil})))
  (doall
    (for [[id name] [[18937 "Karl-Anthony Towns"]
                     [16800 "Michael Carter-Williams"]
                     [16869 "Kentavious Caldwell-Pope"]
                     [1279 "Al-Farouq Aminu"]
                     [18951 "Willie Cauley-Stein"]
                     [31805 "Bryce Dejean-Jones"]
                     [13896 "Michael Kidd-Gilchrist"]
                     [18951 "Cauley-Stein"]
                     [18956 "T McConnell"]
                     [18626 "Danté Exum"]
                     [16846 "Phil (Flip) Pressey"]
                     ]]
      (mc/update db c/*collection* {:Name name}
                 (assoc (mc/find-one-as-map db c/*collection* {:Name name})
                   :rotogrinder-id id)))))


(defn add-data-to-player
  [db name key value]
  (mc/update db c/*collection* {:Name name}
             (assoc (mc/find-one-as-map db c/*collection* {:Name name})
               key value)))

(defn ingest-player-info
  [db {:keys [rotogrinder-id Name rotogrinder-events]}]
  (let [url (str "https://rotogrinders.com/players/" rotogrinder-id "/stats?range=this-season")
        ret (utils/fetch-url url)
        stats-data (json/read-str (-> ret first :content first :content first) :key-fn keyword)]
    (add-data-to-player db Name :rotogrinder-events
                        (doall
                          (map (fn [[event-id {:keys [data]}]]
                                 (let [{:keys [player fantasy_points stats schedule team_id game_stat_mappings]} data
                                       {:keys [collection]} fantasy_points
                                       team_id (read-string team_id)
                                       home-team-id (-> schedule :data :team_home :data :id)
                                       dk-stats (first (filter #(= *rotogrind-draftking-id* (-> % second :data :site_id)) collection))
                                       dk-fpts (-> dk-stats second :data :value)
                                       fd-stats (first (filter #(= *rotogrind-fanduel-id* (-> % second :data :site_id)) collection))
                                       fd-fpts (-> fd-stats second :data :value)
                                       [game-date game-time] (string/split (-> schedule :data :time) #" ")
                                       is-home? (= team_id home-team-id)
                                       home-team (-> schedule :data :team_home :data :hashtag)
                                       away-team (-> schedule :data :team_away :data :hashtag)]
                                   {:event-id event-id
                                    :draftking-fpts dk-fpts
                                    :fanduel-fpts fd-fpts
                                    :game-timestamp (-> schedule :data :time)
                                    :game-date game-date
                                    :game-epoch (.getTime (.parse (SimpleDateFormat. "MM/dd/yy") game-date))
                                    :team team_id
                                    :mins (:min game_stat_mappings)
                                    :home-game is-home?
                                    :team-name (if is-home? home-team away-team)
                                    :opp-name (if is-home? away-team home-team)
                                    }))
                               stats-data)))))

(defn get-rotogrinder-data
  [db]
  (doall
    (map
      (fn [{:keys [rotogrinder-id Name] :as player-data}]
          (try
            (println (str "Getting data for " Name ", id=" rotogrinder-id " . . ." ))
            (ingest-player-info db player-data)
            (catch Exception e
              (println (str "ERROR in updating " Name " data, Exception: " e)))))
      (mc/find-maps db c/*collection* {:rotogrinder-events nil}))))


(defn ingest-data
  [content-provider]
  (let [db (utils/get-db)]
    (init-players db content-provider)
    (get-rotogrinder-id-name db)
    (get-rotogrinder-data db)))

