(ns rz.scrap.rotogrinder
  (:require [monger.core :as mg]
            [monger.collection :as mc]
            [rz.data :as data]
            [rz.optimizers.utils :as utils]
            [rz.scrap.scrap :as scrap]
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
                     [18897 "Rondae Hollis-Jefferson"]
                     ]]
      (mc/update db c/*collection* {:Name name}
                 (assoc (mc/find-one-as-map db c/*collection* {:Name name})
                   :rotogrinder-id id)))))


(defn verify-data
  [{:keys [fd-salary dk-salary game-date]}]
  (and true
       (not (= 0 dk-salary))
       (not (= 0 fd-salary))
       ))

(defn map-events
  [stats-data]
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
               fd-salary (-> (filter #(= *rotogrind-fanduel-id* (-> % :data :site_id))
                                     (-> schedule :data :salaries :collection))
                             first :data :salary utils/nil->zero)
               dk-salary (-> (filter #(= *rotogrind-draftking-id* (-> % :data :site_id))
                                     (-> schedule :data :salaries :collection))
                             first :data :salary utils/nil->zero)
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
            :fd-salary fd-salary
            :dk-salary dk-salary
            }))
       stats-data))

(defn ingest-player-info
  [db {:keys [rotogrinder-id Name]}]
  (let [url (str "https://rotogrinders.com/players/" rotogrinder-id "/stats?range=this-season")
        ret (utils/fetch-url url)
        stats-data (json/read-str (-> ret first :content first :content first) :key-fn keyword)]
    (scrap/add-data-to-player db Name :rotogrinder-events
                        (filter verify-data (map-events stats-data)))))

(defn get-date-info
  [rotogrinder-id game-date-str]
  (let [ret (utils/fetch-url (str "https://rotogrinders.com/players/" rotogrinder-id "/stats?range=this-season"))
        stats-data (json/read-str (-> ret first :content first :content first) :key-fn keyword)]
                              (filter (fn [ev]
                                        ;(println (str "Event: " (:game-date ev) " -> " (:fanduel-fpts ev)))
                                        (= game-date-str (:game-date ev))) (map-events stats-data))))

(defn get-rotogrinder-data
  [db force-update]
  (doall
    (map
      (fn [{:keys [rotogrinder-id Name] :as player-data}]
          (try
            (println (str "Getting data for " Name ", id=" rotogrinder-id " . . ." ))
            (ingest-player-info db player-data)
            (catch Exception e
              (println (str "ERROR in updating " Name " data, Exception: " e)))))
      (mc/find-maps db c/*collection* (if force-update
                                        {}
                                        {:rotogrinder-events nil} ;only ones that do not events
                                        )))))


(defn ingest-data
  [players-data & {:keys [force-update] :or {force-update false}}]
  (println (str "ingest-data, force update: " force-update))
  (let [db (utils/get-db)]
    (scrap/create-players db players-data)
    (get-rotogrinder-id-name db)
    (get-rotogrinder-data db force-update)))



(defn add-rotogrinders-projection
  [db players-data contest-provider]
    (map (fn [{:keys [Name] :as p}]
           (println (str "Getting info for " Name))
           (let [{:keys [rotogrinder-id]} (mc/find-one-as-map db c/*collection* {:Name Name})
                 url (str "https://rotogrinders.com/players/"rotogrinder-id"/projection" )
                 _ (println url)
                 ret (utils/fetch-url url)
                 data (json/read-str (first (:content (first (:content (first ret))))) :key-fn keyword)]
             (assoc p
                   :rotogrinders-projection (get (:fpts (:stats data))
                                                 (keyword
                                                   (if (= c/*draftking* contest-provider)
                                                     *rotogrind-draftking-id*
                                                     *rotogrind-fanduel-id*) )))))
         players-data))


