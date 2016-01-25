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



(def ^:dynamic *rotogrind-draftking-id* "20")

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
             (println (str "Player already exists: " (:Name p)))))
        players-data)))


(defn init-players
  [db]
  (create-players db (add-first-list (data/init-players-data))))

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
  (for [[id name] [[18937 "Karl-Anthony Towns"]
                   [16800 "Michael Carter-Williams"]
                   [16869 "Kentavious Caldwell-Pope"]
                   [1279 "Al-Farouq Aminu"]
                   [18951 "Willie Cauley-Stein"]
                   [31805 "Bryce Dejean-Jones"]
                   [13896 "Michael Kidd-Gilchrist"]
                   [18951 "Cauley-Stein"]]]
    (mc/update db c/*collection* {:Name name}
               (assoc (mc/find-one-as-map db c/*collection* {:Name name})
                 :rotogrinder-id id))))

;draftkng -> data-side-id = 20

(defn add-data-to-player
  [db name key value]
  (mc/update db c/*collection* {:Name name}
             (assoc (mc/find-one-as-map db c/*collection* {:Name name})
               key value)))

(defn get-rotogrinder-data
  [db]
  (map
    (fn [{:keys [rotogrinder-id Name rotogrinder-events]}]
        (try
          (println (str "Getting data for " Name ", id=" rotogrinder-id " . . ." ))
          (let [url (str "https://rotogrinders.com/players/" rotogrinder-id "/stats?range=this-season")
                ret (utils/fetch-url url)
                stats-data (json/read-str (-> ret first :content first :content first) :key-fn keyword)]
            (add-data-to-player db Name :rotogrinder-events
              (map (fn [[event-id {:keys [data]}]]
                   (let [{:keys [player fantasy_points stats schedule team_id]} data
                         {:keys [collection]} fantasy_points
                         team_id (read-string team_id)
                         home-team-id (-> schedule :data :team_home :data :id)
                         dk-stats (first (filter #(= *rotogrind-draftking-id* (-> % second :data :site_id)) collection))
                         fpts (-> dk-stats second :data :value)
                         [game-date game-time] (string/split (-> schedule :data :time) #" ")]
                     {:event-id event-id
                      :draftking-fpts fpts
                      :game-timestamp (-> schedule :data :time)
                      :game-date game-date
                      :game-epoch (.getTime (.parse (SimpleDateFormat. "MM/dd/yy") game-date))
                      :team team_id
                      :home-game (= team_id home-team-id)}))
               stats-data)))
            (catch Exception e
              (println (str "ERROR in updating " Name " data, Exception: " e)))))
    (mc/find-maps db c/*collection* {})))



(defn ingest-data
  []
  (let [db (utils/get-db)]
    (init-players db)
    (get-rotogrinder-id-name db)
    (get-rotogrinder-data db)))



