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
    (for [[id name] [;NBA
                     [18937 "Karl-Anthony Towns"]
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
                     ;NHL
                     [14517 "Marc-Andre Fleury"]
                     [14425 "Oliver Ekman-Larsson"]
                     [14414 "John-Michael Liles"]
                     [14414 "John-Michael Liles"]
                     [17956 "Devante Smith-Pelly"]
                     [17319 "Pierre-Alexandre Parenteau"]
                     [14153 "Ryan Nugent-Hopkins"]
                     [14396 "Marc-Edouard Vlasic"]
                     [18728 "Jean-Francois Berube"]
                     [16694 "Jean-Gabriel Pageau"]
                     [18469 "Pierre-Edouard Bellemare"]

                     ]]
      (mc/update db c/*collection* {:Name name}
                 (assoc (mc/find-one-as-map db c/*collection* {:Name name})
                   :rotogrinder-id id)))))


(defn verify-data
  [{:keys [fd-salary dk-salary game-date]}]
  (and true
       (not (= 0 dk-salary))
       (not (= 0 fd-salary))
       ;(not (= game-date "2016-02-20"))
       ;(not (= game-date "2016-02-19"))
       ))

(defn map-events-nba
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

(defn map-events-nhl-old
  [stats-data]
  (map (fn [[event-id {:keys [data]}]]
         (let [{:keys [player fantasy_points stats schedule team_id game_stat_mappings]} data
               {:keys [min gp ga gaa w so sa l fpts otl sv]} game_stat_mappings
               {:keys [collection]} fantasy_points
               team_id (read-string team_id)
               home-team-id (-> schedule :data :team_home :data :id)
               dk-stats (first (filter #(= *rotogrind-draftking-id* (-> % second :data :site_id)) collection))
               dk-fpts (-> dk-stats second :data :value utils/nil->zero)
               fd-stats (first (filter #(= *rotogrind-fanduel-id* (-> % second :data :site_id)) collection))
               fd-fpts (-> fd-stats second :data :value utils/nil->zero)
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
            :home-game is-home?
            :team-name (if is-home? home-team away-team)
            :opp-name (if is-home? away-team home-team)
            :fd-salary fd-salary
            :dk-salary dk-salary
            :mins min
            :W w
            :L l
            :OTL otl
            :GA ga
            :GP gp
            :GAA gaa
            :SA sa
            :SV sv
            :SO so
            }))
       stats-data))

(defn map-events-nhl
  [stats-data]
  (map (fn [{:keys [opp min date gp pim pts ppa shg ppg g fpts sog a salary] :as e}]
           {:draftking-fpts (utils/nil->zero (get fpts (keyword *rotogrind-draftking-id*) 0))
            :fanduel-fpts (utils/nil->zero (get fpts (keyword *rotogrind-fanduel-id*) 0))
            :game-timestamp date
            :game-date date
            :game-epoch (.getTime (.parse (SimpleDateFormat. "yyyy-MM-dd") date))
            :home-game (not (some? (re-find #"@" opp)))
            :opp-team (second (string/split opp #" " ))
            :fd-salary (utils/nil->zero (get salary (keyword *rotogrind-fanduel-id*) 0))
            :dk-salary (utils/nil->zero (get salary (keyword *rotogrind-draftking-id*) 0))
            :mins min
            :GP gp
            :PTS pts
            :SHG shg
            :G g
            :SOG sog
            :A a
            :PIM pim
            :PPA ppa
            :PPG ppg
            ;:event-data e
            })
       stats-data))

(defn map-events
  [stats-data]
  (if (c/nhl?)
    (map-events-nhl stats-data)
    (map-events-nba stats-data)))

(defn ingest-player-info-old
  [db {:keys [rotogrinder-id Name]}]
  (let [url (str "https://rotogrinders.com/players/" rotogrinder-id "/stats?range=this-season")
        ret (utils/fetch-url url)
        stats-data (json/read-str (-> ret first :content first :content first) :key-fn keyword)]
    (scrap/add-data-to-player db Name :rotogrinder-events
                        (filter verify-data (map-events stats-data)))))

(defn get-events-for-player
  [db Name rotogrinder-id]
  (let [url (str "https://rotogrinders.com/players/jakob-silfverberg-" rotogrinder-id)
      ret (utils/fetch-url url)
      script-str (-> (html/select ret [:body :script]) first :content first)
      script-str (.substring script-str 24)
      script-str (.substring script-str 0 (- (count script-str) 150))
      data (json/read-str script-str :key-fn keyword)
      stats-data (:this-season data)]
  (map-events stats-data)))

(defn ingest-player-info
  [db {:keys [rotogrinder-id Name]}]
  (scrap/add-data-to-player db Name :rotogrinder-events
                            (filter verify-data
                                    (get-events-for-player db Name rotogrinder-id))))


(defn get-date-info
  [rotogrinder-id game-date-str]
  (let [ret (utils/fetch-url (str "https://rotogrinders.com/players/" rotogrinder-id "/stats?range=this-season"))
        stats-data (json/read-str (-> ret first :content first :content first) :key-fn keyword)]
                              (filter (fn [ev]
                                        ;(println (str "Event: " (:game-date ev) " -> " (:fanduel-fpts ev)))
                                        (= game-date-str (:game-date ev))) (map-events stats-data))))

(defn force-ingest-data
  [db players-data]
  (doall
    (pmap
      (fn [{:keys [Name]}]
        (try
          (let [{:keys [rotogrinder-id Name] :as player-data}
                (mc/find-one-as-map db c/*collection* {:Name Name})]
            (println (str "Getting data for " Name ", id=" rotogrinder-id " . . ." ))
            (ingest-player-info db player-data))
          (catch Exception e
            (println (str "ERROR in updating " Name " data, Exception: " e)))))
      players-data)))

(defn get-rotogrinder-data
  [db force-update]
  (doall
    (pmap
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
    (if force-update
      (force-ingest-data db players-data)
      (get-rotogrinder-data db force-update))))

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


(defn get-player-score
  [name fpts-kwd real-date]
  (let [db (utils/get-db)
        {:keys [Name rotogrinder-id]} (mc/find-one-as-map db c/*collection* {:Name name})
        events (get-events-for-player db Name rotogrinder-id)
        ev (first (filter #(= real-date (:game-date %)) events))
        score (fpts-kwd ev)]
    (println (str name " -> " score))
    score))

(def get-player-score-memo
  (memoize get-player-score))