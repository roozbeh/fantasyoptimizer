(ns rz.web
  [:require [org.httpkit.server :as srv]
            [compojure.route :as route]
            [compojure.core :as ccore]
            [hiccup.core :as h]
            [hiccup.page :as hp]
            [ring.util.response :as r]
            [clojure.data.csv :as csv]
            [cemerick.url :refer (url url-encode)]
            [clojure.string :as string]
            [rz.optimizer :as optimizer]
            [monger.collection :as mc]
            [rz.optimizers.utils :as utils]
            [rz.optimizers.constants :as c]]
  (:use [incanter core stats charts])
  (:import (java.io ByteArrayInputStream ByteArrayOutputStream)))


(defn header
  []
  [:head
   [:meta {:charset :utf-8}]
   [:meta {:http-equiv "X-UA-Compatible" :content "IE=edge" }]
   [:meta {:name "viewport" :content "width=device-width, initial-scale=1"}]

   [:meta {:name "description" :content "Fantasy Lineup Generator"}]
   [:meta {:name "author" :content "iPronto Systems LLC"}]
   [:link {:rel "icon" :href "/static/images/favicon.png"}]
   [:title "RotoPronto - Index"]
   (hp/include-css "/static/css/bootstrap.min.css")
   (hp/include-css "/static/css/starter-template.css")
   (hp/include-js "https://oss.maxcdn.com/html5shiv/3.7.2/html5shiv.min.js")
   (hp/include-js "https://oss.maxcdn.com/respond/1.4.2/respond.min.js")])

(defn navbar
  [page-name]
  [:nav {:class "navbar navbar-inverse navbar-fixed-top"}
   [:div {:class "container"}
    [:div {:class "navbar-header"}
     [:button {:type "button" :class "navbar-toggle collapsed"
               :data-toggle "collapse" :data-target "#navbar" :aria-expanded "false" :aria-controls="navbar"}
      [:span {:class "sr-only"} "Toggle navigation"]
      [:span {:class "icon-bar"}]
      [:span {:class "icon-bar"}]
      [:span {:class "icon-bar"}]]
     [:a {:href "#" :class "navbar-brand"} "RotoPronto"]]
    [:div {:id "navbar" :class "collapse navbar-collapse"}
     [:ul {:class "nav navbar-nav"}
      [:li {:class (if (= "home" page-name) "active" "")} [:a {:href "#"} "Home"]]
      [:li {:class (if (= "nba" page-name) "active" "")} [:a {:href "/nba"} "NBA"]]
      [:li {:class (if (= "nhl" page-name) "active" "")} [:a {:href "/nhl"} "NHL"]]]]]])


(defn show-landing-page
  [req]
  (h/html
    (header)
    [:body {:background "/static/images/Youth-soccer-indiana.jpg" }
     (navbar "home")
     [:div {:class "jumbotron"}
      [:div {:class "container"}
       [:h1 "RotoPronto Helps you Win"]
       [:p {:class "lead"}
        "RotoPronto finds the best lineups for daily fantasy competitions and
        would help you choose the best strategy to win."]
       [:p
        [:a {:class "btn btn-primary btn-lg" :href "#" :role "button"}
         "Learn more &raquo;"]]]]
     ]))


(defn show-nba-players-page
  [req players]
  (h/html
    (header)
    [:body
     (navbar "nba")
     [:div {:class "jumbotron"}
      [:div {:class "container"}
       [:p { :style "text-align: center"}
        [:a {:class "btn btn-primary btn-lg" :href "#" :role "button"}
         "Create NBA lineups"]]]]
     [:div {:class "container"}
      [:div {:class "row"}
       [:table {:class "table"}
        [:thead
         [:th "#"]
         [:th "Name"]
         [:th "Team"]
         [:th "Opponent Team"]
         [:th "Average FPTS"]]
        (map-indexed (fn [idx {:keys [_id Name TeamAbbrev opp-team espn-data]}]
                       [:tr
                        [:td (inc idx) ]
                        [:td [:a {:href (str "/nba/player/" (.toString _id))} Name]]
                        [:td TeamAbbrev]
                        [:td opp-team]
                        [:td (format "%2.2f" (double (utils/array->mean (map :draftking-fpts (:events espn-data)))))]]
                       )
                     players
                     )]]]]))
(defn show-nba-all
  [req]
  (show-nba-players-page req
                         (mc/find-maps (utils/get-db) c/*nba* {})))

(defn show-nba-player
  [req]
  (let [{:keys [route-params]} req
                {:keys [pid]} route-params]
      (if-let [db-player (mc/find-one-as-map db c/*collection* {:_id (org.bson.types.ObjectId. pid)})]
        (h/html
          (header)
          [:body
           (navbar "nba")
           [:div {:class "container"}
            [:div {:class "row"}
             [:div {:class "col-xs-6"}
              [:h1 (:Name db-player)]]
             [:div {:class "col-xs-6"}
              [:img {:src (str "/nba/player/chart/" pid) :height 200}]]]
            [:div {:class "row"}
             [:table {:class "table"}
              [:thead
               [:th "#"]
               [:th "Game Date"]
               [:th "Match"]
               [:th "Home Game?"]
               [:th "minutes"]
               [:th "FGM-FGA"]
               [:th "REB"]
               [:th "AST"]
               [:th "BLK"]
               [:th "STL"]
               [:th "TO"]
               [:th "PTS"]
               [:th "FantasyPoints"]]
              (map-indexed (fn [idx {:keys [opp-team home-game mins FGM-FGA rebounds assists blocks
                                            steals turnover points draftking-fpts game-date]}]
                             [:tr
                              [:td (inc idx) ]
                              [:td game-date]
                              [:td opp-team]
                              [:td (if home-game "HOME" "")]
                              [:td mins]
                              [:td FGM-FGA]
                              [:td rebounds]
                              [:td assists]
                              [:td blocks]
                              [:td steals]
                              [:td turnover]
                              [:td points]
                              [:td draftking-fpts]]
                             )
                           (:events (:espn-data db-player))
                           )]]]])
         (h/html
          (str "Player could not be found: " pid)))))


(defn show-nba-player-chart
  [req]
  (let [{:keys [route-params]} req
        {:keys [pid]} route-params]
        (if-let [db-player (mc/find-one-as-map db c/*collection* {:_id (org.bson.types.ObjectId. pid)})]
          (let [events (:events (:espn-data db-player))
                events (sort-by :game-epoch events)
                chart (time-series-plot (range (count events)) (map :draftking-fpts events))
                out-stream (ByteArrayOutputStream.)
                in-stream (do
                            (save chart out-stream)
                            (ByteArrayInputStream.
                              (.toByteArray out-stream)))]
            (-> (r/response in-stream)
                (r/header "Content-Type" "image/png")))
          (h/html "Player could not be found"))))

(defn show-nhl
  [req]
  (let [db (utils/get-db)]
    (h/html
      (header)
      [:body
       (navbar "nhl")
       [:div {:class "jumbotron"}
        [:div {:class "container"}
         [:p { :style "text-align: center"}
          [:a {:class "btn btn-primary btn-lg" :href "#" :role "button"}
           "Create NBA lineups"]]]]
       [:div {:class "container"}
        [:div {:class "row"}
         [:table {:class "table"}
          [:thead
           [:th "#"]
           [:th "Name"]
           [:th "Team"]
           [:th "Opponent Team"]
           [:th "Average FPTS"]]
          (map-indexed (fn [idx {:keys [Name TeamAbbrev opp-team espn-data]}]
                         [:tr
                          [:td (inc idx) ]
                          [:td Name]
                          [:td TeamAbbrev]
                          [:td opp-team]
                          [:td (format "%2.2f" (double (utils/array->mean (map :draftking-fpts (:events espn-data)))))]]
                         )
                       (mc/find-maps db c/*nhl* {}))
          ]
         ]
        ]
       ]))
  )

(defn get-user-by-id
  [])

(defn update-userinfo
  [])

(ccore/defroutes all-routes
                 (ccore/GET "/" [] show-landing-page)
                 (ccore/GET "/nba" [] show-nba-all)
                 (ccore/GET "/nhl" [] show-nhl)
                 (ccore/GET "/nba/player/:pid" [pid] show-nba-player)
                 (ccore/GET "/nba/player/chart/:pid" [pid] show-nba-player-chart)
                 ;(ccore/GET "/resptime" [] show-response-time)
                 ;(ccore/GET "/reqcnt-chart/:url" [url] show-request-chart)
                 ;(ccore/GET "/resptime-chart/:url" [url] show-resptime-chart)
                 ;(ccore/GET "/reqcnt-csv" []  show-request-cnt-csv)
                 ;(ccore/GET "/error_rates" [] show-error-rates)
                 (ccore/context "/user/:id" []
                                (ccore/GET / [] get-user-by-id)
                                (ccore/POST / [] update-userinfo))
                 (route/files "/static/") ;; static file url prefix /static, in `public` folder
                 (route/not-found "<p>Page not found.</p>")) ;; all other, return 404


(defonce server (atom nil))

(defn start-web-server
  []
  (println "Starting web server")
  (reset! server (srv/run-server all-routes {:port 8080})))

(defn stop-server []
  (when-not (nil? @server)
    ;; graceful shutdown: wait 100ms for existing requests to be finished
    ;; :timeout is optional, when no timeout, stop immediately
    (@server :timeout 100)
    (reset! server nil)))

(defn -main [&args]
  ;; The #' is useful when you want to hot-reload code
  ;; You may want to take a look: https://github.com/clojure/tools.namespace
  ;; and http://http-kit.org/migration.html#reload
  (start-web-server))