(ns rz.scrap.scrap
  (:require [rz.optimizers.constants :as c]
            [monger.collection :as mc]))

(defn create-players
  [db players-data]
  (doall
    (map (fn [{:keys [Name] :as p}]
           (if (empty? (mc/find-maps db c/*collection* {:Name Name}))
             (mc/insert db c/*collection* p)
             ;(println (str "Player already exists: " (:Name p)))
             ))
         players-data)))

(defn add-data-to-player
  [db name key value]
  (mc/update db c/*collection* {:Name name}
             (assoc (mc/find-one-as-map db c/*collection* {:Name name})
               key value)))

