(ns rz.projection
  (:require [clojure.java.io :as io]
            [clojure.data.csv :as csv]
            [clojure.pprint :as pp]
            [net.cgrand.enlive-html :as html]
            [rz.optimizers.constants :as constants]
            [rz.optimizers.utils :as utils]
            [rz.optimizers.constants :as c]))


(comment

;  csv = []
;  rows = $('#projection-data tr');
;  for(i =0;i < rows.length;i++) {
;        cells = $(rows[i]).find('td,th');
;        csv_row = [];
;        for (j=0;j<cells.length;j++) {
;              csv_row.push(cells[j].innerText);
;}
;csv.push(csv_row.join(","));
;}
;output = csv.join("\n")
;

  )


;lock, name, team, Opp, Pos, Mins, Salary, Points, Value

(defn get-rotowires-projections-nba
  [contest-provider]
  (let [url (if (= contest-provider constants/*fanduel*)
              (str "http://www.rotowire.com/daily/" c/*active-sport* "/optimizer.htm")
              (str "http://www.rotowire.com/daily/" c/*active-sport* "/optimizer.htm?site=DraftKings"))
        data (utils/fetch-url url)
        players (html/select data [:table#playerPoolTable :#players :tr])]
    (map (fn [p]
           (let [pdata (html/select p [:td])]
             {:name (-> (second pdata) :content first :attrs :title)
              :title (-> (second pdata) :content first :content first)
              :team (-> (nth pdata 2) :content first)
              :opp (-> (nth pdata 3) :content first)
              :pos (-> (nth pdata 4) :content first)
              :mins (-> (nth pdata 5) :content first)
              :salary (-> (nth pdata 6) :content first)
              :points (-> (nth pdata 7) :content first)
              :value (-> (nth pdata 8) :content first)}
             ))
         players)))


(defn get-rotowires-projections-nhl
  [contest-provider]
  (let [url (if (= contest-provider constants/*fanduel*)
              (str "http://www.rotowire.com/daily/" c/*active-sport* "/optimizer.htm")
              (str "http://www.rotowire.com/daily/" c/*active-sport* "/optimizer.htm?site=DraftKings"))
        data (utils/fetch-url url)
        players (html/select data [:table#playerPoolTable :#players :tr])]
    (map (fn [p]
           (let [pdata (html/select p [:td])]
             {:name (-> pdata second :content second :attrs :title)
              :team (-> (nth pdata 2) :content first)
              :opp (-> (nth pdata 3) :content first)
              :pos (-> (nth pdata 4) :content first)
              :line (-> (nth pdata 5) :content first)
              :pp-line (-> (nth pdata 6) :content first)
              :ml (-> (nth pdata 7) :content first)
              :ou (-> (nth pdata 8) :content first)
              :salary (-> (nth pdata 9) :content first)
              :points (-> (nth pdata 10) :content first)
              :value (-> (nth pdata 11) :content first)}
             ))
         players)))


(defn get-rotowires-projections
  [contest-provider]
  (cond
    (c/nba?) (get-rotowires-projections-nba contest-provider)
    (c/nhl?) (get-rotowires-projections-nhl contest-provider)
    true (throw (Exception. "Undefined sport!")))
    )

