(ns rz.optimizers.utils
  (:require [clojure.pprint :as pp]
            [net.cgrand.enlive-html :as html]
            [rz.optimizers.constants :as c]
            [monger.core :as mg]
            [incanter.stats :refer :all]
            [monger.collection :as mc]
            [clj-http.client :as client]))

(defn nil->zero
  [x]
  (if (nil? x)
    0
    (if (string? x)
      (read-string x)
      x)))

(defn array->mean
  [x]
  (if (not (empty? x))
    (mean x)
    0))

(defn nil->zero2
  [x]
  (if (nil? x)
    0
    x))

(defn bool->int
  [x]
  (if x 1 0))


(defn get-db  []
  (mg/get-db (mg/connect) c/*db-name*))



;(defn print-team-metrics
;  [team]
;  (println (str "Team Salary: " (reduce + (map :Salary (vals team)))))
;  (println (str "Team Sum FPPG: " (reduce + (map :FPPG (vals team)))))
;  (println (str "Team Sum Projection: " (reduce + (map :roto-wire-projection (vals team)))))
;  )

(defn fetch-url [url]
  (html/html-resource (java.net.URL. url)))


(defn fetch-with-header
  [url headers]
  (let [response (client/get "https://www.draftkings.com/lineup/upload"
              {:headers {"Cookie" "optimizelyEndUserId=oeu1453422055655r0.3579712901264429; C3UID-145=4334520891453422056; C3UID=4334520891453422056; __qca=P0-1245848395-1453422057561; ajs_anonymous_id=%22ddcabd09-414c-488c-b5b0-19d7f10bb35c%22; GC-HTML5-UUID=196BFDA9-CDB3-4FF9-B8AA-5F5DB4F752A9; h2hDescriptionShown=true; lobby-onboarding=1; __ar_v4=%7C2BBUQFJ2FZE7DE47JUDVCP%3A20160222%3A1%7CA4QBDG7G5NEOZGEDGHI4M5%3A20160222%3A1%7CGEVSJNULLJBYJG2CISJY7C%3A20160222%3A1; __zlcmid=YofGpcuZPSnh8k; rafseen=1%7C2068542569; ASP.NET_SessionId=dlm0osc4pot1yzuoq5pxckbv; __utmt=1; C3S-145=on; _gat=1; EXC=1472461239:41; optimizelySegments=%7B%22215091535%22%3A%22false%22%2C%22215399201%22%3A%22search%22%2C%22215484327%22%3A%22gc%22%2C%223583230797%22%3A%22none%22%7D; optimizelyBuckets=%7B%7D; __utma=180391794.791047378.1453422056.1457808687.1458391531.223; __utmb=180391794.10.10.1458391531; __utmc=180391794; __utmz=180391794.1456556578.182.9.utmcsr=google|utmccn=(organic)|utmcmd=organic|utmctr=(not%20provided); ajs_group_id=null; ajs_user_id=6459034; optimizelyPPID=6459034; _ga=GA1.2.791047378.1453422056; gch=eyJSZXN0cmljdGVkIjpmYWxzZSwiU291cmNlIjoxLCJTdGF0dXMiOjEsIkV4cGlyZXMiOiIyMDE2LTAzLTE5VDE1OjUwOjI0LjM3Mjk0MjVaIiwiTG9jYXRpb24iOiJVUy1GTCIsIkhhc2giOiJzaUpMRHVWSTM0bU5DLzVKVDUvSlAvem1vSDExWVU5OGw0UXRwbzBHZHk4PSJ9; STIDN=eyJDIjoxMjIzNTQ4NTIzLCJTIjoxNDQ4MDQ1MTMyLCJTUyI6MTQ3MjQ2MTIzOSwiViI6MTMyNzU1MDgzNiwiTCI6MSwiRSI6IjIwMTYtMDMtMTlUMTM6MjA6MzguNTEwOTg2WiJ9; STH=3f48ed0e36383f6ff20af98b310a3dd077bc7701df641bd1ef8226a18bfe979f; VIDN=1327550836; SIDN=1448045132; SSIDN=1472461239; SN=1223548523; LID=1; SINFN=PID=100&AOID=124&PUID=6459034&SSEG=3:2-4:2&GLI=6826&LID=1&BID=0&BH=; jwe=/J5VTWpUtyGZODXGGMwKWuGAzttpLISmh37JGgPLcyI9pZnZJ9ECoT3TIqBEUfFFwj199s2xchRJEW9GPcL3PFZWTzBM5CsQm4VWc6JGtoYXoDFIGlwCXkHImzXAwUg4Gzh6t6M2OC5myYVU+S/khDielEw6VFwFV7Q1ZoqM/XM0EBXEK+UbUKwckSE1Zrr5CJWxyP4NPhOAdmIgJNuW43Jsz8cXcDCr9+m4RsSBGI+BlHyrhVTRH7h43CjQTrO12pjhBFrfiFKdx8aH5XUGlJ6vXdpw/W5HUHtVtoqySTwc+Z6+p+bNGmO0KRTdGgRqmmXR99cg3AjkKv08jPy/zBjAFyRTW4csgVRid5ENAyLlK7MTRFphjCrYT3PvTGZf4eaGCwfujZgZ9/PhqlrRFBGaXnM8QBJAgEqJGL/ZzrY77MFO3474Z0YktqR766IBIKG9H8tX+p+DH8xdkxuqzpx674prMB3qx5f17wOUMNwYYnC0IKtJP+usfMuRfjJWqCB+CNIlwSTewvbsOMOtBV7zDPEnjCsIm5+EawzsdW9cwzTngdO/7LwWzP0FkjKiDAV3TFuBp0FzK5mhrGM7qLFVGHMbnj49lvEz0mA9TFo=; iv=fI9AQ3jNWYUgWcSI/S8mRA==; uk=0LBsKs0DTfnZ0MZeamA9CkO4djYK6PyUUkUuKQptNOSo7QRRKJjgyESokPFoRZP//+EvkCO5EdOak9CSGp0QPA==; optimizelyPendingLogEvents=%5B%5D"}})
        ]

))