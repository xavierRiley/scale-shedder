(ns scale-shedder.handler
  (:use compojure.core
        scale-shedder.core
        scale-shedder.views
        [ring.middleware.format-response :only [wrap-restful-response]])
  (:require [compojure.handler :as handler]
            [compojure.route :as route]
            [clojure.data.json :as json]))

(defn json-response [data & [status]]
  {:status (or status 200)
   :headers {
       "Content-Type" "application/json"
       "Access-Control-Allow-Origin" "*"}
   :body (str "var songs = " (json/write-str data))})

(defroutes app-routes
  (GET "/" [] 
    (json-response {"hello" 
      {"title" (reduce str all-the-scales)
       "artist" "me"
       "timeSignature" [4 4]
       "temp" 120
       "vexTabCode" (output-vextab (generate-scales))}}))
  (route/not-found "Not Found"))

(def app  (handler/site app-routes))
