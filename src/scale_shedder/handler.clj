(ns scale-shedder.handler
  (:use compojure.core
        scale-shedder.core
        scale-shedder.views)
  (:require [compojure.handler :as handler]
            [compojure.route :as route]))

(defroutes app-routes
  (GET "/" [] (index-page))
  (route/not-found "Not Found"))

(def app
  (handler/site app-routes))
