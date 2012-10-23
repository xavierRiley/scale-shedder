(defproject scale-shedder "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [overtone "0.7.1"]
                 [compojure "1.1.1"]]
  :plugins [[lein-ring "0.7.1"]]
  :ring {:handler scale-shedder.handler/app}
  :dev-dependencies [[ring-mock "0.1.2"]])
