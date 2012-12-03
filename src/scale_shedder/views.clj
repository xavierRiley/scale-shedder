(ns scale-shedder.views
  (:use [overtone.music.pitch])
  (:use [scale-shedder core])
  (:use [hiccup core page]))

(defn index-page []
  (html5
    [:head
      [:title "Hello World"]
      ]
    [:body
      [:h1 "Hello World"]
      [:h2 "Welcome to shedder"]
      [:p 
        (map calculate-string-and-fret (->>
          (scale-from (scale-within-range (scale-field (nth all-the-scales 0) :major)))
          (scale-from (scale-within-range (scale-field (nth all-the-scales 1) :major)) :prev-scale)
          (scale-from (scale-within-range (scale-field (nth all-the-scales 2) :major)) :prev-scale)
          (scale-from (scale-within-range (scale-field (nth all-the-scales 3) :major)) :prev-scale)
          (scale-from (scale-within-range (scale-field (nth all-the-scales 4) :major)) :prev-scale)
          (scale-from (scale-within-range (scale-field (nth all-the-scales 5) :major)) :prev-scale)
          (scale-from (scale-within-range (scale-field (nth all-the-scales 6) :major)) :prev-scale)
          (scale-from (scale-within-range (scale-field (nth all-the-scales 7) :major)) :prev-scale)
          (scale-from (scale-within-range (scale-field (nth all-the-scales 8) :major)) :prev-scale)
          (scale-from (scale-within-range (scale-field (nth all-the-scales 9) :major)) :prev-scale)
          (scale-from (scale-within-range (scale-field (nth all-the-scales 10) :major)) :prev-scale)
          (scale-from (scale-within-range (scale-field (nth all-the-scales 11) :major)) :prev-scale)))
      ]]))
