(ns scale-shedder.core
    (:use [overtone.music.pitch]))

(declare continue-scale-from-prev)

(defn rotate-while
  "Rotates a collection left while (pred item) is true. Will return a
   unrotated sequence if (pred item) is never true. Executes in O(n) time."
  [pred coll]
  (let [head (drop-while pred coll)]
    (take (count coll) (concat head coll))))

(defn get-dir [x]
  (let [penul (note (first (rest (reverse x)))) final (note (last x))]
  (cond
    (> penul final) :down
    (> final penul) :up)))

(defn scale-from [n &{:keys [beats prev-scale] :or {beats 32} }]
  (let [current-scale (take beats (cycle (concat (vec n) (reverse (subvec (vec n) 1 (- (count n) 1))))))]
    (concat
      prev-scale
      (if (not (empty? prev-scale))
        (continue-scale-from-prev n prev-scale)
        current-scale))))

(defn continue-scale-from-prev [current-scale prev-scale]
  (let [direction (get-dir prev-scale) last-note (last prev-scale)]
    (if (= direction :up)
      (rotate-while (fn [x] (>= (note x) (note last-note))) (scale-from current-scale))
      (rotate-while (fn [x] (>= (note x) (note last-note))) (scale-from (reverse current-scale)) ))))

(def lownote (note :F2))
(def highnote (note :A4))
(def fullrange (range lownote highnote))
(def stringmap [
    "1/6" "2/6" "3/6" "4/6" "5/6"
    "1/5" "2/5" "3/5" "4/5" "5/5"
    "1/4" "2/4" "3/4" "4/4" "5/4"
    "1/3" "2/3" "3/3" "4/3"
    "1/2" "2/2" "3/2" "4/2" "5/2"
    "1/1" "2/1" "3/1" "4/1" "5/1"
  ])

(defn calculate-string-and-fret [n]
  (last
    (find (zipmap fullrange stringmap) (note n))))

(defn output-vextab [notes]
  (map #(str "tabstave notation=true\n" "notes " % "\n") (map (partial clojure.string/join " ") 
       (vec (partition 8 notes)))))

(defn scale-notes-above-tone [scale note]
  (last (split-with (partial >= (dec note)) scale)))

(defn scale-notes-below-tone [scale note]
  (first (split-with (partial >= note) scale)))

(defn scale-within-range [scale &{:keys [start end] :or {start lownote end highnote} }]
  (vec (map find-note-name (scale-notes-below-tone (scale-notes-above-tone scale start) end))))

;;(definst harpsichord [freq 440]
;;  (let [duration 1]
;;    (*
;;      (line:kr 1 1 duration FREE)
;;      (pluck (* (white-noise) (env-gen (perc 0.001 5) :action FREE))
;;             1 1 (/ 1 freq) (* duration 2) 0.25))))

(def all-the-scales (vec (shuffle (list :F :Gb :G :Ab :A :Bb :B :C :Db :D :Eb :E))))
(defn generate-scales []
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
    (scale-from (scale-within-range (scale-field (nth all-the-scales 11) :major)) :prev-scale))))
(println all-the-scales)
