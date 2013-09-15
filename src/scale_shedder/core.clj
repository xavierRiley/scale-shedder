(ns scale-shedder.core
    (:use [overtone.music.pitch]))
(def lownote (note :F2))
(def highnote (note :A4))
(def note-count 16)
(def fullrange (range lownote (inc highnote)))
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
                                                              (vec (partition note-count notes)))))
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

(defn make-saw
  "Takes an ascending range and produces an ascending and descending
   sequence of those nubmers"
  [rng]
  (concat (vec rng) (reverse (subvec (vec rng) 1 (- (count rng) 1)))))

;; IDEAS
(def master-range (make-saw (range lownote (inc highnote))))
(defn master-scale [saw]
  (into [] (map-indexed
            (fn [idx nt]
              {
               :index idx
               :midi-note nt
               :note-name (find-note-name nt)
               :pitch-class (find-pitch-class-name nt)
              })
            (vec saw))))

(defn pitch-set-for-scale [scale-range]
  (set (map find-pitch-class-name scale-range)))

(defn scale-dups [seq]
  (for [[id freq] (frequencies (map :midi-note seq))  ;; get the frequencies, destructure
        :when (> freq 1)]            ;; this is the filter condition
   id))

(defn annotate-with-scale [saw pitch-set]
  (map (fn [nt] (merge nt (if (and (contains? pitch-set (:pitch-class nt))
                                  ()) {:active true}))) saw))

(def c-set (pitch-set-for-scale (scale :C4 :major)))
(def ab-set (pitch-set-for-scale (scale :Ab4 :major)))
(def eb-set (pitch-set-for-scale (scale :Eb4 :major)))
(def e-set (pitch-set-for-scale (scale :E4 :major)))

(defn scale-set [tone scale-type] (pitch-set-for-scale (scale tone scale-type)))
(defn scale-saw [pitch-set] (map first (partition-by (fn [x] (:note-name x))
                                                     (filter
                                                      (fn [x] (:active x))
                                                      (annotate-with-scale (master-scale master-range) pitch-set)))))
(defn next-index [x]
  (cond
   (= x 0) 1
   (= x (:index (last (master-scale master-range)))) 0
   :else (inc x)))

(defn not-equal-to [x y]
  (if (= x y) nil true))

(defn scale-seq
  ([pitch-set] (take note-count (cycle (scale-saw pitch-set))))
  ([pitch-set prev-scale] (concat prev-scale
                                  (take note-count
                                        (cycle
                                          (rotate-while
                                           (fn [x] (< (:index x) (next-index (:index (last prev-scale)))))
                                           (scale-saw pitch-set)))))))

;; (defn filter-consecutive [coll]
;; (when-let [[f & r] (seq coll)]
;; (if (= f (first r))
;;  (filter-consecutive r)
;;  (cons f (filter-consecutive r)))))

;; THIS WORKS - I think
(println (output-vextab (map calculate-string-and-fret
                             (vec (map :note-name
                                       (scale-seq e-set
                                                  (scale-seq ab-set
                                                             (scale-seq c-set
                                                                        (scale-seq eb-set)))))))))

;; so this is messed up
;; It currently tries to continue from 32 beat patterns
;; Which is not accurate
;; It needs to calculate an upward arch and a downward arch
;; rotate as necessary and then cycle that rotation
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
  (into [] (map calculate-string-and-fret (->>
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
    (scale-from (scale-within-range (scale-field (nth all-the-scales 11) :major)) :prev-scale)))))
