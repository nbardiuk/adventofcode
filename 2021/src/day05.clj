(ns day05)

(defn- parse-lines [input]
  (->> input
       (re-seq #"\d+")
       (map parse-long)
       (partition 2)
       (partition 2)))

(defn- range-inc [start end step]
  (range start (+ end step) step))

(defn- direction [start end]
  (if (<= start end) 1 -1))

(defn- diagonal? [[[ax ay] [bx by]]]
  (and (not= ax bx) (not= ay by)))

(defn- points [[[ax ay] [bx by] :as line]]
  (let [xrange (range-inc ax bx (direction ax bx))
        yrange (range-inc ay by (direction ay by))]
    (if (diagonal? line)
      (map vector xrange yrange)
      (for [x xrange, y yrange] [x y]))))

(defn solution [keep-line? input]
  (->> (parse-lines input)
       (filter keep-line?)
       (mapcat points)
       frequencies
       (filter #(< 1 (val %)))
       count))

(def part1 (partial solution (comp not diagonal?)))
(def part2 (partial solution identity))
