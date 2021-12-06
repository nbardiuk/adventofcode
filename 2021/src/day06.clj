(ns day06)

(defn- parse-longs [input]
  (->> input (re-seq #"\d") (map parse-long)))

(def family-size
  (memoize
   (fn [cycle-day days-left]
     (->> (range (- days-left cycle-day) 0 -7)
          (map #(family-size 9 %))
          (reduce + 1)))))

(defn solution [days-left input]
  (->> (parse-longs input)
       (map #(family-size % days-left))
       (reduce +)))

(def part1 (partial solution 80))
(def part2 (partial solution 256))

