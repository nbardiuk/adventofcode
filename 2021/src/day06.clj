(ns day06)

(defn- parse-longs [input]
  (->> input (re-seq #"\d") (map parse-long)))

(def family-size
  (memoize
   (fn [age days]
     (->> (range (- days age) 0 -7)
          (map #(family-size 9 %))
          (reduce + 1)))))

(defn solution [days input]
  (->> (parse-longs input)
       (map #(family-size % days))
       (reduce +)))

(def part1 (partial solution 80))
(def part2 (partial solution 256))

