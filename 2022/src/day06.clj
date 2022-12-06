(ns day06)

(defn index-of [p xs]
  (->> xs
       (keep-indexed (fn [i x] (when (p x) i)))
       first))

(defn start-of-marker [n input]
  (->> input
       (partition n 1)
       (index-of #(apply distinct? %))
       (+ n)))

(defn part1 [input]
  (start-of-marker 4 input))

(defn part2 [input]
  (start-of-marker 14 input))
