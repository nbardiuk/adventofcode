(ns day15)

(defn read-numbers [input]
  (->> (re-seq #"\d+" input)
       (map read-string)))

(defn solution [n input]
  (let [xs (read-numbers input)
        seen (int-array n Integer/MAX_VALUE)]
    (->> xs (map-indexed #(aset seen %2 ^int %1)) dorun)
    (loop [item 0
           index (count xs)]
      (if (= (inc index) n)
        item
        (let [last-seen (min index (aget seen item))]
          (aset seen item index)
          (recur (- index last-seen)
                 (inc index)))))))

(defn part1 [input] (solution 2020 input))
(defn part2 [input] (solution 30000000 input))
