(ns day11)

(defn parse-longs [input]
  (->> input (re-seq #"\d") (mapv parse-long)))

(defn- fix-point [f x]
  (->> (iterate f x)
       (partition 2 1)
       (drop-while #(apply not= %))
       ffirst))

(defn neighbours [i]
  (let [x (mod i 10)
        y (quot i 10)]
    (for [dx [-1 0 1]
          dy [-1 0 1]
          :let [x' (+ x dx)
                y' (+ y dy)]
          :when (and (<= 0 x' 9) (<= 0 y' 9)
                     (not= [x y] [x' y']))]
      (+ (* y' 10) x'))))

(defn flash [levels]
  (reduce
   (fn [levels pos]
     (if (< 9 (get levels pos))
       (->> (neighbours pos)
            (remove #(zero? (get levels %)))
            (reduce #(update %1 %2 inc) (assoc levels pos 0)))
       levels))
   levels (range (count levels))))

(defn step [levels]
  (->> levels (mapv inc) (fix-point flash)))

(defn part1 [input]
  (->> input
       parse-longs
       (iterate step)
       (take 101)
       (mapcat #(filter zero? %))
       count))

(defn part2 [input]
  (->> input
       parse-longs
       (iterate step)
       (keep-indexed #(when (every? zero? %2) %1))
       first))
