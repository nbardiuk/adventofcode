(ns day21)

(defn parse [input]
  (->> (re-seq #"\d+" input)
       (map parse-long)
       ((juxt second last))
       (mapv dec)))

(defn part1 [input]
  (loop [[p1 p2] (parse input)
         [s1 s2] [0 0]
         [roll & dice] (->> (range 1 101) cycle (partition 3) (map #(reduce + %)))
         turns 1]
    (let [np (mod (+ p1 roll) 10)
          ns (+ s1 np 1)]
      (if (<= 1000 ns)
        (* s2 turns 3)
        (recur [p2 np] [s2 ns] dice (inc turns))))))

(def roll-counts
  (->> (for [a (range 1 4)
             b (range 1 4)
             c (range 1 4)]
         (+ a b c))
       frequencies))

(def wins
  (memoize
   (fn [[p1 p2] [s1 s2] first?]
     (->> (for [[roll n] roll-counts]
            (let [np (mod (+ p1 roll) 10)
                  ns (+ s1 np 1)]
              (if (<= 21 ns)
                {first? n}
                (-> (wins [p2 np] [s2 ns] (not first?))
                    (update-vals #(* n %))))))
          (reduce (partial merge-with +))))))

(defn part2 [input]
  (->> (wins (parse input) [0 0] true)
       vals
       (reduce max)))
