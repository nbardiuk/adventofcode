(ns day14)

(defn parse-input [input]
  {:polymer (re-find #"^\w+" input)
   :insertion (->> (for [[_ [a b] [c]] (re-seq #"(\w\w) -> (\w)" input)]
                     [[a b] [a c b]])
                   (into {}))})

(def counts
  (memoize
   (fn [insertion polymer steps]
     (letfn [(into-counts [result polymer]
               (merge-with +
                           (update result (first polymer) (fnil dec 1))
                           (counts insertion polymer (dec steps))))]

       (if (zero? steps)
         (frequencies polymer)
         (->> (partition 2 1 polymer)
              (map insertion)
              (reduce into-counts {})))))))

(defn checksum [counts]
  (apply - (apply (juxt max min) (vals counts))))

(defn solution [steps input]
  (let [{:keys [insertion polymer]} (parse-input input)]
    (checksum (counts insertion polymer steps))))

(def part1 (partial solution 10))
(def part2 (partial solution 40))
