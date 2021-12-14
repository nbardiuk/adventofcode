(ns day14)

(defn parse-input [input]
  {:polymer (re-find #"^\w+" input)
   :rules (into {} (for [[_ pair ins] (re-seq #"(\w+) -> (\w)" input)]
                     [(vec pair) (vec ins)]))})

(def counts
  (memoize
   (fn [rules polymer steps]
     (letfn [(expand [[a b :as pair]]
               (let [i (rules pair)]
                 [(into [a] i) (into i [b])]))
             (into-counts [result polymer]
               (merge-with
                +
                (update result (first polymer) (fnil dec 1))
                (counts rules polymer (dec steps))))]

       (if (zero? steps)
         (frequencies polymer)
         (->> (partition 2 1 polymer)
              (mapcat expand)
              (reduce into-counts {})))))))

(defn checksum [counts]
  (apply - (apply (juxt max min) (vals counts))))

(defn solution [steps input]
  (let [{:keys [rules polymer]} (parse-input input)]
    (checksum (counts rules polymer steps))))

(def part1 (partial solution 10))
(def part2 (partial solution 40))
