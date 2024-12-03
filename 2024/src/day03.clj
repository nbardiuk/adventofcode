(ns day03)

(defn- calc [exprs]
  (->> exprs
       (map (fn [[_op a b]]
              (* (parse-long a) (parse-long b))))
       (reduce +)))

(defn- keep-do [exprs]
  (->> exprs
       (reduce
         (fn [{:keys [do?] :as state} [op :as expr]]
           (cond
             (= "do()" op) (assoc state :do? true)
             (= "don't()" op) (assoc state :do? false)
             do? (update state :keep conj expr)
             :else state))
         {:keep [] :do? true})
       :keep))

(defn part1 [input]
  (->> input
       (re-seq #"mul\((\d+),(\d+)\)")
       calc))

(defn part2 [input]
  (->> input
       (re-seq #"mul\((\d+),(\d+)\)|do\(\)|don't\(\)")
       keep-do
       calc))
