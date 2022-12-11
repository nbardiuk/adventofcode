(ns day11
  (:require [clojure.string :as str]))

(defn parse-operation [operation]
  (let [[_ a op b] (re-find #"= (\S+) (\S) (\S+)" operation)]
    [(case op "*" * "+" +)
     (or (parse-long a) :old)
     (or (parse-long b) :old)]))

(defn parse-longs [input]
  (->> (re-seq #"\d+" input)
       (map parse-long)))

(defn first-long [input]
  (->> input parse-longs first))

(defn parse-input [input]
  (for [monkey (str/split input #"\R\R")]
    (let [[name items operation test when-true when-false] (str/split-lines monkey)]
      {:id        (first-long name)
       :operation (parse-operation operation)
       :divisor   (first-long test)
       :true-id   (first-long when-true)
       :false-id  (first-long when-false)
       :items     (vec (parse-longs items))})))

(defn app [[op a b] item]
  (let [evl #(case % :old item %)]
    (op (evl a) (evl b))))

(defn throw-item
  [worry-drop common-divisor
   {:keys [operation divisor true-id false-id]}
   items item]
  (let [item (-> (app operation item)
                 (/ worry-drop)
                 long
                 (mod common-divisor))
        dest (if (zero? (mod item divisor))
               true-id
               false-id)]
    (update items dest conj item)))

(defn monkey-turn [worry-drop common-divisor {:keys [items stats]} {:keys [id] :as monkey}]
  (let [monkey-items (items id)
        items        (assoc items id [])]
    {:items (reduce (partial throw-item worry-drop common-divisor monkey) items monkey-items)
     :stats (update stats id (fnil + 0) (count monkey-items))}))

(defn round [worry-drop common-divisor monkeys state]
  (reduce (partial monkey-turn worry-drop common-divisor) state monkeys))

(defn monkey-business [worry-drop n input]
  (let [monkeys        (parse-input input)
        items          (->> monkeys (map (juxt :id :items)) (into {}))
        common-divisor (->> monkeys (map :divisor) (reduce *))
        {:keys [stats]} (->> {:items items :stats {}}
                             (iterate #(round worry-drop common-divisor monkeys %))
                             (#(nth % n)))]
    (->> stats vals (sort >) (take 2) (reduce *))))

(defn part1 [input]
  (monkey-business 3 20 input))

(defn part2 [input]
  (monkey-business 1 10000 input))
