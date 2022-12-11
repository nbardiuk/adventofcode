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

(defn parse-notes [input]
  (for [monkey (str/split input #"\R\R")]
    (let [[name items operation test when-true when-false] (str/split-lines monkey)]
      {:id        (first-long name)
       :operation (parse-operation operation)
       :divisor   (first-long test)
       :true-id   (first-long when-true)
       :false-id  (first-long when-false)
       :items     (vec (parse-longs items))})))

(defn evl [[op a b] old]
  (let [resolve #(case % :old old %)]
    (op (resolve a) (resolve b))))

(defn update-item
  [normalize {:keys [operation]} item]
  (normalize (evl operation item)))

(defn destination
  [{:keys [divisor true-id false-id]} item]
  (if (zero? (mod item divisor)) true-id false-id))

(defn throw-item
  [normalize {:keys [id] :as monkey} items]
  (let [item (->> (items id) peek (update-item normalize monkey))
        dest (destination monkey item)]
    (-> items
        (update id pop)
        (update dest conj item))))

(defn monkey-turn
  [normalize {:keys [items stats]} {:keys [id] :as monkey}]
  (let [steps (count (items id))]
    {:items (->> items
                 (iterate #(throw-item normalize monkey %))
                 (#(nth % steps)))
     :stats (update stats id (fnil + 0) steps)}))

(defn round [normalize monkeys state]
  (reduce (partial monkey-turn normalize) state monkeys))

(defn solution
  [normalize iterations monkeys]
  (let [items (->> monkeys (map (juxt :id :items)) (into {}))
        stats (->> {:items items :stats {}}
                   (iterate #(round normalize monkeys %))
                   (#(nth % iterations))
                   :stats)]
    (->> stats vals (sort >) (take 2) (reduce *))))

(defn part1 [input]
  (let [monkeys   (parse-notes input)
        normalize #(long (/ % 3))]
    (solution normalize 20 monkeys)))

(defn part2 [input]
  (let [monkeys   (parse-notes input)
        divisor   (->> monkeys (map :divisor) (reduce *))
        normalize #(mod % divisor)]
    (solution normalize 10000 monkeys)))
