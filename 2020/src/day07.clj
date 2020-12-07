(ns day07
  (:require [clojure.string :as str]))

(defn- parse-bags [bags]
  (->> (re-seq #"(\d+) (\w+ \w+)" bags)
       (keep (comp #(update % 1 read-string) vec reverse rest))
       (into {})))

(defn- parse-graph [input]
  (->> (str/split-lines input)
       (map #(str/split % #" bags contain "))
       (map #(update % 1 parse-bags))
       (into {})))

(defn- transpose [graph]
  (->> (for [[parent children] graph
             [child _] children]
         {child {parent 1}})
       (reduce (partial merge-with merge) {})))

(defn- sum-counts [graph aggr item]
  (loop [[[item n] & queue] [[item 1]]
         counts {}]
    (if-not item
      (->> counts vals (apply +))
      (let [children (map #(update % 1 * n) (get graph item))
            queue (concat queue children)
            counts (merge-with aggr counts (into {} children))]
        (recur queue counts)))))

(defn part1 [input]
  (-> (parse-graph input)
      transpose
      (sum-counts (constantly 1) "shiny gold")))

(defn part2 [input]
  (-> (parse-graph input)
      (sum-counts + "shiny gold")))
