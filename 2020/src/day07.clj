(ns day07
  (:require [clojure.string :as str]))

(defn- parse-bags [line]
  (->> (for [bag (-> line
                     (str/replace #" bags?\.?|no other" "")
                     (str/split #", "))
             :let [[quantity bag] (str/split bag #" " 2)]
             :when (seq quantity)]
         [bag (read-string quantity)])
       (into {})))

(defn- parse-graph [input]
  (->> (for [line (str/split-lines input)
             :let [[parent bags] (str/split line #" bags contain ")]]
         [parent (parse-bags bags)])
       (into {})))

(defn- parents [graph item]
  (->> (for [parent (->> graph
                         (filter #(-> % second (contains? item)))
                         (map first))]
         (cons parent (parents graph parent)))
       (apply concat)))

(defn- count-parents [graph item]
  (->> item (parents graph) distinct count))

(defn- count-children [graph parent]
  (->> (for [[child n] (get graph parent)]
         (* n (+ 1 (count-children graph child))))
       (reduce +)))

(defn part1 [input]
  (-> (parse-graph input)
      (count-parents "shiny gold")))

(defn part2 [input]
  (-> (parse-graph input)
      (count-children "shiny gold")))
