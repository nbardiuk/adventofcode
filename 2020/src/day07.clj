(ns day07
  (:require [clojure.string :as str]
            [clojure.set :as set]))

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

(defn- transpose [graph]
  (->> (for [[parent children] graph
             [child _] children]
         {child [parent]})
       (reduce (partial merge-with concat) {})))

(defn- count-parents [graph item]
  (let [graph (transpose graph)]
    (loop [[item & queue] [item]
           seen #{}]
      (if-not item
        (count seen)
        (let [parents (filter (comp not seen) (get graph item))]
          (recur (concat queue parents)
                 (set/union seen (set parents))))))))

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
