(ns day05
  (:require
    [clojure.string :as str]))

(defn- read-rules+updates [input]
  (let [[rules updates] (str/split input #"\R\R")
        rules (for [line (str/split-lines rules)]
                (mapv parse-long (re-seq #"\d+" line)))
        updates (for [line (str/split-lines updates)]
                  (mapv parse-long (re-seq #"\d+" line)))]
    [rules updates]))

(defn- topological-order [edges]
  (loop [order []
         edges edges
         nodes (into #{} (mapcat identity) edges)]
    (let [larger-nodes (into #{} (map second) edges)
          smallest (first (remove larger-nodes nodes))]
      (if smallest
        (recur (conj order smallest)
               (remove (comp #{smallest} first) edges)
               (remove #{smallest} nodes))
        order))))

(defn- mid-indx [xs]
  (/ (dec (count xs)) 2))

(defn- ordered? [xs]
  (= xs (sort xs)))

(defn same-order? [as bs]
  (let [indexes (into {} (map vector as (range)))]
    (ordered? (keep indexes bs))))

(defn- matching-rules [pages rules]
  (let [pages (into #{} pages)]
    (filter #(every? pages %) rules)))

(defn part1 [input]
  (let [[rules updates] (read-rules+updates input)]
    (->> (for [pages updates
               :let [order (topological-order (matching-rules pages rules))]
               :when (same-order? pages order)]
           (get pages (mid-indx pages)))
         (reduce +))))


(defn part2 [input]
  (let [[rules updates] (read-rules+updates input)]
    (->> (for [pages updates
               :let [order (topological-order (matching-rules pages rules))]
               :when (not (same-order? pages order))]
           (get order (mid-indx pages)))
         (reduce +))))
