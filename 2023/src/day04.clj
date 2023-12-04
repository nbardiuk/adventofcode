(ns day04
  (:require
    [clojure.math :as math]
    [clojure.set :as set]
    [clojure.string :as str]))


(defn- parse-cards [input]
  (for [line (str/split-lines input)]
    (let [[card winning have] (str/split line #"[:|]")]
      {:card (->> card (re-find #"\d+") parse-long)
       :winning (->> winning (re-seq #"\d+") (mapv parse-long))
       :have (->> have (re-seq #"\d+") (mapv parse-long))})))


(defn part1 [input]
  (->> (parse-cards input)
       (mapv (fn [{:keys [winning have]}]
               (count (set/intersection (set winning) (set have)))))
       (mapv #(if (zero? %) 0 (long (math/pow 2 (dec %)))))
       (reduce +)))


(defn part2 [input]
  (let [cards (parse-cards input)
        counts (into {} (map #(hash-map (:card %) 1)) cards)]
    (->> cards
         (mapv
           (fn [{:keys [winning have] :as card}]
             (let [score (count (set/intersection (set winning) (set have)))]
               (assoc card :score score))))
         (reduce
           (fn [counts {:keys [score card]}]
             (let [next-cards (range (inc card) (+ score (inc card)))
                   next-cards-counts (into {}
                                           (map #(hash-map % (counts card)))
                                           next-cards)]
               (merge-with + counts next-cards-counts)))
           counts)
         vals
         (reduce +))))
