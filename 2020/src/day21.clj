(ns day21
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn- parse-food [input]
  (for [line (str/split-lines input)
        :let [[_ ings alls] (->> line
                                 (re-find #"((?:\w+\s*)*) \(contains (.*)\)")
                                 (map #(re-seq #"\w+" %)))]]
    {:ingredients ings
     :allergens alls}))

(defn- fix-point [f x]
  (->> (iterate f x)
       (partition 2 1)
       (drop-while #(apply not= %))
       ffirst))

(defn- prune-decided [allergens]
  (let [decided (->> allergens vals (filter #(= 1 (count %))) (apply set/union))]
    (->> allergens
         (map (fn [[k v]] [k (if (= 1 (count v)) v (set/difference v decided))]))
         (into {}))))

(defn- allergic-ingredients [food]
  (->> (for [{:keys [ingredients allergens]} food
             allergen allergens]
         {allergen (set ingredients)})
       (reduce (partial merge-with set/intersection))))

(defn part1 [input]
  (let [food (parse-food input)
        allergic (->> food allergic-ingredients vals (apply set/union))]
    (->> food
         (map :ingredients)
         (mapcat #(filter (comp not allergic) %))
         count)))

(defn part2 [input]
  (->> (parse-food input)
       allergic-ingredients
       (fix-point prune-decided)
       (sort-by first)
       (map (comp first second))
       (str/join ",")))
