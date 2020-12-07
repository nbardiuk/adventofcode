(ns day07
  (:require [clojure.string :as str]))

(defn- parse-parents [input]
  (->> (str/split-lines input)
       (map #(str/split % #" bags contain "))
       (mapcat (fn [[parent bags]]
                 (->> (re-seq #"\d+ (\w+ \w+)" bags)
                      (keep (comp #(vector parent %) first rest)))))
       (reduce (fn [m [parent child]] (update m child conj parent)) {})))

(defn- parse-counts [input]
  (->> (str/split-lines input)
       (map #(str/split % #" bags contain "))
       (map (fn [[parent bags]]
              [parent (->> (re-seq #"(\d+) (\w+ \w+)" bags)
                           (keep (comp vec reverse rest))
                           (into {}))]))
       (into {})))

(defn- count-parents [graph item]
  (letfn [(all-parents [item]
            (let [parents (get graph item)]
              (concat parents (mapcat all-parents parents))))]
    (->> item all-parents distinct count)))

(defn- sum-children [graph parent]
  (->> (get graph parent)
       (map (fn [[parent n]]
              (* (read-string n) (+ 1 (sum-children graph parent)))))
       (apply +)))

(defn part1 [input]
  (-> (parse-parents input)
      (count-parents "shiny gold")))

(defn part2 [input]
  (-> (parse-counts input)
      (sum-children "shiny gold")))
