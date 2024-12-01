(ns day01
  (:require
    [clojure.edn :as edn]))

(defn- read-columns [input]
  (let [pairs (partition 2 (edn/read-string (str "[" input "]")))]
    [(mapv first pairs)
     (mapv second pairs)]))

(defn part1 [input]
  (let [[a b] (read-columns input)]
    (->> (map (comp abs -)
              (sort a) (sort b))
         (reduce +))))

(defn part2 [input]
  (let [[a b] (read-columns input)
        fr (frequencies b)]
    (->> a
         (map (fn [i] (* i (fr i 0))))
         (reduce +))))
