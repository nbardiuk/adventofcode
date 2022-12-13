(ns day13
  (:require [clojure.edn :as edn]))

(defn parse-packets [input]
  (edn/read-string (str "[" input "]")))

(defn find-first [p xs]
  (first (filter p xs)))

(defn packet-compare [left right]
  (cond

    (and (map? left) (map? right))  0
    (map? left)                    -1
    (map? right)                    1

    (and (number? left) (number? right))
    (compare left right)

    (number? left)
    (packet-compare [left] right)

    (number? right)
    (packet-compare left [right])

    :else
    (if-let [order (->> (map packet-compare left right)
                        (find-first (comp not zero?)))]
      order
      (compare (count left) (count right)))))

(defn part1 [input]
  (->> (parse-packets input)
       (partition 2)
       (keep-indexed (fn [i [left right]]
                       (when (= -1 (packet-compare left right))
                         (inc i))))
       (reduce + 0)))

(defn part2 [input]
  (let [dividers #{[[2]] [[6]]}]
    (->> (parse-packets input)
         (concat dividers)
         (sort packet-compare)
         (keep-indexed (fn [i p]
                         (when (dividers p)
                           (inc i))))
         (reduce * 1))))
