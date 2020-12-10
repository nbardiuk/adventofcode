(ns day10
  (:require [clojure.string :refer [split-lines]]))

(defn- read-numbers [input]
  (->> (split-lines input)
       (map read-string)))

(defn- with-edges [ns]
  (into ns [0 (+ (apply max ns) 3)]))

(defn connect-all [ns]
  (->> (for [a ns
             b ns
             :when (<= 1 (- b a) 3)]
         {a b})
       (reduce (partial merge-with min) {})))

(defn count-connections [ns]
  (let [[n & rest] (sort ns)
        connections (filter #(<= 1 (- % n) 3) rest)
        subs (map #(filter (partial <= %) rest) connections)]
    (if (seq connections)
      (apply + (map count-connections subs))
      1)))

(defn connected-components [ns]
  (reduce (fn [[[top-element :as top-group] & rest-groups :as groups] n]
            (if (or (nil? top-element) (= 3 (- n top-element)))
              (cons [n] groups)
              (cons (cons n top-group) rest-groups)))
          []
          (sort ns)))

(defn part1 [input]
  (letfn [(difference [[a b]] (- b a))
          (checksum [m] (* (get m 1) (get m 3)))]
    (->> (read-numbers input)
         with-edges
         connect-all
         (map difference)
         frequencies
         checksum)))

(defn part2 [input]
  (->> (read-numbers input)
       with-edges
       connected-components
       (map count-connections)
       (apply *)))
