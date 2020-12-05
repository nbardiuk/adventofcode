(ns day05
  (:require [clojure.string :refer [replace split-lines]]))

(defn- seat-id [seat]
  (-> seat
      (replace #"." {"R" "1" "B" "1"
                     "L" "0" "F" "0"})
      (Integer/parseInt 2)))

(defn- ids [input]
  (->> input split-lines (map seat-id)))

(defn part1 [input]
  (->> input ids (apply max)))

(defn part2 [input]
  (let  [ids (->> input ids set)]
    (->> (iterate inc (apply min ids))
         (drop-while ids)
         first)))
