(ns day01
  (:require
    [clojure.edn :as edn]
    [clojure.string :as str]))


(def name->digit
  {"one" 1
   "two" 2
   "three" 3
   "four" 4
   "five" 5
   "six" 6
   "seven" 7
   "eight" 8
   "nine" 9})


(defn part1 [input]
  (apply +
         (for [line (str/split-lines input)]
           (let [digits (re-seq #"\d" line)
                 number (str (first digits) (last digits))]
             (edn/read-string number)))))


(defn part2 [input]
  (apply +
         (for [line (str/split-lines input)]
           (let [digits (->> line
                             (re-seq #"(?=(\d|one|two|three|four|five|six|seven|eight|nine))")
                             (mapv (fn [[_ digit]] (name->digit digit digit))))
                 number (str (first digits) (peek digits))]
             (edn/read-string number)))))
