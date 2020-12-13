(ns day13
  (:require [clojure.string :as str]
            [clojure.math.numeric-tower :as math]))

(defn part1 [input]
  (let [[time busses] (str/split-lines input)
        time (read-string time)
        busses (map read-string (re-seq #"\d+" busses))]
    (->> busses
         (map #(vector (- % (rem time %)) %))
         (apply min-key first)
         (apply *))))

(defn part2 [input]
  (let [[_ busses] (str/split-lines input)]
    (->> (str/split busses #",")

         (keep-indexed
          (fn [i bus]
            (when-not (= "x" bus)
              [(read-string bus) i])))

         (reduce
          (fn [[result multiple] [bus i]]
            [(->> (iterate #(+ multiple %) result)
                  (some
                   (fn [time]
                     (when (= (rem i bus)
                              (rem (- bus (rem time bus)) bus))
                       time))))
             (math/lcm multiple bus)])
          [0 1])

         first)))
