(ns day13
  (:require [clojure.string :as str]
            [clojure.math.numeric-tower :as math]))

(defn- read-schedule [input]
  (let [[time busses] (str/split-lines input)
        time (read-string time)
        busses (map read-string (re-seq #"\d+" busses))]
    [time busses]))

(defn- read-busses [input]
  (let [[_ busses] (str/split-lines input)]
    (->> (str/split busses #",")
         (keep-indexed (fn [i bus-cycle]
                         (when-not (= "x" bus-cycle)
                           [(read-string bus-cycle) i]))))))

(defn- wait-time [time bus-cycle]
  (mod (- bus-cycle time) bus-cycle))

(defn part1 [input]
  (let [[time busses] (read-schedule input)]
    (->> busses
         (map #(vector (wait-time time %) %))
         (apply min-key first)
         (apply *))))

(defn part2 [input]
  (->> (read-busses input)

       (reduce
        (fn [[step result] [bus-cycle i]]
          (let [shift (mod i bus-cycle)
                result (->> (iterate #(+ step %) result)
                            (filter (fn [time] (= shift (wait-time time bus-cycle))))
                            first)
                step (math/lcm step bus-cycle)]
            [step result])))

       second))
