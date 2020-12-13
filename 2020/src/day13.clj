(ns day13
  (:require [clojure.string :as str]))

(defn- read-busses [input]
  (let [[_ busses] (str/split-lines input)]
    (->> (str/split busses #",")
         (keep-indexed (fn [bus-delay bus-cycle]
                         (when-not (= "x" bus-cycle)
                           [(read-string bus-cycle) bus-delay]))))))

(defn- wait-time [time bus-cycle]
  (mod (- bus-cycle time) bus-cycle))

(defn part1 [input]
  (let [[time & busses] (->> input (re-seq #"\d+") (map read-string))]
    (->> busses
         (map #(vector (wait-time time %) %))
         (apply min-key first)
         (apply *))))

(defn part2 [input]
  (->> (read-busses input)

       (reduce
        (fn [[cycle delay] [bus-cycle bus-delay]]
          (let [delay (->> (iterate #(+ cycle %) delay)
                           (filter #(zero? (wait-time (+ % bus-delay) bus-cycle)))
                           first)
                cycle (* cycle bus-cycle)]
            [cycle delay])))

       second))
