(ns day24
  (:require [clojure.set :as set]
            [clojure.string :as str]))

(defn enumerate [xs]
  (map vector (range) xs))

(defn parse-valley [input]
  (->> (for [[y line] (enumerate (str/split-lines input))
             [x char] (enumerate line)]
         {({\# :wall \. :open \< :left \> :right \v :down \^ :up} char) #{[x y]}})
       (reduce (partial merge-with set/union))))

(defn update-valley [{:as valley :keys [max-x max-y wall]}]
  (let [max-x (or max-x (apply max (map first wall)))
        max-y (or max-y (apply max (map second wall)))]
    (-> valley
        (assoc :max-x max-x)
        (assoc :max-y max-y)
        (update :time  (fnil inc 0))
        (update :right #(set (for [[x y] %] [(inc (mod x (dec max-x))) y])))
        (update :left  #(set (for [[x y] %] [(dec (if (= 1 x) max-x x)) y])))
        (update :up    #(set (for [[x y] %] [x (dec (if (= 1 y) max-y y))])))
        (update :down  #(set (for [[x y] %] [x (inc (mod y (dec max-y)))]))))))

(defn neighbours [[x y]]
  (for [dx [-1 0 1]
        dy [-1 0 1]
        :when (or (= 0 dx) (= 0 dy))]
    [(+ x dx) (+ y dy)]))

(defn traverse [valley start end]
  (loop [valley valley
         positions #{start}]
    (if (positions end)
      valley
      (let [{:as valley :keys [wall max-y right left up down]} (update-valley valley)
            next-position  (comp (mapcat neighbours) (filter (fn [[_ y]] (<= 0 y max-y))))
            next-positions (into #{} next-position positions)]
        (recur valley (set/difference next-positions wall right left up down))))))

(defn part1 [input]
  (let [{:keys [open] :as valley} (parse-valley input)
        start (apply (partial min-key second) open)
        end   (apply (partial max-key second) open)]
    (:time (traverse valley start end))))

(defn part2 [input]
  (let [{:as valley :keys [open]} (parse-valley input)
        start (apply (partial min-key second) open)
        end   (apply (partial max-key second) open)]
    (-> valley
        (traverse start end)
        (traverse end start)
        (traverse start end)
        :time)))
