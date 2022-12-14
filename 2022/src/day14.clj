(ns day14
  (:require [clojure.string :as str]))

(defn- parse-longs [input]
  (->> input (re-seq #"\d+") (map parse-long)))

(defn arange [a b]
  (if (<= a b)
    (range a (inc b))
    (range a (dec b) -1)))

(defn find-first [p xs]
  (first (filter p xs)))

(defn neighbours [[x y]]
  (for [dx [0 -1 1]]
    [(+ x dx) (+ y 1)]))

(defn first-repeat [[x & xs]]
  (if (= x (first xs))
    x
    (recur xs)))

(defn parse-lines [input]
  (for [line (str/split-lines input)]
    (partition 2 (parse-longs line))))

(defn read-obstacles [input]
  (set (for [line (parse-lines input)
             [[ax ay] [bx by]] (partition 2 1 line)
             x (arange ax bx)
             y (arange ay by)]
         [x y])))

(defn shift-grain [obstacle? sand]
  (->> (neighbours sand)
       (find-first (comp not obstacle?))
       (#(or % sand))))

(defn drop-grain [obstacle?]
  (->> [500 0]
       (iterate #(shift-grain obstacle? %))
       first-repeat))

(defn part1 [input]
  (let [obstacles (read-obstacles input)
        bottom (->> obstacles (map second) (reduce max))
        floor? (fn [[_x y]] (= (+ 2 bottom) y))
        grains (->> #{}
                    (iterate (fn [grains]
                               (let [obstacle? (some-fn grains obstacles floor?)
                                     [_x y :as grain] (drop-grain obstacle?)]
                                 (if (< bottom y)
                                   grains
                                   (conj grains grain)))))
                    first-repeat)]
    (count grains)))

(defn part2 [input]
  (let [obstacles (read-obstacles input)
        bottom (->> obstacles (map second) (reduce max))
        floor? (fn [[_x y]] (= (+ 2 bottom) y))
        grains (->> #{}
                    (iterate (fn [grains]
                               (let [obstacle? (some-fn grains obstacles floor?)]
                                 (conj grains (drop-grain obstacle?)))))
                    first-repeat)]
    (count grains)))
