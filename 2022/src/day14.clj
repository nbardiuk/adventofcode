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

(defn first-repeat [xs]
  (loop [[x & xs] xs
         prev nil]
    (if (= x prev)
      prev
      (recur xs x))))

(defn parse-lines [input]
  (for [line (str/split-lines input)]
    (partition 2 (parse-longs line))))

(defn read-obstacles [input]
  (set (for [line (parse-lines input)
             [[ax ay] [bx by]] (partition 2 1 line)
             x (arange ax bx)
             y (arange ay by)]
         [x y])))

(defn drop-sand [floor? obstacles]
  (->> [500 0]
       (iterate
        (fn [sand]
          (->> (neighbours sand)
               (find-first (fn [p] (not (or (floor? p) (obstacles p)))))
               (#(or % sand)))))
       first-repeat))

(defn part1 [input]
  (let [obstacles (read-obstacles input)
        bottom (->> obstacles (map second) (reduce max))
        floor (+ 2 bottom)
        floor? #(= floor (second %))
        filled (->> obstacles
                    (iterate (fn [obstacles]
                               (let [sand (drop-sand floor? obstacles)]
                                 (if (< (second sand) bottom)
                                   (conj obstacles sand)
                                   obstacles))))
                    first-repeat)]

    (- (count filled) (count obstacles))))

(defn part2 [input]
  (let [obstacles (read-obstacles input)
        floor (->> obstacles (map second) (reduce max) (+ 2))
        floor? #(= floor (second %))
        filled (->> obstacles
                    (iterate (fn [obstacles]
                               (conj obstacles (drop-sand floor? obstacles))))
                    first-repeat)]

    (- (count filled) (count obstacles))))
