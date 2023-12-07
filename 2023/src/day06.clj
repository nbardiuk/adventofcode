(ns day06
  (:require
    [clojure.string :as str]
    [clojure.math :as math]))


;; x^2 - time*x + distance < 0


(defn roots [a b c]
  (let [d (math/sqrt (- (math/pow b 2) (* 4 a c)))]
    (sort
      [(/ (- b d) (* 2 a))
       (/ (+ b d) (* 2 a))])))


(defn count-longs
  [a b]
  (let [a (math/floor (+ a 1))
        b (math/ceil (- b 1))]
    (long (inc (- b a)))))


(defn part1 [input]
  (let [[times distances] (str/split-lines input)
        times (mapv parse-long (re-seq #"\d+" times))
        distances (mapv parse-long (re-seq #"\d+" distances))]
    (->> (mapv (fn [time distance]
                 (apply count-longs (roots 1 time distance)))
               times distances)
         (reduce *))))


(defn part2 [input]
  (let [[times distances] (str/split-lines input)
        time (parse-long (str/join (re-seq #"\d" times)))
        distance (parse-long (str/join (re-seq #"\d+" distances)))]
    (apply count-longs (roots 1 (- time) distance))))
