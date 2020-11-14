(ns aoc.day05
  (:import (java.security MessageDigest)
           (java.math BigInteger)))

(defn- md5 [^String s]
  (.digest (MessageDigest/getInstance "MD5") (.getBytes s)))

(defn- digests [input]
  (map #(md5 (str input %)) (range)))

(defn- starts-with-5-zeros [digest]
  (and (= (nth digest 0) 0)
       (= (nth digest 1) 0)
       (<= 0 (nth digest 2) 15)))

(defn- starts-with-valid-index [digest]
  (and (= (nth digest 0) 0)
       (= (nth digest 1) 0)
       (<= 0 (nth digest 2) 7)))

(defn- sixth-character [digest]
  (format "%x" (nth digest 2)))

(defn- seventh-character [digest]
  (format "%x" (bit-and (bit-shift-right (nth digest 3) 4) 0x0F)))

(defn part1 [input]
  (->> input
       digests
       (filter starts-with-5-zeros)
       (map sixth-character)
       (take 8)
       (apply str)))

(defn- collect-chars [result digest]
  (let [i (int (nth digest 2))
        v (seventh-character digest)
        result (if (get result i) result (assoc result i v))]
    (if (not-any? nil? result)
      (reduced result)
      result)))

(defn part2 [input]
  (->> input
       digests
       (filter starts-with-valid-index)
       (reduce collect-chars (into [] (repeat 8 nil)))
       (apply str)))
