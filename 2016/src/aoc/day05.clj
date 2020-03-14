(ns aoc.day05
  (:import (java.security MessageDigest)
           (java.math BigInteger)))

(def md5-algo (MessageDigest/getInstance "MD5"))

(defn- md5 [^String s]
  (.digest md5-algo (.getBytes s)))

(defn- digests [input]
  (map #(md5 (str input %)) (range)))

(defn- starts-with-5-zeros [digest]
  (and (= (nth digest 0) 0)
       (= (nth digest 1) 0)
       (<= 0 (nth digest 2) 15)))

(defn- sixth-character [digest]
  (format "%x" (nth digest 2)))

(defn part1 [input]
  (->> input
       digests
       (filter starts-with-5-zeros)
       (map sixth-character)
       (take 8)
       (apply str)))
