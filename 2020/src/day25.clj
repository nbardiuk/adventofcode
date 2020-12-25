(ns day25
  (:require [clojure.edn :as edn]))

(defn transform [subject-number value]
  (-> (* value subject-number)
      (rem 20201227)))

(defn loop-size [public-key]
  (loop [value 1
         n 0]
    (cond
      (= value public-key) n
      :else (recur (transform 7 value) (inc n)))))

(defn ecryption-key [public-key loop-size]
  (-> (iterate (partial transform public-key) 1)
      (nth loop-size)))

(defn part1 [input]
  (let [[pa pb] (->> (re-seq #"\d+" input) (map edn/read-string))]
    (ecryption-key pa (loop-size pb))))
