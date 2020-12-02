(ns day02
  (:require [clojure.string :refer [split-lines]]))

(defn- parse [line]
  (let [[[_ a b c p]] (re-seq #"(\d+)-(\d+) (\w)\: (.*)" line)]
    {:a (Integer/parseInt a)
     :b (Integer/parseInt b)
     :letter (first c)
     :password p}))

(defn- valid-range? [{:keys [a b letter password]}]
  (<= a (count (filter #{letter} password)) b))

(defn- valid-position? [{:keys [a b letter password]}]
  (let [at  #(get password (dec %))
        xor not=]
    (xor (= (at a) letter)
         (= (at b) letter))))

(defn- count-passwords [valid? input]
  (->> input
       split-lines
       (map parse)
       (filter valid?)
       count))

(def part1 #(count-passwords valid-range? %))
(def part2 #(count-passwords valid-position? %))
