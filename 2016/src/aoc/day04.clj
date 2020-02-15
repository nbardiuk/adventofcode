(ns aoc.day04
  (:require [clojure.string :as s]))

(defn- read-room [line]
  (let [[_ encrypted-name sector checksum] (re-matches #"([a-z\-]+)-(\d+)\[(\w+)\]" line)]
    {:encrypted-name encrypted-name
     :sector (read-string sector)
     :checksum checksum}))

(defn- sort-by-frequency [letters]
  (->> letters
       frequencies
       (sort-by (fn [[letter freq]] [(- freq) letter]))
       (map first)))

(defn- real? [{:keys [encrypted-name checksum]}]
  (->> encrypted-name
       (filter #(not= \- %))
       sort-by-frequency
       (take 5)
       (apply str)
       (= checksum)))

(defn- real-rooms [input]
  (->> input
       s/split-lines
       (map read-room)
       (filter real?)))

(defn- seq-find [p xs]
  (->> xs (filter p) first))

(defn- rotate [shift letter]
  (let [a (int \a)]
    (-> letter int (- a) (+ shift) (mod 26) (+ a) char)))

(defn- decrypt [shift word]
  (->> word
       (map (partial rotate shift))
       (apply str)))

(defn- room-name [{:keys [encrypted-name sector]}]
  (s/replace encrypted-name #"[a-z]+" (partial decrypt sector)))

(defn- has-north-pole [room]
  (->> room room-name (re-find #"northpole")))

(defn part1 [input]
  (->> input
       real-rooms
       (map :sector)
       (apply +)))

(defn part2 [input]
  (->> input
       real-rooms
       (seq-find has-north-pole)
       :sector))
