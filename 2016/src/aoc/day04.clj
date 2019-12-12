(ns aoc.day04
  (:require [clojure.string :as cs]))

(defn read-room [line]
  (let [[_ encoded sector checksum] (re-matches #"([a-z\-]+)-(\d+)\[(\w+)\]" line)]
    [encoded (read-string sector) checksum]))

(defn real? [[encoded _ checksum]]
  (let [counts (frequencies (filter #(not= \- %) encoded))
        sorted (sort-by (fn [[letter cnt]] [(- cnt) letter]) counts)
        code (apply str (take 5 (map first sorted)))]
    (= checksum code)))

(defn decrypt [shift word]
  (let [ds (map (fn [c] (char (+ (int \a) (mod (+ (- (int c) (int \a)) shift) 26)))) word)]
    (apply str ds)))

(defn decrypt-message [[encoded sector _]]
  (let [words (cs/split encoded #"-")
        message (cs/join " " (map (partial decrypt sector) words))]
    [message sector]))

(defn part1 [input]
  (let [lines (cs/split-lines input)
        rooms (map read-room lines)
        real-rooms (filter real? rooms)
        sectors (map second real-rooms)]
    (apply + sectors)))

(defn part2 [input]
  (let [lines (cs/split-lines input)
        rooms (map read-room lines)
        real-rooms (filter real? rooms)
        decrypted (map decrypt-message real-rooms)
        north-pole (filter #(.contains (first  %) "northpole") decrypted)]
    (second (first north-pole))))
