(ns day02
  (:require [clojure.set :as set]))

(def shape->score
  {:rock     1
   :paper    2
   :scissors 3})

(def opponent->shape
  {:A :rock
   :B :paper
   :C :scissors})

(def winner
  {:scissors :rock
   :rock     :paper
   :paper    :scissors})

(def looser
  (set/map-invert winner))

(defn parse-strategy [input]
  (->> input
       (re-seq #"\w")
       (map keyword)
       (partition 2)))

(defn round-score [[opponent mine]]
  (+ (shape->score mine)
     (cond
       (= opponent (winner mine)) 0
       (= opponent mine)          3
       (= opponent (looser mine)) 6)))

(defn decode-round
  [response-decoding [opponent-code response-code]]
  (let [opponent-shape (opponent->shape opponent-code)
        my-shape       (response-decoding response-code opponent-shape)]
    [opponent-shape my-shape]))

(defn game-score [input response-decoding]
  (->> input
       parse-strategy
       (map (partial decode-round response-decoding))
       (map round-score)
       (reduce +)))

(defn constant-response
  [response-code _opponent-shape]
  (case response-code
    :X :rock
    :Y :paper
    :Z :scissors))

(defn dynamic-response
  [response-code opponent-shape]
  (case response-code
    :X (looser opponent-shape)
    :Y opponent-shape
    :Z (winner opponent-shape)))

(defn part1 [input]
  (game-score input constant-response))

(defn part2 [input]
  (game-score input dynamic-response))
