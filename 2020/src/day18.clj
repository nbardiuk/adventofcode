(ns day18
  (:require [clojure.string :as str]))

(defn parse-tokens [input]
  (->> input
       (re-seq #"\d+|[()+*]")
       (map #(case %
               "*" :mul
               "+" :add
               "(" :open
               ")" :close
               (read-string %)))))

(defn eval1 [tokens]
  (letfn [(single [[token & tokens]]
            (case token
              :open (let [[expr tokens] (binary tokens)]
                      [expr (rest tokens)])
              [token tokens]))

          (binary [tokens]
            (let [[a [op & tokens] :as left] (single tokens)
                  [b tokens] (single tokens)]
              (case op
                :add (binary (cons (+ a b) tokens))
                :mul (binary (cons (* a b) tokens))
                left)))]

    (first (binary tokens))))

(defn eval2 [tokens]
  (letfn [(single [[token & tokens]]
            (case token
              :open (let [[expr tokens] (product tokens)]
                      [expr (rest tokens)])
              [token tokens]))

          (product [tokens]
            (let [[a [op & tokens] :as left] (sum tokens)]
              (case op
                :mul (let [[b tokens] (product tokens)]
                       [(* a b) tokens])
                left)))

          (sum [tokens]
            (let [[a [op & tokens] :as left] (single tokens)]
              (case op
                :add (let [[b tokens] (sum tokens)]
                       [(+ a b) tokens])
                left)))]

    (first (product tokens))))

(defn solution [evaluation input]
  (->> (str/split-lines input)
       (map parse-tokens)
       (map evaluation)
       (reduce +)))

(defn part1 [input] (solution eval1 input))
(defn part2 [input] (solution eval2 input))
