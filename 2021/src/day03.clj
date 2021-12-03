(ns day03
  (:require [clojure.string :as string]))

(defn- parse-long-bin [bits]
  (-> bits string/join (Long/parseLong 2)))

(defn- bit-at [position bits]
  (nth bits position))

(defn- bits-count [numbers]
  (count (first numbers)))

(defn- bit-stats [bits]
  (let [f (frequencies bits)]
    (if (< (get f \1 0) (get f \0 0))
      {:most-common \0 :least-common \1}
      {:most-common \1 :least-common \0})))

(defn- bit-by-criteria [criteria position numbers]
  (->> numbers
       (map #(bit-at position %))
       bit-stats
       criteria))

(defn- gama-epsilon [numbers criteria]
  (->> (range (bits-count numbers))
       (map #(bit-by-criteria criteria % numbers))
       parse-long-bin))

(defn- oxygen-co2 [numbers criteria]
  (->> (range (bits-count numbers))
       (reduce (fn [numbers position]
                 (if (= 1 (count numbers))
                   (reduced numbers)
                   (let [bit (bit-by-criteria criteria position numbers)]
                     (filter #(= bit (bit-at position %)) numbers))))
               numbers)
       parse-long-bin))

(defn- solution [rate input]
  (let [numbers (string/split-lines input)]
    (* (rate numbers :most-common)
       (rate numbers :least-common))))

(def part1 (partial solution gama-epsilon))
(def part2 (partial solution oxygen-co2))
