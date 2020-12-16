(ns day16
  (:require [clojure.string :as str]))

(defn- read-numbers [input]
  (->> (re-seq #"\d+" input)
       (map read-string)))

(defn- read-tickets [input]
  (let [[rules my-ticket nearby] (str/split input #"\R\R")]
    {:rules (->> (str/split-lines rules)
                 (map #(str/split % #":"))
                 (map (fn [[field ranges]] [field (read-numbers ranges)]))
                 (into {}))
     :my-ticket (read-numbers my-ticket)
     :nearby (->> (str/split-lines nearby)
                  next
                  (map #(read-numbers %)))}))

(defn- matches-rule [[_ [a b c d]] value]
  (or (<= a value b) (<= c value d)))

(defn- matches-rules [rules value]
  (some #(matches-rule % value) rules))

(defn- keep-matching-rules [rules value]
  (->> rules
       (filter #(matches-rule % value))
       (into {})))

(defn- valid-tickets [rules tickets]
  (filter #(every? (partial matches-rules rules) %) tickets))

(defn- fix-point [f x]
  (->> (iterate f x)
       (partition 2 1)
       (drop-while #(apply not= %))
       ffirst))

(defn- drop-decided [candidates]
  (let [decided? #(= 1 (count %))
        decided-fields (->> candidates (filter decided?) (map keys) flatten)]
    (for [candidate candidates]
      (if (decided? candidate)
        candidate
        (apply dissoc candidate decided-fields)))))

(defn- narrow-candidates [candidates ticket]
  (->> (map keep-matching-rules candidates ticket)
       (fix-point drop-decided)))

(defn- resolve-fields [{:keys [rules nearby]}]
  (let [candidates (repeat (count rules) rules)]
    (->> (valid-tickets rules nearby)
         (reduce narrow-candidates candidates)
         (map (comp first keys)))))

(defn part1 [input]
  (let [{:keys [rules nearby]} (read-tickets input)]
    (->> (flatten nearby)
         (remove (partial matches-rules rules))
         (reduce +))))

(defn part2 [input]
  (let [data (read-tickets input)
        ticket (zipmap (resolve-fields data) (:my-ticket data))]
    (->> ticket
         (keep (fn [[field v]] (when (str/starts-with? field "departure") v)))
         (reduce *))))
