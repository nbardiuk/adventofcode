(ns day08
  (:require
    [clojure.string :as str]))


(defn gcd [a b]
  (.longValue (.gcd (BigInteger/valueOf a) (BigInteger/valueOf b))))


(defn lcm [a b]
  (/ (* (abs a) (abs b))
     (gcd a b)))


(defn- path-length
  [graph commands end-node? start-node]
  (->> (cycle commands)
       (reductions (fn [node command]
                     (let [next-node (graph [node command])
                           mark (if (end-node? next-node) reduced identity)]
                       (mark next-node)))
         start-node)
       count
       dec))


(defn solution [input start-node? end-node?]
  (let [[commands graph] (str/split input #"\R\R")
        graph (->> (for [line (str/split-lines graph)
                         :let [[from left right] (re-seq #"\w+" line)]]
                     {[from \L] left
                      [from \R] right})
                   (reduce merge))]
    (->> (mapv ffirst graph)
         (filterv start-node?)
         (mapv #(path-length graph commands end-node? %))
         (reduce lcm))))


(defn part1 [input]
  (solution input #{"AAA"} #{"ZZZ"}))


(defn part2 [input]
  (solution input #(str/ends-with? % "A") #(str/ends-with? % "Z")))
