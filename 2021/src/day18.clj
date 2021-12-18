(ns day18
  (:require [clojure.string :as string]
            [clojure.edn :as edn]
            [clojure.java.math :as math]))

(defn parse [input]
  (->> (string/split-lines input)
       (mapv edn/read-string)))

(defn fix-point [f x]
  (let [x' (f x)]
    (if (= x x')
      x
      (recur f x'))))

(defn flat-index [xs]
  (let [branch? (comp not number? first)
        children (fn [[[l r] path]]
                   [[l (conj path :left)]
                    [r (conj path :right)]])]
    (->> (tree-seq branch? children [xs []])
         (remove branch?)
         vec)))

(defn half-up [v]
  (-> v (* 0.5) math/ceil long))

(defn half-down [v]
  (-> v (* 0.5) math/floor long))

(defn explode [xs]
  (let [[[i a path]
         [_ b]] (keep-indexed (fn [i [v path]] (when (= 5 (count path)) [i v path])) xs)]
    (if (not i)
      xs
      (->> [(cond-> (subvec xs 0 i) (< 0 i) (update-in [(dec i) 0] + a))
            [[0 (pop path)]]
            (cond-> (subvec xs (+ 2 i)) (< (+ 2 i) (count xs)) (update-in [0 0] + b))]
           (reduce into)))))

(defn split [xs]
  (let [[[i v path]] (keep-indexed (fn [i [v path]] (when (<= 10 v) [i v path])) xs)]
    (if (not i)
      xs
      (->> [(subvec xs 0 i)
            [[(half-down v) (conj path :left)]
             [(half-up v) (conj path :right)]]
            (subvec xs (inc i))]
           (reduce into)))))

(defn nest [vs direction]
  (vec (for [[v path] vs] [v (into [direction] path)])))

(defn add [a b]
  (->> (into (nest a :left) (nest b :right))
       (fix-point (comp split #(fix-point explode %)))))

(defn magnitude [vs]
  (->> vs
       (map (fn [[v path]] (->> path (map {:left 3 :right 2}) (reduce * v))))
       (reduce +)))

(defn part1 [input]
  (->> input
       parse
       (map flat-index)
       (reduce add)
       magnitude))

(defn part2 [input]
  (let [vs (->> input parse (map flat-index))]
    (->> (for [a vs b vs :when (not= a b)] (add a b))
         (map magnitude)
         (reduce max))))
