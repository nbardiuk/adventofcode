(ns day02
  (:require
    [clojure.string :as str]))


(defn parse-games [input]
  (for [line (str/split-lines input)]
    (let [[[_ game]] (re-seq #"Game (\d+)" line)]
      {:game (parse-long game)
       :draws (vec (for [draw (rest (re-seq #"[^:;]+" line))]
                (->> (re-seq #"(\d+) (\w+)" draw)
                     (map (fn [[_ n c]] [(keyword c) (parse-long n)]))
                     (into {}))))})))


(defn part1 [input]
  (->> (for [{:keys [game draws]} (parse-games input)
             :let [mx #(reduce max 0 (keep % draws))]
             :when (and (<= (mx :red) 12)
                     (<= (mx :green) 13)
                     (<= (mx :blue) 14))]
         game)
    (reduce +)))


(defn part2 [input]
  (->> (for [{:keys [draws]} (parse-games input)
             :let [mx #(reduce max 0 (keep % draws))]]
         (* (mx :red)
           (mx :green)
           (mx :blue)))
    (reduce +)))
