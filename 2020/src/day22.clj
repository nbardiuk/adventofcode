(ns day22
  (:require [clojure.string :as str]
            [clojure.edn :as edn]))

(defn- queue [xs]
  (reduce conj clojure.lang.PersistentQueue/EMPTY xs))

(defn- parse-players [input]
  (for [player (str/split input #"\R\R")]
    (drop 1 (map edn/read-string (re-seq #"\d+" player)))))

(defn simple-round [a b]
  (if (< b a)
    [[a b] []]
    [[] [b a]]))

(defn simple-game [p1 p2]
  (loop [p1 (queue p1)
         p2 (queue p2)]
    (cond
      (or (empty? p1) (empty? p2)) [p1 p2]
      :else (let [[a p1] [(peek p1) (pop p1)]
                  [b p2] [(peek p2) (pop p2)]
                  [w1 w2] (simple-round a b)]
              (recur (reduce conj p1 w1)
                     (reduce conj p2 w2))))))

(defn recursive-game [p1 p2]
  (letfn [(sub-game-round [a p1 b p2]
            (let [[p1 _p2] (recursive-game (take a p1) (take b p2))]
              (if (seq p1)
                [[a b] []]
                [[] [b a]])))]

    (loop [seen1 #{}
           seen2 #{}
           p1 (queue p1)
           p2 (queue p2)]
      (cond
        (or (seen1 p1) (seen2 p2)) [p1 nil]
        (or (empty? p1) (empty? p2)) [p1 p2]
        :else (let [seen1 (conj seen1 p1)
                    seen2 (conj seen2 p2)
                    [a p1] [(peek p1) (pop p1)]
                    [b p2] [(peek p2) (pop p2)]
                    [w1 w2] (if (or (< (count p1) a)
                                    (< (count p2) b))
                              (simple-round a b)
                              (sub-game-round a p1 b p2))]
                (recur seen1 seen2
                       (reduce conj p1 w1)
                       (reduce conj p2 w2)))))))

(defn score [player]
  (->> (map * (reverse player) (iterate inc 1))
       (reduce +)))

(defn solution [input game]
  (->> (parse-players input)
       (apply game)
       (map score)
       (reduce +)))

(defn part1 [input] (solution input simple-game))
(defn part2 [input] (solution input recursive-game))
