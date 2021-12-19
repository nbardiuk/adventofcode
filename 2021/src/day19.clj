(ns day19
  (:require [clojure.string :as string]
            [clojure.set :as set]
            [fastmath.core :as math]
            [fastmath.vector :as vector]
            [day00 :refer [tails]]))

(defn parse [input]
  (vec (for [scanner (string/split input #"\n\n")
             :let [nums (->> scanner (re-seq #"-?\d+") (map parse-long))]]
         (->> nums (drop 1) (partition 3) (mapv vector/seq->vec3) set))))

(def rotations
  (for [o [[1 2 3]
           [-2 1 3]
           [-1 -2 3]
           [2 -1 3]
           [-1 2 -3]
           [2 1 -3]
           [1 -2 -3]
           [-2 -1 -3]
           [-3 2 1]
           [-3 1 -2]
           [-3 -2 -1]
           [-3 -1 2]
           [3 2 -1]
           [3 1 2]
           [3 -2 1]
           [3 -1 -2]
           [1 -3 2]
           [-2 -3 1]
           [-1 -3 -2]
           [2 -3 -1]
           [1 3 -2]
           [-2 3 -1]
           [-1 3 2]
           [2 3 1]]]
    [(vector/seq->vec3 (map (comp dec math/abs) o))
     (vector/seq->vec3 (map #(if (pos? %) 1 -1) o))]))

(defn rotate-one [point [indexes cs]]
  (-> point
      (vector/permute indexes)
      (vector/emult cs)))

(defn rotate-all [points rotation]
  (for [point points] (rotate-one point rotation)))

(defn merge-scans [[beacons & scans]]
  (loop [cloud beacons
         [beacons & scans] scans
         scanners [(vector/vec3 0 0 0)]]
    (if (nil? beacons)
      [scanners cloud]
      (let [[[scanner match]]
            (for [rotation rotations
                  :let [beacons (rotate-all beacons rotation)]
                  b (drop 11 beacons)
                  a (drop 11 cloud)
                  :let [scanner (vector/sub a b)
                        beacons (set (map #(vector/add % scanner) beacons))]
                  :when (<= 12 (count (set/intersection cloud beacons)))]
              [scanner beacons])]
        (if scanner
          (recur (into cloud match) scans (conj scanners scanner))
          (recur cloud (concat scans [beacons]) scanners))))))

(defn part1 [input]
  (let [[_ cloud] (merge-scans (parse input))]
    (count cloud)))

(defn part2 [input]
  (let [[scanners] (merge-scans (parse input))]
    (apply max (for [[a & bs] (tails scanners)
                     b bs]
                 (vector/dist-abs a b)))))
