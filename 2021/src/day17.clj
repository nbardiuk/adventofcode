(ns day17
  (:require [clojure.java.math :as math]))

(defn parse-input [input]
  (->> (re-seq #"-?\d+" input)
       (map parse-long)
       (map vector [:min-x :max-x :min-y :max-y])
       (into {})))

(defn x-pos [vx t]
  (apply + (take t (iterate #(max 0 (dec %)) vx))))

(defn y-pos [vy t]
  (* 1/2 t (- (* 2 vy) (dec t))))

(defn min-v [pos]
  ;; (= pos (* 1/2 v (+ v 1))
  (-> (* 1/2 (dec (math/sqrt (inc (* 8 pos)))))
      math/ceil
      long))

(defn part1 [input]
  (let [{:keys [min-y]} (parse-input input)
        y (math/abs min-y)]
    (* 1/2 y (dec y))))

(defn part2 [input]
  (let [{:keys [min-x max-x min-y max-y]} (parse-input input)
        min-vx (min-v min-x)]
    (->> (for [vy (range min-y (math/abs min-y))
               t (->> (range) (filter #(<= (y-pos vy %) max-y)) (take-while #(<= min-y (y-pos vy %))))
               vx (range min-vx (inc max-x))
               :when (<= min-x (x-pos vx t) max-x)]
           [vx vy])
         set
         count)))

