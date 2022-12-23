(ns day23
  (:require [clojure.string :as str]))

(def directions
  [:n :s :w :e])

(defn enumerate [xs]
  (map vector (range) xs))

(defn parse-elfs [input]
  (set (for [[y line] (enumerate (str/split-lines input))
             [x char] (enumerate line)
             :when (= \# char)]
         [x y])))

(defn neighbours-all [[x y]]
  (for [dx [-1 0 1]
        dy [-1 0 1]
        :when (not= 0 dx dy)]
    [(+ x dx) (+ y dy)]))

(defn neighbours [direction [x y]]
  (case direction
    :n (for [dx [-1 0 1]] [(+ x dx) (- y 1)])
    :s (for [dx [-1 0 1]] [(+ x dx) (+ y 1)])
    :w (for [dy [-1 0 1]] [(- x 1) (+ y dy)])
    :e (for [dy [-1 0 1]] [(+ x 1) (+ y dy)])))

(defn- move [direction [x y]]
  (case direction
    :n [x (- y 1)]
    :s [x (+ y 1)]
    :w [(- x 1) y]
    :e [(+ x 1) y]))

(defn rotations [directions]
  (iterate (fn [d] (conj (subvec d 1) (get d 0))) directions))

(defn propose [elfs directions elf]
  (if (every? (comp not elfs) (neighbours-all elf))
    elf
    (-> (for [direction directions
              :when (every? (comp not elfs) (neighbours direction elf))]
          (move direction elf))
        first
        (or elf))))

(defn index-of-repeat [xs]
  (loop [[x & xs] xs
         i 1]
    (if (= x (first xs))
      i
      (recur xs (inc i)))))

(defn next-step [elfs directions]
  (->> (group-by #(propose elfs directions %) elfs)
       (mapcat (fn [[next elfs]] (if (= 1 (count elfs)) [next] elfs)))
       set))

(defn steps [elfs]
  (reductions next-step elfs (rotations directions)))

(defn area [elfs]
  (let [xs (map first elfs)
        ys (map second elfs)]
    (* (inc (- (apply max xs) (apply min xs)))
       (inc (- (apply max ys) (apply min ys))))))

(defn part1 [input]
  (let [elfs (-> (parse-elfs input)
                 steps
                 (nth 10))]
    (- (area elfs) (count elfs))))

(defn part2 [input]
  (-> (parse-elfs input)
      steps
      index-of-repeat))
