(ns day25
  (:require [clojure.string :as str]))

(def special->digit
  {\0  0
   \1  1
   \2  2
   \- -1
   \= -2})

(defn snafu->decimal [snafu]
  (->> (reverse snafu)
       (map-indexed (fn [i s] (* (long (Math/pow 5 i)) (special->digit s))))
       (reduce + 0)))

(def digit->special+carry
  [[\0 0]
   [\1 0]
   [\2 0]
   [\= 1]
   [\- 1]])

(defn decimal->snafu [n]
  (loop [n n
         snafu ""]
    (let [d (mod n 5)
          n (quot n 5)
          [s carry] (digit->special+carry d)
          snafu (str s snafu)
          n (+ carry n)]
      (if (zero? n)
        snafu
        (recur n snafu)))))

(defn part1 [input]
  (->> (str/split-lines input)
       (map snafu->decimal)
       (reduce + 0)
       decimal->snafu))
