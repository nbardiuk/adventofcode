(ns day08
  (:require
   [clojure.string :as string]
   [clojure.set :as set]))

(def sectors->digit
  {#{\a \b \c \e \f \g}    0
   #{\c \f}                1
   #{\a \c \d \e \g}       2
   #{\a \c \d \f \g}       3
   #{\b \c \d \f}          4
   #{\a \b \d \f \g}       5
   #{\a \b \d \e \f \g}    6
   #{\a \c \f}             7
   #{\a \b \c \d \e \f \g} 8
   #{\a \b \c \d \f \g}    9})

(defn common-sectors-by-count [digits]
  (update-vals (group-by count digits)
               #(apply set/intersection %)))

(def count->correct
  (common-sectors-by-count (keys sectors->digit)))

(def all-sectors (set "abcdefg"))

(def correct->mixed (into {} (map vector all-sectors (repeat all-sectors))))

(defn narrow [correct->mixed correct mixed]
  (let [not-correct (set/difference all-sectors correct)]
    (as-> correct->mixed $
      (reduce #(update %1 %2 set/intersection mixed) $ correct)
      (reduce #(update %1 %2 set/difference mixed) $ not-correct))))

(defn- decode [line]
  (let [digits (re-seq #"\w+" line)
        count->mixed (->> digits (map set) common-sectors-by-count)
        correct->mixed (reduce (fn [correct->mixed [count correct]]
                                 (narrow correct->mixed correct (count->mixed count)))
                               correct->mixed count->correct)
        mixed->correct (-> correct->mixed (update-vals first) set/map-invert)]
    (->> digits
         (map #(map mixed->correct %))
         (map (comp sectors->digit set)))))

(defn part1 [input]
  (->> (string/split-lines input)
       (map decode)
       (mapcat #(take-last 4 %))
       (keep #{1 4 7 8})
       count))

(defn part2 [input]
  (->> (string/split-lines input)
       (map decode)
       (mapcat #(take-last 4 %))
       (map * (cycle [1000 100 10 1]))
       (reduce +)))
