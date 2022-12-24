(ns day22
  (:require [clojure.string :as str]))

(def size 50)

(def a [1 0])
(def b [2 0])
(def c [1 1])
(def d [0 2])
(def e [1 2])
(def f [0 3])

(def west [-1 0])
(def east [1 0])
(def south [0 1])
(def north [0 -1])

(def score
  {east  0
   south 1
   west  2
   north 3})

(defn max-x [grid row]
  (->> (keys grid)
       (keep (fn [[x y]] (when (= y row) x)))
       (apply max)))

(defn min-x [grid row]
  (->> (keys grid)
       (keep (fn [[x y]] (when (= y row) x)))
       (apply min)))

(defn max-y [grid col]
  (->> (keys grid)
       (keep (fn [[x y]] (when (= x col) y)))
       (apply max)))

(defn min-y [grid col]
  (->> (keys grid)
       (keep (fn [[x y]] (when (= x col) y)))
       (apply min)))

(defn step-torus [grid [direction [x y :as position]]]
  (let [position (mapv + position direction)]
    [direction
     (if (get grid position)
       position
       (condp = direction
         east  [(min-x grid y) y]
         south [x (min-y grid x)]
         west  [(max-x grid y) y]
         north [x (max-y grid x)]))]))

(defn step-cube [grid [direction [x y :as position]]]
  (let [position (mapv + position direction)
        my (mod y size)
        mx (mod x size)
        to (fn [x d] (mapv #(+ (* %1 size) %2) x d))
        bottom-x [mx (- size 1)]
        bottom-y [my (- size 1)]
        left-x [0 mx]
        left-y [0 (- size 1 my)]
        right-x [(- size 1) mx]
        right-y [(- size 1) (- size 1 my)]
        top-x [mx 0]
        top-y [my 0]]
    (if (get grid position)
      [direction position]
      (condp =  [(mapv #(quot % size) [x y]) direction]
        ;; [ ][a][b]
        ;; [ ][c][ ]
        ;; [d][e][ ]
        ;; [f][ ][ ]
        [a north] [east  (to f left-x)]
        [a west]  [east  (to d left-y)]
        [b east]  [west  (to e right-y)]
        [b north] [north (to f bottom-x)]
        [b south] [west  (to c right-x)]
        [c east]  [north (to b bottom-y)]
        [c west]  [south (to d top-y)]
        [d north] [east  (to c left-x)]
        [d west]  [east  (to a left-y)]
        [e east]  [west  (to b right-y)]
        [e south] [west  (to f right-x)]
        [f east]  [north (to e bottom-y)]
        [f south] [south (to b top-x)]
        [f west]  [south (to a top-y)]))))

(defn move [step grid cursor amount]
  (loop [cursor cursor
         amount amount]
    (if (= 0 amount)
      cursor
      (let [[_ pos :as ncursor] (step grid cursor)]
        (case (get grid pos)
          :open (recur ncursor (dec amount))
          :wall cursor)))))

(defn rotate [[[x y] pos] command]
  [(case command
     :L [y (- x)]
     :R [(- y) x]) pos])

(defn solution [step input]
  (let [[m cs] (str/split input #"\R\R")
        commands (->> (re-seq #"\d+|[A-Z]" cs)
                      (map #(or (parse-long %) (keyword %))))
        grid (into {} (for [[y line] (map vector (range) (str/split-lines m))
                            [x char] (map vector (range) line)
                            :let [c ({\. :open \# :wall} char)]
                            :when c]
                        [[x y] c]))
        first-open (first (keep (fn [x] (when (= :open (grid [x 0])) [x 0])) (range)))
        [direction [x y]] (reduce
                           (fn [cursor command]
                             (if (keyword? command)
                               (rotate cursor command)
                               (move step grid cursor command)))
                           [east first-open]
                           commands)]
    (+ (* 1000 (inc y)) (* 4 (inc x)) (score direction))))

(defn part1 [input]
  (solution step-torus input))

(defn part2 [input]
  (solution step-cube input))
