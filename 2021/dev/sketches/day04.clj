(ns sketches.day04
  (:require [quil.core :as q]
            [clojure.java.io :as io]
            [clojure.string :as string]))

(def my-input (slurp (io/resource "input4.txt")))

(defn- parse-input [input]
  (let [[draws & boards] (string/split-lines input)]
    {:draws (->> draws (re-seq #"\d+") (map parse-long))
     :boards (->> boards
                  (mapcat #(re-seq #"\d+" %))
                  (map (fn [value]
                         {:marked? false
                          :bingo? false
                          :number (parse-long value)}))
                  (partition 25))}))

(defn- mark [board draw]
  (if (some :bingo? board)
    board
    (let [marked (vec (for [cell board]
                        (update cell :marked? #(or % (= draw (:number cell))))))]
      (->> (concat (->> (range 5) (map #(take 5 (range (* % 5) 25))))
                   (->> (range 5) (map #(take 5 (range % 25 5)))))
           (filter (fn [indexes] (every? #(get-in marked [% :marked?]) indexes)))
           first
           (reduce (fn [board i] (assoc-in board [i :bingo?] true)) marked)))))

(def steps
  (let [{:keys [boards draws]} (parse-input my-input)]
    (->> draws
         (reductions
          (fn [boards draw] (map #(mark % draw) boards))
          boards))))

(def step (atom 0))

(defn draw []
  (swap! step #(-> % inc (rem (count steps))))

  (q/smooth)
  (q/color-mode :hsb 360 100 100 1.0)
  (q/background 240 57 13)
  (q/no-stroke)

  (let [boards (nth steps @step)
        board-pad 2
        size (min (q/width) (q/height))
        board-outer-size (/ size 10.)
        board-inner-size (- board-outer-size (* 2 board-pad))
        cell-pad 2.5
        cell-outer-size (/ board-inner-size 5.)
        cell-inner-size (- cell-outer-size (* 2 cell-pad))]
    (doseq [row (range 10)
            col (range 10)
            :let [board-x (+ board-pad (* row board-outer-size))
                  board-y (+ board-pad (* col board-outer-size))
                  board (nth boards (+ col (* 10 row)))]]
      (q/fill 240 57 13)
      (q/rect board-x board-y
              board-inner-size board-inner-size)

      (doseq [x (range 5)
              y (range 5)
              :let [cell (nth board (+ y (* 5 x)))]]
        (cond
          (:bingo? cell) (q/fill 60 60 100) ; gold
          (:marked? cell) (q/fill 193 100 100) ; blue
          :else (q/fill 0 0 80 0.7) ; grey
          )
        (q/rect (+ board-x cell-pad (* x cell-outer-size))
                (+ board-y cell-pad (* y cell-outer-size))
                cell-inner-size cell-inner-size))))
  #_(q/save (format "images/day04-%03d.jpg" @step)))

(comment (q/defsketch day04
           :title "day04"
           :draw draw
           :size [1024 1024]))
