(ns sketches.day06
  (:require [quil.core :as q]
            [clojure.java.io :as io]))

(def input (->> "input6.txt"
                io/resource
                slurp
                (re-seq #"\d+")
                (map parse-long)))

(defn- dec-clock [max v]
  (if (zero? v) max (dec v)))

(do
  (def steps
    (let [items 55000
          steps 500]
      (->> input
           (iterate (fn [timers]
                      (->> (concat timers
                                   (->> timers (filter zero?) (map (constantly 9))))
                           (map #(dec-clock 6 %))
                           (take items)
                           vec)))
           (take steps)
           vec)))

  (def last-count (count (last steps))))

(def step (atom 0))

(defn setup []
  (q/frame-rate 30)
  (reset! step 0))

(defn draw []
  (swap! step inc)
  (if (<= (count steps) @step)
    (q/exit)
    ; (comment (q/exit))
    (do
      (q/smooth)
      (q/color-mode :hsb 360 100 100 1.0)
      (q/tint 0 0 0 0)
      (q/background 240 57 13)
      (q/no-stroke)
      (q/no-fill)

      (let [colors (for [i (range 9)
                         :let [mn 20
                               mx 100
                               v (- mx (/ (- mx mn) (/ 9.0 i)))]]
                     (q/color 60 60 v))
            ages (nth steps @step)
            ; ages (last steps)
            cx (/ (q/width) 2)
            cy (/ (q/height) 2)
            im (q/create-image (q/width) (q/height) :rgb)]

        (doseq [i (range (count ages))
                :let [age (nth ages i)
                      r (* 3 (Math/sqrt i))
                      th (* Math/PI (Math/sqrt i))
                      x (+ cx (* r (Math/cos th)))
                      y (+ cy (* r (Math/sin th)))]
                a (range -1 2)]
          (q/set-pixel im (+ x a) y (nth colors age))
          (q/set-pixel im x (+ y a) (nth colors age)))
        (q/set-image 0 0 im))

      #_(q/save (format "images/day06-%03d.jpg" @step)))))

(comment (q/defsketch day06
           :title "day06"
           :draw draw
           :setup setup
           :size [1000 1000]))
