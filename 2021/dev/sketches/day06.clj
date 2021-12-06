(ns sketches.day06
  (:require [clojure.java.io :as io]
            [clojure2d.core :as c2d]
            [clojure2d.color :as color]))

(def input (->> "input6.txt"
                io/resource
                slurp
                (re-seq #"\d+")
                (map parse-long)))

(defn- dec-clock [max v]
  (if (zero? v) max (dec v)))

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

(def age-colors
  (->> (range 9)
       (mapv #(color/darken "#ffff66" (* 4 (/ % 9.0))))))

(defn draw-2d [canvas _ ^long frameno _]
  (let [step (mod frameno (count steps))
        ages (nth steps step)
        cx (/ (c2d/width canvas) 2)
        cy (/ (c2d/width canvas) 2)]
    (doseq [i (range (count ages))
            :let [age (nth ages i)
                  r (* 3 (Math/sqrt i))
                  th (* Math/PI (Math/sqrt i))
                  x (+ cx (* r (Math/cos th)))
                  y (+ cy (* r (Math/sin th)))]]
      (c2d/set-color canvas (nth age-colors age))
      (c2d/ellipse canvas x y 2 2))
    #_(c2d/save-image (c2d/get-image canvas) (format "images/day06-%03d.jpg" step))))

(comment
  (def window
    (c2d/show-window
     {:canvas (c2d/canvas 1000 1000 :high)
      :window-name "day06"
      :hint :high
      :w 1000
      :h 1000
      :draw-fn draw-2d
      :setup (fn [c _] (c2d/set-background c "#0f0f23"))})))
