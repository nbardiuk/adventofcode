(ns sketcher.day11
  (:require [clojure.java.io :as io]
            [clojure2d.core :as c2d]
            [clojure2d.color :as color]
            [fastmath.vector :as v]
            [fastmath.core :as m]
            [day11 :refer [parse-grid flash]]))

(def levels (->> "input11.txt" io/resource slurp parse-grid))

(defn- take-until-repeat [xs]
  (->> (partition 2 1 xs)
       (take-while #(apply not= %))
       (map second)
       (cons (first xs))))

(defn step [levels]
  (->> (update-vals (last levels) inc)
       (iterate flash)
       take-until-repeat))

(def steps (->> [levels] (iterate step) (apply concat) (take 1233) vec))

(defn draw [canvas _ ^long frameno _]
  (c2d/set-background canvas "#0f0f23")
  (let [flash "#ffff66"
        gradient (color/gradient ["#0f0f23" "#00c8ff"])
        size (/ (c2d/width canvas) 10)
        index (mod frameno (count steps))
        levels (nth steps index)]
    (doseq [[[x y] level] levels
            :let [cx (+ (/ size 2) (* x size))
                  cy (+ (/ size 2) (* y size))
                  body-size (* 0.6 size (if (zero? level) 1 (+ 1.1 -1/3 (/ level (* 3 9.)))))]]
      (c2d/set-color canvas (if (zero? level) flash (gradient (* 0.5 (/ level 9.)))))
      (c2d/ellipse canvas cx cy body-size body-size)
      (c2d/path canvas (for [i (range 8)
                             :let [r (m/radians (* i 135))
                                   len (* size 0.45
                                          (if (zero? level) 1 (/ level 9.))
                                          (if (zero? (mod i 2)) 1. 1.15))]]
                         (v/vec2 (+ cx (* len (m/sin r)))
                                 (+ cy (* len (m/cos r)))))
                true false))

    #_(c2d/save-image (c2d/get-image canvas) (format "images/day11-%03d.jpg" index))))

(comment
  (c2d/show-window
   {:canvas (c2d/canvas 1000 1000 :highest)
    :window-name "day11"
    :hint :highest
    :h 1000
    :w 1000
    :draw-fn draw}))
