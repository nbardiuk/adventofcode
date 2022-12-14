(ns day14-sketch
  (:require
   [clojure.java.io :as io]
   [clojure2d.core :as c2d]
   [fastmath.core :as math]
   [colors :as colors]
   [day14 :as solution]))

(def obstacles (solution/read-obstacles (slurp (io/resource "input14.txt"))))
(def floor (->> obstacles (map second) (reduce max) (+ 2)))
(def floor? #(= floor (second %)))

(def part1
  (->> #{}
       (iterate (fn [grains]
                  (conj grains (solution/drop-grain (some-fn grains obstacles floor?)))))
       (#(nth % 737))))

(def part2
  (->> #{}
       (iterate (fn [grains]
                  (conj grains (solution/drop-grain (some-fn grains obstacles floor?)))))
       (take 28145)))

(defn draw [canvas _ ^long frameno _]
  (c2d/set-background canvas colors/aoc-background)
  (c2d/set-font canvas "Iosevka")
  (c2d/set-font-attributes canvas 20.)
  (let [loop (* 30 10)
        frame (inc (mod frameno loop))
        cursor (->> (range frame)
                    (map #(math/pow 200 (/ % loop)))
                    (reduce +)
                    (* 5))
        sand (nth part2 cursor (last part2))
        c 5
        dx -2000
        dy 60]

    (doseq [[x y :as p] sand]
      (c2d/set-color canvas (if (part1 p) colors/aoc-gold colors/aoc-brown))
      (c2d/rect canvas (+ dx (* c x)) (+ dy (* c y)) c c))

    (c2d/set-color canvas colors/aoc-silver)
    (doseq [[x y] obstacles]
      (c2d/rect canvas (+ dx (* c x)) (+ dy (* c y)) c c))

    (c2d/set-color canvas colors/aoc-dark-grey)
    (c2d/rect canvas 0 (+ dy (* c floor)) 1000 c)

    #_(c2d/save-image (c2d/get-image canvas) (format "images/%03d.png" frame))))

(comment
  (c2d/show-window
   {:canvas (c2d/canvas 1000 1000 :highest)
    :window-name (str *ns*)
    :hint :highest
    :fps 20
    :draw-fn #'draw}))
