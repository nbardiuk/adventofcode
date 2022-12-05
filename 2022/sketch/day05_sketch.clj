(ns day05-sketch
  (:require
   [clojure.java.io :as io]
   [clojure2d.core :as c2d]
   [clojure2d.color :as color]
   [fastmath.core :as math]
   [colors :as colors]
   [day05 :as solution]))

(def input (solution/parse-input (slurp (io/resource "input05.txt"))))

(def stacks
  (vec (for [[s i] (map vector (:stacks input) (range))]
         (for [[c j] (map vector s (range))]
           {:label c
            :index [i j]}))))

(def steps-1
  (vec (reductions (partial solution/step reverse) stacks (:commands input))))
(def steps-2
  (vec (reductions (partial solution/step identity) stacks (:commands input))))

(def index->color
  (->> steps-2 last
       (mapcat #(map-indexed (fn [i c] [(:index c) (- 15 i)]) %))
       (into {})))

(def palette
  (color/palette
   (color/gradient
    [colors/aoc-gold colors/aoc-brown colors/aoc-dark-grey]
    {:colorspace :HSL, :interpolation :cubic-spline})
   16))

(defn draw [canvas _ ^long frameno _]
  (c2d/set-background canvas colors/aoc-background)
  (c2d/set-font canvas "Iosevka")
  (c2d/set-font-attributes canvas 20.)
  (let [loop (* 30 10)
        frame (mod frameno loop)
        cursor (->> (range frame)
                    (map #(math/pow 10 (/ (- loop (math/abs (- (/ loop 2) %))) (/ loop 2))))
                    (reduce +)
                    (* 0.045)
                    long)
        stacks-1 (get steps-1 cursor (last steps-1))
        stacks-2 (get steps-2 cursor (last steps-2))]
    (c2d/set-color canvas colors/aoc-dark-grey)
    (c2d/rect canvas 499 0 2 1000)
    (doseq [i (range (count stacks-1))]
      (let [stack    (get stacks-1 i)
            stack-c  (count stack)
            x        (+ 5 (* i 55))]
        (doseq [j (range stack-c)]
          (let [{:keys [label index]} (nth stack j)
                y (- 1000 (* 30 (- stack-c j)))]
            (c2d/set-color canvas (get palette (index->color index)))
            (c2d/rect canvas x y 50 28)
            (c2d/set-color canvas (get palette (- 15 (index->color index))))
            (c2d/text canvas label (+ 22 x) (+ 21 y))))))
    (doseq [i (range (count stacks-2))]
      (let [stack    (get stacks-2 i)
            stack-c  (count stack)
            x        (+ 505 (* i 55))]
        (doseq [j (range stack-c)]
          (let [{:keys [label index]} (nth stack j)
                y (- 1000 (* 30 (- stack-c j)))]
            (c2d/set-color canvas (get palette (index->color index)))
            (c2d/rect canvas x y 50 28)
            (c2d/set-color canvas (get palette (- 15 (index->color index))))
            (c2d/text canvas label (+ 22 x) (+ 21 y))))))
    (let [x 25 y 25 w 170 h 45]
      (c2d/set-color canvas colors/aoc-dark-background)
      (c2d/rect canvas x y w h)
      (c2d/set-color canvas colors/aoc-dark-grey)
      (c2d/rect canvas x y w h true)
      (c2d/set-color canvas colors/aoc-silver)
      (c2d/text canvas "CrateMover 9000" (+ 10 x) (+ 30 y)))
    (let [x 525 y 25 w 170 h 45]
      (c2d/set-color canvas colors/aoc-dark-background)
      (c2d/rect canvas x y w h)
      (c2d/set-color canvas colors/aoc-dark-grey)
      (c2d/rect canvas x y w h true)
      (c2d/set-color canvas colors/aoc-gold)
      (c2d/text canvas "CrateMover 9001" (+ 10 x) (+ 30 y)))
    #_(c2d/save-image (c2d/get-image canvas) (format "images/%03d.png" frame))))

(comment
  (c2d/show-window
   {:canvas (c2d/canvas 1000 1000 :highest)
    :window-name (str *ns*)
    :hint :highest
    :fps 30
    :draw-fn #'draw}))
