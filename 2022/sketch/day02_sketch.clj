(ns day02-sketch
  (:require
   [clojure.java.io :as io]
   [clojure2d.core :as c2d]
   [colors :as colors]
   [day02 :refer [constant-response decode-round dynamic-response
                  parse-strategy round-score]]))

(def strategy (parse-strategy (slurp (io/resource "input02.txt"))))
(def part1-moves (mapv (partial decode-round dynamic-response) strategy))
(def part2-moves (mapv (partial decode-round constant-response) strategy))

(def steps
  (->> (mapv vector
             (->> part1-moves (map (comp round-score reverse)) (reductions +))
             (->> part1-moves (map round-score) (reductions +))
             (->> part2-moves (map (comp round-score reverse)) (reductions +))
             (->> part2-moves (map round-score) (reductions +))
             (->> part1-moves (map first) (reductions conj []) (map frequencies))
             (->> part1-moves (map second) (reductions conj []) (map frequencies))
             (->> part2-moves (map second) (reductions conj []) (map frequencies)))))

(defn draw-paper [canvas x y width height]
  (let [k 0.6
        xlb x
        ylb y
        xrb (+ x (* k width))
        yrb y
        xlt (+ x (* (- 1. k) width))
        ylt (- y height)
        xrt (+ x width)
        yrt (- y height)]
    (c2d/triangle canvas xlb ylb (+ 0.5 xlt) ylt (+ 0.5 xrb) yrb)
    (c2d/triangle canvas xrt yrt xlt ylt xrb yrb)))

(defn draw-scissors [canvas x y width height]
  (let [k 0.6]
    (c2d/triangle canvas
                  x y
                  (+ x width) y
                  (+ x width) (- y (* k height)))
    (c2d/triangle canvas
                  x (- y height)
                  (+ x width) (- y height)
                  (+ x width) (- y (* (- 1. k) height)))))

(defn draw-stats [canvas fr x y]
  (let [{:keys [paper rock scissors]
         :or {paper 0 rock 0 scissors 0}} fr]
    (c2d/rect canvas x         (- y 60 (/ rock 7))     50 (/ rock 7))
    (c2d/rect canvas (+ x 75)  (- y 60 (/ scissors 7)) 50 (/ scissors 7))
    (c2d/rect canvas (+ x 150) (- y 60 (/ paper 7))    50 (/ paper 7))

    (c2d/text canvas rock     x         (- y 65 (/ rock 7)))
    (c2d/text canvas scissors (+ x 75)  (- y 65 (/ scissors 7)))
    (c2d/text canvas paper    (+ x 150) (- y 65 (/ paper 7))))

  (c2d/ellipse canvas   (+ x 25)  (- y 20) 45 40)
  (draw-scissors canvas (+ x 75)  y        45 40)
  (draw-paper canvas    (+ x 150) y        50 40))

(defn draw [canvas _ ^long frameno _]
  (c2d/set-background canvas colors/aoc-background)
  (c2d/set-font canvas "Iosevka")
  (c2d/set-font-attributes canvas 20.)
  (let [loop (* 30 8)
        frame (mod frameno loop)
        step (* (inc frame) (/ (count steps) loop))
        k (int (/ (count steps) 300))
        scores (->> steps
                    (take step)
                    (keep-indexed (fn [i v] (when (= 0 (mod i k)) v)))
                    vec)]

    (when-let [[op1 mine1 op2 mine2 op-fr my-fr1 my-fr2] (peek scores)]
      (let [x (count scores)
            xb1 100
            xb2 600
            yb1 885
            yb2 885]
        (c2d/set-color canvas colors/aoc-dark-grey)
        (c2d/line canvas xb1 yb1 (+ xb1 300) yb1)
        (c2d/line canvas xb2 yb2 (+ xb2 300) yb2)
        (c2d/line canvas xb1 yb1 xb1 (- yb1 200))
        (c2d/line canvas xb2 yb2 xb2 (- yb2 200))
        (c2d/set-color canvas colors/aoc-grey)
        (c2d/text canvas op1   (+ xb1 x) (- yb1 -15 (/ op1 100)))
        (c2d/set-color canvas colors/aoc-silver)
        (c2d/text canvas mine1 (+ xb1 x) (- yb1 (/ mine1 100)))
        (c2d/set-color canvas colors/aoc-grey)
        (c2d/text canvas op2   (+ xb2 x) (- yb2 -15 (/ op2 100)))
        (c2d/set-color canvas colors/aoc-gold)
        (c2d/text canvas mine2 (+ xb2 x) (- yb2 (/ mine2 100))))

      (c2d/set-color canvas colors/aoc-grey)
      (draw-stats canvas  op-fr 75 450)

      (c2d/set-color canvas colors/aoc-silver)
      (draw-stats canvas  my-fr1 400 450)

      (c2d/set-color canvas colors/aoc-gold)
      (draw-stats canvas  my-fr2 725 450))

    (doseq [i (range (count scores))]
      (let [[op1 mine1 op2 mine2] (get scores i)
            x i
            xb1 100
            xb2 600
            yb1 885
            yb2 885]
        (c2d/set-color canvas colors/aoc-grey)
        (c2d/ellipse canvas (+ xb1 x) (- yb1 (/ op1 100)) 2 2)
        (c2d/ellipse canvas (+ xb2 x) (- yb2 (/ op2 100)) 2 2)
        (c2d/set-color canvas colors/aoc-silver)
        (c2d/ellipse canvas (+ xb1 x) (- yb1 (/ mine1 100)) 2 2)
        (c2d/set-color canvas colors/aoc-gold)
        (c2d/ellipse canvas (+ xb2 x) (- yb2 (/ mine2 100)) 2 2)))

    (c2d/set-color canvas colors/aoc-dark-green)
    (c2d/text canvas "Frequency of shapes" 30 75)
    (c2d/set-color canvas colors/aoc-dark-green)
    (c2d/text canvas "Score per round" 30 600)
    (c2d/set-color canvas colors/aoc-grey)
    (c2d/text canvas "Opponent" 75 140)
    (c2d/set-color canvas colors/aoc-silver)
    (c2d/text canvas "Part 1" 400 140)
    (c2d/set-color canvas colors/aoc-gold)
    (c2d/text canvas "Part 2" 725 140)

    #_(c2d/save-image (c2d/get-image canvas) (format "images/%03d.png" frame))))

(comment
  (c2d/show-window
   {:canvas (c2d/canvas 1000 1000 :highest)
    :window-name (str *ns*)
    :hint :highest
    :fps 30
    :draw-fn #'draw}))

(comment
  (print (->>
        (for [i (range 240 301)]
          (str "cp 239.png " i ".png"))
        (clojure.string/join "\n"))))
