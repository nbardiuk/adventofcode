(ns day23-sketch
  (:require
   [clojure.java.io :as io]
   [clojure2d.core :as c2d]
   [clojure2d.color :as color]
   [fastmath.core :as math]
   [colors :as colors]
   [day23 :as solution]))

(def elfs
  (solution/parse-elfs (slurp (io/resource "input23.txt"))))

(def steps
  (take 1005 (solution/steps elfs)))

(def palette
  (color/palette
   (color/gradient
    [colors/aoc-red
     colors/aoc-red
     colors/aoc-gold
     colors/aoc-light-blue]
    {:colorspace :HSL, :interpolation :cubic-spline})
   9))

(defn draw [canvas _ ^long frameno _]
  (c2d/set-background canvas colors/aoc-background)
  (c2d/set-font canvas "Iosevka")
  (c2d/set-font-attributes canvas 20.)
  (let [loop (* 30 10)
        frame (inc (mod frameno loop))
        s (* (count steps) (/ frame (- loop (* 1 30))))
        step (nth steps s (last steps))
        gc 7
        w (* 1.3 gc)
        dx 120
        dy 120]
    (doseq [[x y] step]
      (c2d/set-color canvas (palette (count (remove step (solution/neighbours-all [x y])))))
      (c2d/ellipse canvas (+ dx (* gc x)) (+ dy (* gc y)) w w))

    #_(c2d/save-image (c2d/get-image canvas) (format "images/%03d.png" frame))))

(comment
  (c2d/show-window
   {:canvas (c2d/canvas 1000 1000 :highest)
    :window-name (str *ns*)
    :hint :highest
    :fps 30
    :draw-fn #'draw}))
