(ns day10-sketch
  (:require
   [clojure.java.io :as io]
   [clojure2d.core :as c2d]
   [clojure2d.color :as color]
   [fastmath.core :as math]
   [colors :as colors]
   [day10 :as solution]))

(def my-signal (solution/signal (slurp (io/resource "input10.txt"))))
(def example-signal (solution/signal (slurp (io/resource "example10.txt"))))
(def my-screen
  (vec
   (for [y (range 6)
         x (range 40)]
     (let [time     (+ x (* 40 y))
           sprite   (get my-signal time)
           overlap? (<= -1 (- x sprite) 1)]
       {:x x
        :y y
        :overlap? overlap?
        :sprite sprite}))))
(def example-screen
  (vec
   (for [y (range 6)
         x (range 40)]
     (let [time     (+ x (* 40 y))
           sprite   (get example-signal time)
           overlap? (<= -1 (- x sprite) 1)]
       {:x x
        :y y
        :overlap? overlap?
        :sprite sprite}))))

(defn draw-screen [canvas screen screen-x screen-y pixel-h pixel-w]
  (c2d/set-color canvas colors/aoc-silver)
  (when-let [sprite (some->> screen last :sprite dec)]
    (c2d/rect canvas (+ screen-x (* sprite pixel-w)) screen-y (* (min 3 (- 40 sprite)) pixel-w) (* 6 pixel-h)))

  (c2d/set-color canvas colors/aoc-gold)
  (doseq [{:keys [x y overlap?]} screen]
    (when overlap?
      (c2d/rect canvas (+ screen-x (* x pixel-w)) (+ screen-y (* y pixel-h)) pixel-w pixel-h)))

  (c2d/set-color canvas colors/aoc-light-green)
  (when-let [{:keys [x y overlap?]} (->> screen last)]
    (c2d/set-color canvas (if overlap? colors/aoc-gold colors/aoc-light-green))
    (c2d/rect canvas (+ screen-x (* x pixel-w)) (+ screen-y (* y pixel-h)) pixel-w pixel-h))

  (c2d/set-color canvas colors/aoc-dark-background)
  (doseq [y (range 6)
          x (range 40)]
    (c2d/rect canvas (+ screen-x (* x pixel-w)) (+ screen-y (* y pixel-h)) pixel-w pixel-h true))

  (c2d/set-color canvas colors/aoc-dark-grey)
  (let [padding 7]
    (c2d/rect canvas
              (- screen-x padding)
              (- screen-y padding)
              (+ (* 2 padding) (* 40 pixel-w))
              (+ (* 2 padding) (* 6 pixel-h))
              true)))

(defn draw [canvas _ ^long frameno _]
  (c2d/set-background canvas colors/aoc-background)
  (c2d/set-font canvas "Iosevka")
  (c2d/set-font-attributes canvas 20.)
  (let [loop 240
        frame (inc (mod frameno loop))]

    (draw-screen canvas (take frame example-screen) 20 125 48 24)
    (draw-screen canvas (take frame my-screen) 20 575 48 24)

    #_(c2d/save-image (c2d/get-image canvas) (format "images/%03d.png" frame))))

(comment
  (c2d/show-window
   {:canvas (c2d/canvas 1000 1000 :highest)
    :window-name (str *ns*)
    :hint :highest
    :fps 20
    :draw-fn #'draw}))
