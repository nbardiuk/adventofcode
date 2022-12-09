(ns day09-sketch
  (:require
   [clojure.java.io :as io]
   [clojure2d.core :as c2d]
   [clojure2d.color :as color]
   [fastmath.core :as math]
   [colors :as colors]
   [day09 :as solution]))

(def larger-example
  "R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20")
(def motions (solution/parse-motions larger-example))
(def track
  (let [rope (repeat 10 {:x 11 :y 15})]
    (->> motions
         (mapcat (fn [[direction steps]]
                   (repeat steps direction)))
         (reductions solution/move-rope rope)
         vec)))

(def track1
  (let [rope (repeat 2 {:x 11 :y 15})]
    (->> motions
         (mapcat (fn [[direction steps]]
                   (repeat steps direction)))
         (reductions solution/move-rope rope)
         vec)))

(defn draw [canvas _ ^long frameno _]
  (c2d/set-background canvas colors/aoc-background)
  (c2d/set-font canvas "Iosevka")
  (c2d/set-font-attributes canvas 20.)
  (let [loop (* 30 10)
        frame (mod frameno loop)
        cursor (long (/ frame 2.5))
        c 22]

    (let [rope (get track1 cursor (last track1))
          tail (->> track1 (take (inc cursor)) (map last) distinct)
          dx 270
          dy 20
          m 5
          w (+ (* 2 m) (* 26 c))
          h (+ (* 2 m) (* 21 c))]
      (c2d/set-color canvas colors/aoc-dark-grey)
      (c2d/rect canvas (- dx m) (- dy m) w h true)

      (c2d/set-color canvas colors/aoc-silver)
      (doseq [{:keys [x y]} tail]
        (c2d/rect canvas (+ dx (* c x)) (+ dy (* c y)) c c))

      (c2d/set-color canvas colors/aoc-brown)
      (doseq [{:keys [x y]} rope]
        (c2d/ellipse canvas (+ dx (* c x) (/ c 2)) (+ dy (* c y) (/ c 2)) c c))

      (c2d/set-color canvas colors/aoc-silver)
      (c2d/text canvas (str "Part 1: " (count tail)) (- dx 150) (+ dy 20)))

    (let [rope (get track cursor (last track))
          tail (->> track (take (inc cursor)) (map last) distinct)
          dx 270
          dy 520
          m 5
          w (+ (* 2 m) (* 26 c))
          h (+ (* 2 m) (* 21 c))]
      (c2d/set-color canvas colors/aoc-dark-grey)
      (c2d/rect canvas (- dx m) (- dy m) w h true)

      (c2d/set-color canvas colors/aoc-gold)
      (doseq [{:keys [x y]} tail]
        (c2d/rect canvas (+ dx (* c x)) (+ dy (* c y)) c c))

      (c2d/set-color canvas colors/aoc-brown)
      (doseq [{:keys [x y]} rope]
        (c2d/ellipse canvas (+ dx (* c x) (/ c 2)) (+ dy (* c y) (/ c 2)) c c))

      (c2d/set-color canvas colors/aoc-gold)
      (c2d/text canvas (str "Part 2: " (count tail)) (- dx 150) (+ dy 20)))

    #_(c2d/save-image (c2d/get-image canvas) (format "images/%03d.png" frame))))

(comment
  (c2d/show-window
   {:canvas (c2d/canvas 1000 1000 :highest)
    :window-name (str *ns*)
    :hint :highest
    :fps 30
    :draw-fn #'draw}))
