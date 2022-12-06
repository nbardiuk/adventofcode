(ns day06-sketch
  (:require
   [clojure2d.core :as c2d]
   [clojure2d.color :as color]
   [colors :as colors]))

(def examples
  ["mjqjpqmgbljsphdztnvjfqwrcgsmlb"
   "bvwbjplbgvbhsrlpgdmjqwftvncz"
   "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"
   "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"])

(def frs4
  (vec (for [input examples]
         (->> input
              (partition 4 1)
              (mapv frequencies)
              (take-while #(not= 1 (count (set (vals %)))))
              vec))))
(def frs14
  (vec (for [input examples]
         (->> input
              (partition 14 1)
              (mapv frequencies)
              (take-while #(not= 1 (count (set (vals %)))))
              vec))))

(defn enumerate [xs]
  (map-indexed vector xs))

(defn draw [canvas _ ^long frameno _]
  (c2d/set-background canvas colors/aoc-background)
  (c2d/set-font canvas "Iosevka")
  (c2d/set-font-attributes canvas 20.)
  (let [loop (* 30 10)
        frame (mod frameno loop)
        w 27
        hk 4
        left-x 50
        delay 100
        frame-height 210]

    (doseq [[e input] (enumerate examples)]
      (let [fr (get frs4 e)
            cursor (min (int (/ frame 10)) (count fr))]
        (doseq [i (range 4)]
          (let [c (nth input (+ cursor i))
                f (nth fr cursor nil)
                h (* hk (- (int c) (int \a)))
                y (+ 125 (- (* 250 e) h))]
            (when (and f (< 1 (f c)))
              (c2d/set-color canvas (color/set-alpha colors/aoc-red 150))
              (c2d/rect canvas (+ left-x (* w (+ cursor i))) y w (* 2 h)))))
        (c2d/set-color canvas colors/aoc-silver)
        (c2d/rect canvas
                  (+ left-x (* w cursor)) (+ 125 (- (* 250 e) (/ frame-height 2)))
                  (* 4 w) frame-height
                  true)
        (c2d/set-color canvas colors/aoc-silver)
        (c2d/text canvas (+ 4 cursor)
                  (+ 3 left-x (* w (+ 4 cursor)))
                  (+ 125 (- (* 250 e) (/ frame-height 2))))))

    (when (< delay frame)
      (doseq [[e input] (enumerate examples)]
        (let [fr (get frs14 e)
              cursor (min (int (/ (- frame delay) 10)) (count fr))]
          (doseq [i (range 14)]
            (let [c (nth input (+ cursor i))
                  f (nth fr cursor nil)
                  h (* hk (- (int c) (int \a)))
                  y (+ 125 (- (* 250 e) h))]
              (when (and f (< 1 (f c)))
                (c2d/set-color canvas (color/set-alpha colors/aoc-red 150))
                (c2d/rect canvas (+ left-x (* w (+ cursor i))) y w (* 2 h)))))
          (c2d/set-color canvas colors/aoc-gold)
          (c2d/rect canvas (+ left-x (* w cursor)) (+ 125 (- (* 250 e) (/ frame-height 2))) (* 14 w) frame-height true)
          (c2d/set-color canvas colors/aoc-gold)
          (c2d/text canvas (+ 14 cursor)
                    (+ 3 left-x (* w (+ 14 cursor)))
                    (+ 125 (- (* 250 e) (/ frame-height 2)))))))

    (doseq [[e input] (enumerate examples)
            [i char] (enumerate input)]
      (let [x (+ left-x (* i w))
            h (* hk (- (int char) (int \a)))
            y (+ 125 (- (* 250 e) h))]

        (c2d/set-color canvas colors/aoc-dark-green)
        (c2d/rect canvas x y w (* 2 h) true)
        (c2d/set-color canvas (color/set-alpha colors/aoc-dark-green 50))
        (c2d/rect canvas x y w (* 2 h) false)
        (c2d/set-color canvas colors/aoc-light-green)
        (c2d/text canvas char (+ x (* 0.3 w)) (+ y h 5) true)))

    #_(c2d/save-image (c2d/get-image canvas) (format "images/%03d.png" frame))))

(comment
  (c2d/show-window
   {:canvas (c2d/canvas 1000 1000 :highest)
    :window-name (str *ns*)
    :hint :highest
    :fps 30
    :draw-fn #'draw}))
