(ns day04-sketch
  (:require
   [clojure.java.io :as io]
   [clojure2d.core :as c2d]
   [fastmath.core :as math]
   [colors :as colors]
   [day04 :as solution]))

(def ranges
  (solution/range-pairs (slurp (io/resource "input04.txt"))))

(def overlaps
  (->> ranges
       (mapcat identity)
       (map (fn [[a b]]
              (into {} (map vector (range a (inc b)) (repeat 1)))))
       (reductions (partial merge-with +))
       vec))

(def counts
  (->> ranges
       (map (fn [r]
              {:part1 (if (solution/fully-contains? r) 1 0)
               :part2 (if (solution/partially-overlaps? r) 1 0)}))
       (reductions (partial merge-with +))
       vec))

(defn draw [canvas _ ^long frameno _]
  (c2d/set-background canvas colors/aoc-background)
  (c2d/set-font canvas "Iosevka")
  (c2d/set-font-attributes canvas 20.)
  (let [loop (* 10 30)
        frame (mod frameno loop)
        cursor (->> (range frame)
                    (map #(math/pow 10 (/ (- loop (math/abs (- (/ loop 2) %))) (/ loop 2))))
                    (reduce +)
                    (* 0.022))
        shift (- (- (* cursor 40) 550))]

    (doseq [i (range 1000)]
      (let [[[a b] [c d] :as r] (nth ranges i)
            y (+ shift (* 10 i))]
        (when (<= 500 y)
          (c2d/set-color canvas colors/aoc-brown)
          (doseq [j (into (into #{} (range a (inc b))) (range c (inc d)))]
            (c2d/rect canvas (* 10 j) y 8 8))
          (cond
            (solution/fully-contains? r)     (c2d/set-color canvas colors/aoc-silver)
            (solution/partially-overlaps? r) (c2d/set-color canvas colors/aoc-gold))
          (when (solution/partially-overlaps? r)
            (doseq [j (into #{} (range (max a c) (inc (min b d))))]
              (c2d/rect canvas (* 10 j) y 8 8))))))

    (c2d/set-color canvas colors/aoc-brown)
    (doseq [[i v] (nth overlaps (* 8 cursor) (last overlaps))]
      (c2d/rect canvas (* 10 i) (- 500 (* 0.43 v)) 8 (* 0.43 v)))

    (let [{:keys [part1 part2]} (nth counts (* 4 cursor) (last counts))
          y 410
          x1 200
          x2 650
          w 150
          h 50]
      (c2d/set-color canvas colors/aoc-dark-background)
      (c2d/rect canvas x1 y w h)
      (c2d/set-color canvas colors/aoc-inactive-text)
      (c2d/rect canvas x1 y w h true)

      (c2d/set-color canvas colors/aoc-dark-background)
      (c2d/rect canvas x2 y w h)
      (c2d/set-color canvas colors/aoc-inactive-text)
      (c2d/rect canvas x2 y w h true)

      (c2d/set-color canvas colors/aoc-silver)
      (c2d/text canvas (str "Part 1: " part1) (+ 20 x1) (+ 35 y))

      (c2d/set-color canvas colors/aoc-gold)
      (c2d/text canvas (str "Part 2: " part2) (+ 20 x2) (+ 35 y)))

    #_(c2d/save-image (c2d/get-image canvas) (format "images/%03d.png" frame))))

(comment
  (c2d/show-window
   {:canvas (c2d/canvas 1000 1000 :highest)
    :window-name (str *ns*)
    :hint :highest
    :fps 30
    :draw-fn #'draw}))
