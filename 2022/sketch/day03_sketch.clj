(ns day03-sketch
  (:require
   [clojure.java.io :as io]
   [clojure2d.core :as c2d]
   [fastmath.core :as math]
   [colors :as colors]
   [clojure.string :as str]
   [day03 :as solution]))

(def rucksacks (str/split-lines (slurp (io/resource "input03.txt"))))

(def part1
  (for [r rucksacks]
    (let [common (solution/shared-item (solution/halves r))]
      {:char common
       :index (str/index-of r common)
       :last-index (str/last-index-of r common)})))

(def part2
  (let [commons (->> rucksacks
                     (partition 3)
                     (map solution/shared-item)
                     (mapcat #(repeat 3 %)))]
    (for [[r common] (map vector rucksacks commons)]
      {:char common
       :index (str/index-of r common)})))

(def scores1
  (->> part1
       (map (comp solution/priority :char))
       (reductions +)
       vec))

(def scores2
  (->> part2
       (map (comp solution/priority :char))
       (map-indexed (fn [i v] (if (= 0 (mod i 3)) v 0)))
       (reductions +)
       vec))

(defn letter-x [canvas index]
  (c2d/text-width canvas (str/join (repeat index "W"))))

(def fps 30)
(def loop-time 10)

(defn draw [canvas _ ^long frameno _]
  (c2d/set-background canvas colors/aoc-background)
  (c2d/set-font canvas "Iosevka")
  (c2d/set-font-attributes canvas 20.)
  (let [loop (* fps loop-time)
        frame (mod frameno loop)
        cursor (->> (range frame)
                    (map #(math/pow 10 (/ (- loop (math/abs (- (/ loop 2) %))) (/ loop 2))))
                    (reduce +)
                    (* 0.027)
                    (+ -5.0))
        row-height 24
        left-x 200
        scroll (min 0
                    (max (- (* 262 row-height))
                         (- (* 22 (- cursor 5)))))]

    (c2d/set-color canvas colors/aoc-inactive-text)
    (doseq [i (range (count rucksacks))]
      (c2d/text canvas (nth rucksacks i) left-x (+ scroll (* row-height (inc i)))))

    (c2d/set-color canvas colors/aoc-silver)
    (doseq [i (range (min (+ 5 (int cursor)) (count part1)))]
      (let [{:keys [index last-index char]} (nth part1 i)]
        (c2d/text canvas char (+ left-x (letter-x canvas index))      (+ scroll (* row-height (inc i))))
        (c2d/text canvas char (+ left-x (letter-x canvas last-index)) (+ scroll (* row-height (inc i))))))
    (c2d/text canvas
              (get scores1 (min (dec (count rucksacks)) (+ 4 (int cursor))))
              (+ left-x 550)
              (+ scroll (* row-height (min (inc (count rucksacks)) (+ 5 (int cursor))))))

    (c2d/set-color canvas colors/aoc-gold)
    (doseq [i (range (min (+ 3 (* 3 (quot cursor 3))) (count part2)))]
      (let [{:keys [index char]} (nth part2 i)]
        (c2d/text canvas char (+ left-x (letter-x canvas index)) (+ scroll (* row-height (inc i))))))
    (c2d/text canvas
              (get scores2 (min (dec (count rucksacks)) (int (* 3 (quot cursor 3)))))
              (+ left-x 550)
              (+ scroll (* row-height (min (+ 2 (count rucksacks)) (+ 2 (* 3 (quot cursor 3)))))))

    (c2d/set-color canvas colors/aoc-silver)
    (c2d/rect canvas
              (- left-x 25)
              (+ 3 scroll (* row-height (min (count rucksacks) (+ 4 (int cursor)))))
              650
              row-height
              true)
    (c2d/set-color canvas colors/aoc-gold)
    (c2d/rect canvas
              (- left-x 25)
              (+ 3 scroll (* row-height (min (count rucksacks) (* 3 (quot cursor 3)))))
              650
              (* row-height 3) true)
    #_(c2d/save-image (c2d/get-image canvas) (format "images/%03d.png" frame))))

(comment
  (c2d/show-window
   {:canvas (c2d/canvas 1000 1000 :highest)
    :window-name (str *ns*)
    :hint :highest
    :fps fps
    :draw-fn #'draw}))
