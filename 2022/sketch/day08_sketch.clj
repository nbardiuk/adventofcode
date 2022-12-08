(ns day08-sketch
  (:require
   [clojure.java.io :as io]
   [clojure2d.core :as c2d]
   [clojure2d.color :as color]
   [fastmath.core :as math]
   [colors :as colors]
   [day08 :as solution]))

(def grid (solution/parse-grid
           (slurp (io/resource "input08.txt"))))

(def visible
  (->> (for [[height sides] (solution/tree-view-seq grid)]
         (some #(solution/every-smaller? height %) sides))
       (partition 99)
       (mapv vec)))

(def visible-count
  (->> visible
       flatten
       (filter identity)
       count))

(def views
  (->> (for [[height sides] (solution/tree-view-seq grid)]
         (->> sides
              (map #(solution/count-visible height %))
              (reduce *)))
       (partition 99)
       (mapv vec)))

(def palette
  (color/palette
   (color/gradient
    [(color/brighten colors/aoc-dark-background 1.2) colors/aoc-dark-green]
    {:colorspace :HSL, :interpolation :cubic-spline})
   9))

(defn draw [canvas _ ^long frameno _]
  (c2d/set-background canvas colors/aoc-background)
  (c2d/set-font canvas "Iosevka")
  (c2d/set-font-attributes canvas 20.)
  (let [loop (* 30 10)
        frame (mod frameno loop)
        gs 9]

    (doseq [[r row] (solution/enumerate grid)
            [c height] (solution/enumerate row)]
      (let [visible? (get-in visible [r c])
            view     (get-in views [r c])]
        (cond
          ; (and (< 100 frame) visible?)
          ; (c2d/set-color canvas colors/aoc-silver)

          (and (< 200 frame) (= 209880 view))
          (c2d/set-color canvas colors/aoc-gold)

          :else
          (c2d/set-color canvas (get palette height)))

        (let [d 6
              d (- d (* 4 (/ (min frame 100) 100) (if visible? 0 1)))
              d (+ d (* 0.00006 (/ (math/constrain (- frame 100) 0 100) 100) view))]
          (c2d/ellipse canvas (+ 50 (* (+ 1 c) gs)) (+ 90 (* (+ 1 r) gs)) d d))))

    (c2d/set-color canvas colors/aoc-silver)
    (c2d/text canvas (str "Visible trees: " (when (< 70 frame) visible-count)) 60 40)

    (c2d/set-color canvas colors/aoc-gold)
    (c2d/text canvas (str "Best scenic score: " (when (< 200 frame) 209880)) 60 75)

    #_(c2d/save-image (c2d/get-image canvas) (format "images/%03d.png" frame))))

(comment
  (c2d/show-window
   {:canvas (c2d/canvas 1000 1000 :highest)
    :window-name (str *ns*)
    :hint :highest
    :fps 30
    :draw-fn #'draw}))
