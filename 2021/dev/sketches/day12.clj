(ns sketches.day12
  (:require [clojure.java.io :as io]
            [clojure2d.core :as c2d]
            [day12 :refer [lazy-paths graph]]))

(def input
  (->> "input12.txt" io/resource slurp graph))

(def paths (->> (lazy-paths ;#(every? #{1} (vals %))
                 (constantly false)
                 input
                 [{:caves ["start"] :small-freq {}}])
                (shuffle)
                (mapv :caves)))

(def locations
  {"start" [10 0]
   "gx"    [8 5]
   "NY"    [12 5]
   "pf"    [10 9]
   "pk"    [4 12]
   "iz"    [16 12]
   "ag"    [10 13]
   "dc"    [8 16]
   "BN"    [12 16]
   "ZQ"    [0 18]
   "end"   [20 18]
   "FD"    [10 20]})

(comment
  (c2d/show-window
   {:canvas (c2d/canvas 1500 1500 :highest)
    :window-name "day12"
    :hint :highest
    :fps 30
    :h 1000
    :w 1000
    :draw-fn
    (fn draw [canvas _ ^long frameno _]
      (c2d/set-background canvas "#0f0f23")
      (c2d/set-font canvas "Iosevka")
      (let [index (mod frameno (count paths))
            path (nth paths index)
            max-x (+ 1 (apply max (map first (vals locations))))
            max-y (+ 1 (apply max (map second (vals locations))))
            size (min (/ (c2d/width canvas) max-x) (/ (c2d/height canvas) max-y))]
        (c2d/set-font-attributes canvas (/ size 2))
        (c2d/set-stroke canvas (/ size 8))
        (doseq [[from tos] input
                to tos
                :let [[ax ay] (map #(+ (/ size 2) (* size %)) (locations to))
                      [bx by] (map #(+ (/ size 2) (* size %)) (locations from))]]
          (c2d/set-color canvas "#005500")
          (c2d/line canvas ax ay bx by))
        (c2d/set-stroke canvas (/ size 24))
        (doseq [[from to] (partition 2 1 path)
                :let [[ax ay] (map #(+ (/ size 2) (* size %)) (locations to))
                      [bx by] (map #(+ (/ size 2) (* size %)) (locations from))]]
          (c2d/set-color canvas "#ffff66")
          (c2d/line canvas ax ay bx by))
        (doseq [[name [x y]] locations]
          (c2d/set-color canvas "#004a8a")
          (c2d/ellipse canvas (+ (/ size 2) (* x size)) (+ (/ size 2) (* y size)) size size)
          (c2d/set-color canvas "#cccccc")
          (c2d/text canvas name (+ (/ size 2) (* x size)) (+ (* size 0.6) (* y size)) :center))

        (c2d/save-image (c2d/get-image canvas) (format "images/day12-%03d.jpg" index))))}))
