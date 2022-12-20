(ns day20-sketch
  (:require
   [clojure.java.io :as io]
   [clojure2d.core :as c2d]
   [clojure2d.color :as color]
   [fastmath.core :as math]
   [colors :as colors]
   [day20 :as solution]))

(def numbers (solution/parse-longs (slurp (io/resource "input20.txt"))))
(def indexed (map-indexed (fn [i n] [i n]) numbers))
(def steps (reductions solution/mix (vec indexed) indexed))
(def final-idex (->> (last steps) (map-indexed (fn [i v] [v i])) (into {})))

(defn draw [canvas _ ^long frameno _]
  (c2d/set-background canvas colors/aoc-background)
  (c2d/set-font canvas "Iosevka")
  (c2d/set-font-attributes canvas 20.)
  (let [loop (* 30 10)
        frame (inc (mod frameno loop))
        indexed (nth steps (* (count steps) (/ frame (- loop (* 2 10)))) (last steps))
        n  125
        dr 12]
    (doseq [[i v] (reverse (map vector (range) indexed))]
      (let [fi (final-idex v)
            r (quot i n)
            a (mod i n)
            fr (quot fi n)
            fa (mod fi n)]
        (c2d/set-color canvas (color/from-HWB [(* 360 fa (/ 1 n)) 0 (/ fr 60)]))
        (c2d/rarc canvas
                  500
                  500
                  (* r dr)
                  (* math/TWO_PI a (/ 1 n))
                  (- (* math/TWO_PI (/ 1 n)))
                  :pie
                  false)))

    #_(c2d/save-image (c2d/get-image canvas) (format "images/%03d.png" frame))))

(comment
  (c2d/show-window
   {:canvas (c2d/canvas 1000 1000 :highest)
    :window-name (str *ns*)
    :hint :highest
    :fps 30
    :draw-fn #'draw}))
