(ns day15-sketch
  (:require
   [clojure.java.io :as io]
   [clojure2d.core :as c2d]
   [clojure2d.color :as color]
   [colors :as colors]
   [day15 :as solution]))

(def data
  (solution/read-input (slurp (io/resource "input15.txt"))))

(def max-d
  (->> data
       (map #(nth % 2))
       (reduce max)))

(defn draw [canvas _ ^long frameno _]
  (c2d/set-background canvas colors/aoc-background)
  (c2d/set-font canvas "Iosevka")
  (c2d/set-font-attributes canvas 20.)
  (let [fps 30
        scan-time 8
        loop (* fps 10)
        frame (inc (mod frameno loop))
        c 7
        sk (/ 1.7 10000)
        dx 175
        dy 150]

    (doseq [[[sx sy] _ d] data
            :let [d (min d (* max-d (/ frame (* fps scan-time))))]]

      (c2d/set-color canvas (color/set-alpha colors/aoc-silver 50))
      (c2d/quad canvas
                (+ dx (* sk (+ sx d))) (+ dy (* sk sy))
                (+ dx (* sk sx)) (+ dy (* sk (+ sy d)))
                (+ dx (* sk (- sx d))) (+ dy (* sk sy))
                (+ dx (* sk sx)) (+ dy (* sk (- sy d))))

      (c2d/set-color canvas colors/aoc-grey)
      (c2d/quad canvas
                (+ dx (* sk (+ sx d))) (+ dy (* sk sy))
                (+ dx (* sk sx)) (+ dy (* sk (+ sy d)))
                (+ dx (* sk (- sx d))) (+ dy (* sk sy))
                (+ dx (* sk sx)) (+ dy (* sk (- sy d)))
                true))

    (doseq [[[sx sy] [bx by] dist] data]
      (c2d/set-color canvas colors/aoc-light-blue)
      (c2d/ellipse canvas (+ dx (* sk sx)) (+ dy (* sk sy)) c c)

      (when (<= dist (* max-d (/ frame (* fps scan-time))))
        (c2d/set-color canvas colors/aoc-gold)
        (c2d/ellipse canvas (+ dx (* sk bx)) (+ dy (* sk by)) c c)))

    (when (< (* fps scan-time) frame)
      (c2d/set-color canvas colors/aoc-red)
      (c2d/ellipse canvas (+ dx (* sk 3334479)) (+ dy (* sk 3186981)) (+ 4 c) (+ 4 c)))

    #_(c2d/save-image (c2d/get-image canvas) (format "images/%03d.png" frame))))

(comment
  (c2d/show-window
   {:canvas (c2d/canvas 1000 1000 :highest)
    :window-name (str *ns*)
    :hint :highest
    :fps 30
    :draw-fn #'draw}))
