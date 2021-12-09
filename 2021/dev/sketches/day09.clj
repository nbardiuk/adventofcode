(ns sketches.day09
  (:require [clojure.java.io :as io]
            [clojure2d.core :as c2d]
            [clojure2d.color :as color]
            [day09 :refer [grid parse-map]]))

(def heights (->> "input9.txt" io/resource slurp parse-map))

(defn draw [canvas _ ^long frameno _]
  (let [gradient (color/gradient ["#ffff66" "#0f0f23"])
        size (/ (c2d/width canvas) 100)
        step (mod frameno 15)]
    (doseq [[y x] (grid heights)
            :let [height (get-in heights [y x])]]
      (c2d/set-color canvas (gradient (/ (min height step) 9.)))
      (c2d/rect canvas (* x size) (* y size) size size))
    (c2d/save-image (c2d/get-image canvas) (format "images/day09-%03d.jpg" step))))

(comment
  (c2d/show-window
   {:canvas (c2d/canvas 2000 2000 :high)
    :window-name "day09"
    :hint :mid
    :h 500
    :w 500
    :draw-fn draw}))
