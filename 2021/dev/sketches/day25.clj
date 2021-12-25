(ns sketches.day25
  (:require [clojure2d.core :as c2d]
            [day00 :refer [take-until-repeat]]
            [day25-test :refer [my-input]]
            [day25 :refer [parse step]]))

(def steps
  (->> (parse my-input)
       (iterate step)
       take-until-repeat
       vec))

(defn draw [canvas _ ^long frameno _]
  (c2d/set-background canvas "#0f0f23")
  (let [index (mod frameno (count steps))
        scene (nth steps index)
        dx (/ (double (c2d/width canvas)) (:width scene))
        dy (/ (double (c2d/height canvas)) (:height scene))]

    (doseq [[x y] (:right scene)]
      (c2d/set-color canvas "#9999cc")
      (c2d/ellipse canvas (+ (* dx x) (/ dx 2)) (+ (* dy y) (/ dy 2)) dx dy))

    (doseq [[x y] (:down scene)]
      (c2d/set-color canvas "#5555cc")
      (c2d/ellipse canvas (+ (* dx x) (/ dx 2)) (+ (* dy y) (/ dy 2)) dx dy))

    (c2d/save-image (c2d/get-image canvas) (format "images/day25-%03d.jpg" index))))

(comment
  (c2d/show-window
   {:canvas (c2d/canvas 1024 768 :highest)
    :window-name (str *ns*)
    :hint :highest
    :draw-fn #'draw}))
