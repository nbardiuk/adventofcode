(ns sketches.day13
  (:require [clojure.java.io :as io]
            [clojure2d.core :as c2d]
            [clojure.java.math :as math]
            [day13 :refer [fold parse-input]]))

(defn fold-partial [value line percent]
  (let [destination (- line (math/abs (- line value)))]
    (- value (* percent (- value destination)))))

(def steps
  (let [{:keys [dots instructions]} (->> "input13.txt" io/resource slurp parse-input)
        iterations (->> (iterate (fn [[dots [i & is]]]
                                   (if i
                                     [(fold dots i) is]
                                     [dots nil]))
                                 [dots instructions])
                        (take (+ 1 (count instructions))))
        iterations (map (fn [[dots [[axis line]]]] {:dots dots :axis axis :line line}) iterations)]
    (for [{:keys [dots axis line] :as iteration} iterations
          percent (take-while #(<= % 1) (iterate #(* 1.5 %) 0.01))]
      (if axis
        (assoc iteration :dots (map #(update % axis fold-partial line percent) dots))
        iteration))))

(comment
  (c2d/show-window
   {:canvas (c2d/canvas 1500 1500 :highest)
    :window-name "day13"
    :hint :highest
    :fps 30
    :h 500
    :w 500
    :draw-fn
    (fn draw [canvas _ ^long frameno _]
      (c2d/set-background canvas "#0f0f23")
      (let [index (mod frameno (count steps))
            {:keys [dots axis line]} (nth steps index)
            max-x (inc (apply max (map first dots)))
            max-y (inc (apply max (map second dots)))
            size (min (/ (c2d/width canvas) max-x) (/ (c2d/height canvas) max-y))]
        (c2d/set-color canvas "#ffff66")
        (doseq [[x y] dots]
          (c2d/rect canvas (* x size) (* y size) size size))
        (c2d/set-color canvas "#0a4918")
        (case axis
          0 (c2d/rect canvas (* line size) 0 size (* max-y size))
          1 (c2d/rect canvas 0 (* line size) (* max-x size) size)
          nil)
        #_(c2d/save-image (c2d/get-image canvas) (format "images/day13-%03d.jpg" index))))}))
