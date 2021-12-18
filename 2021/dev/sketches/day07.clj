(ns sketches.day07
  (:require [clojure.java.io :as io]
            [clojure2d.core :as c2d]
            [clojure.java.math :as math]
            [day00 :refer [take-until-repeat]]
            [day07 :refer [floor-mean median distance parse-longs]]))

(def positions (->> "input7.txt" io/resource slurp parse-longs))
(def mean-position (floor-mean positions))
(def median-position (median positions))

(def steps
  (let [acceleration 2]
    (->> positions
         (map
          (fn [pos]
            {:speed-mean 1 :pos-mean pos
             :speed-median 1 :pos-median pos}))
         (iterate
          (fn [positions]
            (->> positions
                 (mapv
                  (fn [{:keys [speed-median pos-median speed-mean pos-mean] :as crab}]
                    (let [speed-median (* (compare median-position pos-median)
                                          (min (+ acceleration (math/abs speed-median))
                                               (distance pos-median median-position)))
                          speed-mean (* (compare mean-position pos-mean)
                                        (min (+ acceleration (math/abs speed-mean))
                                             (distance pos-mean mean-position)))]
                      (-> crab
                          (assoc :speed-median speed-median)
                          (update :pos-median + speed-median)
                          (assoc :speed-mean speed-mean)
                          (update :pos-mean + speed-mean))))))))

         take-until-repeat
         vec)))

(defn draw [canvas _ ^long frameno _]
  (when (< frameno (count steps))
    (c2d/set-background canvas "#0f0f23")
    (let [step (mod frameno (count steps))
          crabs (nth steps step)]
      (doseq [i (range (count crabs))
              :let [crab (nth crabs i)]]
        (c2d/set-color canvas "#9999cc")
        (c2d/ellipse canvas (:pos-median crab) i 6 6)
        (c2d/set-color canvas "#ffff66")
        (c2d/ellipse canvas (:pos-mean crab) (+ 1000 i) 6 6))
      #_(c2d/save-image (c2d/get-image canvas) (format "images/day07-%03d.jpg" step)))))

(comment
  (c2d/show-window
   {:canvas (c2d/canvas 2000 2000 :high)
    :window-name "day07"
    :hint :high
    :w 1000
    :h 1000
    :draw-fn draw}))
