(ns sketches.day17
  (:require [clojure2d.core :as c2d]
            [fastmath.grid :as grid]
            [day17 :refer [x-pos y-pos]]))

(def hi
  [[1 0]                    [5 0] [10 0]
   [1 1]                    [5 1] [10 1]
   [1 2]                    [5 2] [10 2]
   [1 3] [2 3] [3 3] [4 3]  [5 3] [10 3]
   [1 4]                    [5 4] [10 4]
   [1 5]                    [5 5] [10 5]
   [1 6]                    [5 6] [10 6]])

(def steps
  (let [sx 5
        sy -4
        result
        (->> (for [[x y] (sort hi)
                   :let [x (+ sx x)
                         y (+ sy (- y))
                         [vx vy t]
                         (->> (for [vy (range y (inc (- y)))
                                    t (->> (range)
                                           (take-while #(<= y (y-pos vy %)))
                                           (filter #(= (y-pos vy %) y)))
                                    vx (range 1 (+ sx 20))
                                    :when (= x (x-pos vx t))]
                                [vx vy t])
                              (apply max-key second))]]
               {:x x
                :y y
                :vx vx
                :vy vy
                :trajectory
                (->> (for [t (range (inc t))]
                       [(x-pos vx t) (y-pos vy t)])
                     vec)})
             vec)]
    (reduce
     (fn [result i]
       (assoc-in result [i :trace] (->> result (take i) (mapv #(select-keys % [:x :y])))))
     result (range (count result)))))

(defn draw [canvas _ ^long frameno _]
  (c2d/set-background canvas "#0f0f23")
  (let [index (mod frameno (count steps))
        {:keys [vx vy trajectory trace]} (nth steps index)
        g (grid/grid :square 13.)
        ga #(first (grid/cell->anchor g [% %]))
        gc #(first (grid/cell->mid g [% %]))
        gr #(c2d/gradient-mode canvas (ga -70) (ga -150) "#0f0f23" (ga 20) (ga 30) %)]

    (c2d/translate canvas 400. 750.)
    (c2d/flip-y canvas)

    (gr "#ffff00")
    (c2d/line canvas (gc 0) (gc 0) (ga (- vx)) (ga (- vy)))
    (c2d/ellipse canvas (ga (- vx)) (ga (- vy)) (ga 3) (ga 3))

    (gr "#0000ff")
    (c2d/rect canvas (ga 4) (ga -12) (ga 15) (ga 11))

    (gr "#ffff00")
    (doseq [[x y] trajectory]
      (c2d/rect canvas (ga x) (ga y) (ga 1) (ga 1)))

    (gr "#00ff00")
    (doseq [{:keys [x y]} trace]
      (c2d/rect canvas (ga x) (ga y) (ga 1) (ga 1)))

    #_(c2d/save-image (c2d/get-image canvas) (format "images/day17-%03d.jpg" index))))

(comment
  (c2d/show-window
   {:canvas (c2d/canvas 1000 1000 :highest)
    :window-name "day17"
    :hint :highest
    :fps 5
    :h 1000
    :w 1000
    :draw-fn #'draw}))
