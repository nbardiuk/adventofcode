(ns day18-sketch
  (:require
   [clojure.java.io :as io]
   [clojure2d.core :as c2d]
   [clojure2d.color :as color]
   [colors :as colors]
   [day18 :as solution]))

(def lava (solution/parse-points (slurp (io/resource "input18.txt"))))
(def box (solution/bounding-box lava))
(def water (solution/flood box (set lava)))

(defn draw [canvas _ ^long frameno _]
  (c2d/set-background canvas colors/aoc-background)
  (c2d/set-font canvas "Iosevka")
  (c2d/set-font-attributes canvas 20.)
  (let [loop (* 30 10)
        frame (mod frameno loop)
        cursor (abs (- 1.0 (* 2.0 (/ frame loop))))
        [[mnx mny mnz] [mxx mxy mxz]] box
        cz (long (+ mnz (* (- mxz mnz) cursor)))
        cx (long (+ mnx (* (- mxx mnx) cursor)))
        gc 18
        gcw (dec gc)
        pad 60
        mid 500]

    (c2d/set-color canvas (color/set-alpha colors/aoc-red 60))
    (doseq [[x _y z] lava]
      (c2d/rect canvas (+ pad (* gc x)) (+ pad (* gc z)) gcw gcw))
    (c2d/set-color canvas colors/aoc-light-blue)
    (doseq [x (range mnx (inc mxx))]
      (c2d/rect canvas (+ pad (* gc x)) (+ pad (* gc cz)) gcw gcw))
    (c2d/set-color canvas colors/aoc-gold)
    (doseq [y (range mny (inc mxy))
            x (range mnx (inc mxx))]
      (c2d/rect canvas (+ mid pad (* gc x)) (+ pad (* gc y)) gcw gcw))
    (c2d/set-color canvas colors/aoc-red)
    (doseq [[x y z] lava :when (= z cz)]
      (c2d/rect canvas (+ mid pad (* gc x)) (+ pad (* gc y)) gcw gcw))
    (c2d/set-color canvas colors/aoc-light-blue)
    (doseq [[x y z] water :when (= z cz)]
      (c2d/rect canvas (+ mid pad (* gc x)) (+ pad (* gc y)) gcw gcw))

    (c2d/set-color canvas (color/set-alpha colors/aoc-red 60))
    (doseq [[x _y z] lava]
      (c2d/rect canvas (+ pad (* gc x)) (+ mid pad (* gc z)) gcw gcw))
    (c2d/set-color canvas colors/aoc-light-blue)
    (doseq [z (range mnz (inc mxz))]
      (c2d/rect canvas (+ pad (* gc cx)) (+ mid pad (* gc z)) gcw gcw))
    (c2d/set-color canvas colors/aoc-gold)
    (doseq [y (range mny (inc mxy))
            z (range mnz (inc mxy))]
      (c2d/rect canvas (+ mid pad (* gc y)) (+ mid pad (* gc z)) gcw gcw))
    (c2d/set-color canvas colors/aoc-light-blue)
    (doseq [[x y z] water :when (= x cx)]
      (c2d/rect canvas (+ mid pad (* gc y)) (+ mid pad (* gc z)) gcw gcw))
    (c2d/set-color canvas colors/aoc-red)
    (doseq [[x y z] lava :when (= x cx)]
      (c2d/rect canvas (+ mid pad (* gc y)) (+ mid pad (* gc z)) gcw gcw))

    #_(c2d/save-image (c2d/get-image canvas) (format "images/%03d.png" frame))))

(comment
  (c2d/show-window
   {:canvas (c2d/canvas 1000 1000 :highest)
    :window-name (str *ns*)
    :hint :highest
    :fps 30
    :draw-fn #'draw}))
