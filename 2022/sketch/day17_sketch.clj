(ns day17-sketch
  (:require
   [clojure2d.core :as c2d]
   [fastmath.core :as math]
   [colors :as colors]
   [day17 :as solution]))

(def wind
  (keep {\> 1 \< -1} ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"))

(def steps
  (->> (cycle solution/shapes)
       (reduce (fn [{:keys [cup height wind steps]} shape]
                 (let [r (loop [cup cup
                                [blow & wind] wind
                                shape (solution/initialize shape height)
                                steps (conj steps (solution/freeze cup shape))]
                           (let [[cup shape] (->> shape (solution/side cup blow) (solution/down cup))]
                             (if shape
                               (recur cup wind shape (conj steps (solution/freeze cup shape)))
                               {:cup cup
                                :steps (conj steps cup)
                                :height (->> cup keys (map second) (reduce max -1))
                                :wind wind})))]
                   (if (< 5000 (count (:steps r)))
                     (reduced r)
                     r)))
               {:cup {}
                :steps []
                :height -1
                :wind (cycle wind)})
       :steps))

(def shapes-palette
  {:- colors/aoc-dark-blue
   :+ colors/aoc-red
   :j colors/aoc-light-green
   :i colors/aoc-light-blue
   :o colors/aoc-gold})

(defn draw [canvas _ ^long frameno _]
  (c2d/set-background canvas colors/aoc-background)
  (let [loop (* 30 10)
        frame (inc (mod frameno loop))
        cursor (->> (range (min frame (- loop (* 30 1))))
                    (map #(math/pow 200 (min 0.65 (/ % loop))))
                    (reduce +)
                    (* 0.02))
        shapes (nth steps (* (count steps) (/ cursor loop)) (last steps))
        height (* 0.36 (count shapes))
        cycle-st 24
        cycle-len 53
        pattern (->> shapes (filter (fn [[[_ y] _]] (<= (+ 1 cycle-st cycle-len) y (+ cycle-st (* 2 cycle-len))))))
        pattern (when (= 154 (count pattern)) pattern)

        dx 400
        dy (- 100 (* 2 cursor))
        c (- 20 (* 0.17 cursor))
        w (- 19 (* 0.17 cursor))
        depth 120
        sdx 700
        sdy 200
        sc 12
        sw 11
        cup-width 7]

    (doseq [[[x y] cl]  shapes
            :let [y (- height y)]]
      (c2d/set-color canvas (shapes-palette cl))
      (c2d/rect canvas (+ dx (* c x)) (+ dy (* c y)) w w))

    (doseq [[[x y] cl] pattern
            :let [y (+ depth (- y (+ cycle-st (* 2 cycle-len))))
                  y (- depth y)]]
      (c2d/set-color canvas (shapes-palette cl))
      (c2d/rect canvas (+  sdx (* sc x)) (+ sdy (* sc y)) sw sw))

    (when pattern
      (c2d/set-color canvas colors/aoc-gold)
      (doseq [y (range cycle-st height cycle-len)
              :let [y (+ depth (- y height))
                    y (- depth y)]]
        (c2d/line canvas dx (+ dy  (* c y)) (+ dx (* cup-width c)) (+ dy  (* c y)))
        (c2d/line canvas (+ dx (* cup-width c)) (+ dy  (* c y)) sdx (+ sdy (* cycle-len sc))))
      (c2d/line canvas sdx (+ sdy (* cycle-len sc)) (+ sdx (* cup-width sc)) (+ sdy (* cycle-len sc))))

    #_(c2d/save-image (c2d/get-image canvas) (format "images/%03d.png" frame))))

(comment
  (c2d/show-window
   {:canvas (c2d/canvas 1000 1000 :highest)
    :window-name (str *ns*)
    :hint :highest
    :fps 30
    :draw-fn #'draw}))
