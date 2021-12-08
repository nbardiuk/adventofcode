(ns sketches.day08
  (:require [clojure.java.io :as io]
            [clojure2d.core :as c2d]
            [clojure.string :as string]
            [clojure.set :as set]
            [day08 :refer [common-sectors-by-count
                           count->correct
                           correct->mixed
                           narrow]]))

(defn- process [line]
  (let [digits (re-seq #"\w+" line)
        count->mixed (->> digits (map set) common-sectors-by-count)
        correct->mixed (reduce (fn [correct->mixed [count correct]]
                                 (narrow correct->mixed correct (count->mixed count)))
                               correct->mixed count->correct)
        mixed->correct (-> correct->mixed (update-vals first) set/map-invert)]
    (->> digits
         (mapv (fn [sectors]
                 (let [correct (map mixed->correct sectors)]
                   {:mixed sectors
                    :correct correct}))))))

(def lines (->> "input8.txt" io/resource slurp
                string/split-lines
                (mapv process)))

(defn draw [canvas _ ^long frameno _]
  (c2d/set-background canvas "#0f0f23")
  (let [unknown "#5f5f5f"
        known-part1 "#009900"
        known-part2 "#00cc00"
        part1 "#9999cc"
        part2 "#ffff66"
        rows 40
        l 10
        w 2
        pad w
        draw-digit (fn [digit grid-x grid-y]
                     (doseq [s digit
                             :let [x (* grid-x (+ w w l pad))
                                   y (* grid-y (+ w w w l l pad))]]
                       (case s
                         \a (c2d/rect canvas (+ x w)   y             l w)
                         \b (c2d/rect canvas x         (+ y w)       w l)
                         \c (c2d/rect canvas (+ x l w) (+ y w)       w l)
                         \d (c2d/rect canvas (+ x w)   (+ y l w)     l w)
                         \e (c2d/rect canvas x         (+ y l w w)   w l)
                         \f (c2d/rect canvas (+ x l w) (+ y l w w)   w l)
                         \g (c2d/rect canvas (+ x w)   (+ y l l w w) l w))))
        freeze 10
        per-row 2
        part-size (* per-row 200)
        cycle (mod frameno (+ freeze part-size part-size freeze))]
    (doseq [j (range (count lines))
            i (range 14)
            :let [x (+ i (* 14 (quot j rows)))
                  x (+ x (quot x 14))
                  y (mod j rows)
                  scene (cond
                          (< cycle freeze) 0
                          (and (< (- cycle freeze) part-size) (<= (/ (- cycle freeze) per-row) j)) 0
                          (and (< (- cycle freeze) part-size) (< j (/ (- cycle freeze) per-row))) 1
                          (and (< (- cycle freeze part-size) part-size) (<= (/ (- cycle freeze part-size) per-row) j)) 1
                          (and (< (- cycle freeze part-size) part-size) (< j (/ (- cycle freeze part-size) per-row))) 2
                          :else 2)]]

      (case scene
        0 (do
            (c2d/set-color canvas unknown)
            (draw-digit (get-in lines [j i :mixed]) x y))

        1 (let [{:keys [mixed correct]} (get-in lines [j i])]
            (cond
              (and (#{2 3 4 7} (count correct)) (< i 10)) (c2d/set-color canvas known-part1)
              (and (#{2 3 4 7} (count correct)) (<= 10 i)) (c2d/set-color canvas part1)
              :else (c2d/set-color canvas unknown))
            (if (#{2 3 4 7} (count correct))
              (draw-digit correct x y)
              (draw-digit mixed x y)))

        2 (let [{:keys [correct]} (get-in lines [j i])]
            (cond
              (and (#{2 3 4 7} (count correct)) (< i 10)) (c2d/set-color canvas known-part1)
              (< i 10) (c2d/set-color canvas known-part2)
              (#{2 3 4 7} (count correct)) (c2d/set-color canvas part1)
              :else (c2d/set-color canvas part2))
            (draw-digit correct x y))))
    (c2d/save-image (c2d/get-image canvas) (format "images/day08-%03d.jpg" cycle))))

(comment
  (c2d/show-window
   {:canvas (c2d/canvas 1186 1120 :high)
    :window-name "day08"
    :hint :high
    :w 1000
    :h 1000
    :draw-fn draw}))
