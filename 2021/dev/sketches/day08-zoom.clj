(ns sketches.day08-zoom
  (:require [clojure.java.io :as io]
            [clojure2d.core :as c2d]
            [clojure2d.color :as color]
            [clojure.string :as string]
            [clojure.set :as set]
            [day08 :refer [common-sectors-by-count
                           count->correct
                           correct->mixed
                           narrow]]))

(def iterations
  (vec (for [line (->> "input8.txt" io/resource slurp string/split-lines (take 3))]
         (let [counts [2 3 4 5 6 7]
               digits (->> line (re-seq #"\w+"))
               count->mixed (->> digits (map set) common-sectors-by-count)
               corrections (reductions
                            (fn [correct->mixed count]
                              (narrow correct->mixed (count->correct count) (count->mixed count)))
                            correct->mixed counts)
               mixed->correct (-> corrections last (update-vals first) set/map-invert)
               candidates (mapv (fn [correct->mixed]
                                  (->> correct->mixed
                                       (mapcat (fn [[k vs]] (map (fn [k v] {k [v]}) vs (repeat k))))
                                       (apply merge-with concat)))
                                corrections)]
           {:candidates candidates
            :input (vec digits)
            :processed (->> (reductions conj [] counts)
                            (mapv #(keep-indexed (fn [i s] (when ((set %) (count s)) i)) digits)))
            :result (mapv #(mapv mixed->correct %) digits)}))))

(defn draw-one [canvas _ ^long frameno _]
  (c2d/set-background canvas "#0f0f23")
  (let [part2 "#ffff66"
        known-part1 "#009900"
        known-part2 "#00cc00"
        l 30
        w 8
        pad w
        draw-horizontal (fn [x y]
                          (c2d/rect canvas (+ x w) y l w)
                          (c2d/ellipse canvas (+ x w) (+ y (/ w 2)) w w)
                          (c2d/ellipse canvas (+ x l w) (+ y (/ w 2)) w w))
        draw-vertical (fn [x y]
                        (c2d/rect canvas x (+ y w) w l)
                        (c2d/ellipse canvas (+ x (/ w 2)) (+ y w) w w)
                        (c2d/ellipse canvas (+ x (/ w 2)) (+ y l w) w w))
        draw-segment (fn [s grid-x grid-y]
                       (let [x (* grid-x (+ w w l pad))
                             y (* grid-y (+ w w w l l pad))]
                         (case s
                           \a (draw-horizontal x         y)
                           \b (draw-vertical   x         y)
                           \c (draw-vertical   (+ x l w) y)
                           \d (draw-horizontal x         (+ y l w))
                           \e (draw-vertical   x         (+ y l w))
                           \f (draw-vertical   (+ x l w) (+ y l w))
                           \g (draw-horizontal x         (+ y l w l w)))))
        draw-digit (fn [digit grid-x grid-y]
                     (doseq [s digit]
                       (draw-segment s grid-x grid-y)))
        frame (mod frameno (+ 3 (count (:candidates (first iterations)))))]

    (doseq [y (range (count iterations))
            :let [{:keys [candidates processed input]} (get iterations y)
                  iteration (min frame (dec (count candidates)))
                  candidates (get candidates iteration)]
            i (range 14)
            :let [x i]]
      (if ((set (get processed iteration)) i)
        (c2d/set-color canvas known-part2)
        (c2d/set-color canvas known-part1))
      (draw-digit (get input i) x (* 3 y))

      (doseq [s (get input i)
              :let [options (candidates s)]
              o options]
        (if (= 1 (count options))
          (c2d/set-color canvas part2)
          (c2d/set-color canvas (color/darken part2 (* 1.2 (dec (count options))))))
        (draw-segment o x (inc (* 3 y)))))

    (c2d/save-image (c2d/get-image canvas) (format "images/day08-%03d.jpg" frame))))

(comment
  (c2d/show-window
   {:canvas (c2d/canvas 750 730 :high)
    :window-name "day08"
    :hint :high
    :draw-fn draw-one}))
