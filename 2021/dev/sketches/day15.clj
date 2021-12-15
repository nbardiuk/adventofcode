(ns sketches.day15
  (:require [clojure2d.core :as c2d]
            [clojure2d.color :as color]
            [day15 :refer [parse-input grid-size neighbours]]
            [day15-test :refer [my-input]])
  (:import [java.util PriorityQueue]))

(def input my-input)

(def input-grid (parse-input input))

(defn slimes [[w h] grid slime queue]
  (lazy-seq
   (let [dest [(dec w) (dec h)]
         [risk pos path] (.poll queue)
         slime (update-vals slime #(max 0 (dec %)))
         slime (reduce #(assoc %1 %2 1000) slime path)]
     (cond
       (= pos dest) [slime]
       (nil? risk) nil
       :else (let [elements (for [npos (neighbours pos)
                                  :let [nrisk (grid npos)]
                                  :when nrisk]
                              [(+ risk nrisk) npos (conj path npos)])]
               (.addAll queue elements)
               (cons slime
                     (slimes
                      [w h]
                      (->> elements (map second) (reduce dissoc grid))
                      slime
                      queue)))))))

(def steps
  (let [slimes (slimes (grid-size input-grid) input-grid {}
                       (new PriorityQueue [[0 [0 0] [[0 0]]]]))]
    (concat slimes
            (->> (last slimes)
                 (iterate (fn [slime] (update-vals slime #(if (< % 1000) (max 0 (dec %)) %))))
                 (take-while #(not= 2 (count (set (vals %)))))))))

(defn draw [canvas _ ^long frameno _]
  (c2d/set-background canvas "#0f0f23")
  (let [index (* 40 frameno)]
    (when (< index (count steps))
      (let [slime (nth steps index)
            [w] (grid-size input-grid)
            size (/ (c2d/width canvas) w)
            grid-gradient (color/gradient ["#0f0f23" "#00c8ff"])
            slime-gradient (color/gradient ["#0f0f23" "#ffff66"])]
        (doseq [[[x y] v] input-grid]
          (c2d/set-color canvas (grid-gradient (/ (inc v) 10.)))
          (c2d/rect canvas (* size x) (* size y) size size))
        (doseq [[[x y] v] slime]
          (c2d/set-color canvas (slime-gradient (/ v 1000.)))
          (c2d/rect canvas (* size x) (* size y) size size))
        #_(c2d/save-image (c2d/get-image canvas) (format "images/day15-%05d.jpg" index))))))

(comment
  (c2d/show-window
   {:canvas (c2d/canvas 1500 1500 :highest)
    :window-name "day15"
    :hint :highest
    :fps 30
    :h 700
    :w 700
    :draw-fn #'draw}))
