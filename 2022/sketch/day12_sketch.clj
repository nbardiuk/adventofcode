(ns day12-sketch
  (:require
   [clojure.java.io :as io]
   [clojure2d.core :as c2d]
   [clojure2d.color :as color]
   [colors :as colors]
   [day12 :as solution])
  (:import [java.util PriorityQueue]))

(def my-map
  (solution/read-map (slurp (io/resource "input12.txt"))))

(defn path [grid start end? wall? dist]
  (let [start (find grid start)
        distance (->> grid
                      keys
                      (map (fn [p] [p (dist p)]))
                      (into {}))

        queue (new PriorityQueue 100
                   (comparator
                    (fn [[ptha [posa]] [pthb [posb]]]
                      (< (+ (distance posa) (count ptha))
                         (+ (distance posb) (count pthb))))))]
    (.add queue [[start] start [start]])
    (loop [seen? #{}]
      (let [[path [pos :as current]] (.poll queue)]
        (cond
          (end? current) path
          (seen? current) (recur seen?)
          :else (let [ns (for [next (solution/neighbours grid pos)
                               :when (not (wall? current next))]
                           [(conj path (conj next seen?)) next])]
                  (.addAll queue ns)
                  (recur (conj seen? current))))))))

(def path1
  (let [{:keys [grid S E]} my-map]
    (path grid S
          (fn [[position]] (= E position))
          (fn [[_ level] [_ next-level]]
            (< 1 (- (long next-level) (long level))))
          (fn [[x y]]
            (+ (abs (- x (first E)))
               (abs (- y (second E))))))))

(def ends (filter (comp #{\a} val) (:grid my-map)))

(def path2
  (let [{:keys [grid E]} my-map]
    (path grid E
          (fn [[_ level]] (= \a level))
          (fn [[_ level] [_ next-level]]
            (< 1 (- (long level) (long next-level))))
          (fn [[x y]]
            (->> (for [[[ex ey]] ends]
                   (+ (abs (- x ex))
                      (abs (- y ey))))
                 (reduce min))))))

(count path1)
(count path2)

(def green-palette
  (color/palette
   (color/gradient
    [(color/darken colors/aoc-dark-green 2)
     colors/aoc-dark-green
     colors/aoc-brown
     (color/brighten colors/aoc-brown 2)]
    {:colorspace :HSL, :interpolation :cubic-spline})
   (inc (- (int \z) (int \a)))))

(def grey-palette
  (color/palette
   (color/gradient
    [(color/darken colors/aoc-dark-grey 2)
     colors/aoc-dark-grey
     colors/aoc-grey
     (color/brighten colors/aoc-grey 1)]
    {:colorspace :HSL, :interpolation :cubic-spline})
   (inc (- (int \z) (int \a)))))

(defn draw [canvas _ ^long frameno _]
  (c2d/set-background canvas colors/aoc-background)
  (c2d/set-font canvas "Iosevka")
  (c2d/set-font-attributes canvas 20.)
  (let [loop (* 30 14)
        frame (inc (mod frameno loop))
        path1 (take frame path1)
        path2 (take frame path2)
        [_ _ traversed1] (last path1)
        [_ _ traversed2] (last path2)
        c 11]

    (let [sx 190
          sy 30]
      (c2d/set-color canvas colors/aoc-light-blue)
      (c2d/text canvas (str "Path: " (count path1)) 60 (+ sy 20))
      (c2d/text canvas (str "Area: " (count traversed1)) 60 (+ sy 50))

      (doseq [[[x y] e] (:grid my-map)]
        (c2d/set-color canvas (get grey-palette (- (long e) (long \a))))
        (c2d/rect canvas (+ sx (* c x)) (+ sy (* c y)) c c))
      (doseq [[[x y] e] traversed1]
        (c2d/set-color canvas (get green-palette (- (long e) (long \a))))
        (c2d/rect canvas (+ sx (* c x)) (+ sy (* c y)) c c))
      (doseq [[[x y] e] path1]
        (c2d/set-color canvas colors/aoc-light-blue)
        (c2d/rect canvas (+ sx (* c x)) (+ sy (* c y)) c c)))

    (let [sx 190
          sy 520]
      (c2d/set-color canvas colors/aoc-gold)
      (c2d/text canvas (str "Path: " (count path2)) 60 (+ sy 20))
      (c2d/text canvas (str "Area: " (count traversed2)) 60 (+ sy 50))

      (doseq [[[x y] e] (:grid my-map)]
        (c2d/set-color canvas (get grey-palette (- (long e) (long \a))))
        (c2d/rect canvas (+ sx (* c x)) (+ sy (* c y)) c c))
      (doseq [[[x y] e] traversed2]
        (c2d/set-color canvas (get green-palette (- (long e) (long \a))))
        (c2d/rect canvas (+ sx (* c x)) (+ sy (* c y)) c c))
      (doseq [[[x y] e] path2]
        (c2d/set-color canvas colors/aoc-gold)
        (c2d/rect canvas (+ sx (* c x)) (+ sy (* c y)) c c)))

    #_(c2d/save-image (c2d/get-image canvas) (format "images/%03d.png" frame))))

(comment
  (c2d/show-window
   {:canvas (c2d/canvas 1000 1000 :highest)
    :window-name (str *ns*)
    :hint :highest
    :fps 20
    :draw-fn #'draw}))
