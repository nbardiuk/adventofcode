(ns day24-sketch
  (:require
   [clojure.java.io :as io]
   [clojure2d.core :as c2d]
   [clojure2d.color :as color]
   [colors :as colors]
   [day24 :as solution]
   [clojure.set :as set]))

(def valley (solution/parse-valley (slurp (io/resource "input24.txt"))))

(defn traverse [valley start end]
  (loop [valley valley
         positions #{start}
         paths [[start]]]
    (if (positions end)
      [(first (filter #(= end (peek %)) paths)) valley]
      (let [{:as valley :keys [wall max-x max-y right left up down]} (solution/update-valley valley)
            next-position  (comp (mapcat solution/neighbours) (filter (fn [[x y]] (or (= end [x y]) (= start [x y]) (and  (<= 1 y (dec max-y)) (<= 1 x (dec max-x)))))))
            next-positions (into #{} next-position positions)
            paths (for [path paths
                        n (solution/neighbours (peek path))
                        :when (next-positions n)]
                    (conj path n))
            paths (mapv peek (vals (group-by peek paths)))]
        (recur valley (set/difference next-positions wall right left up down) paths)))))

(def start (apply (partial min-key second) (:open valley)))
(def end (apply (partial max-key second) (:open valley)))
(def path
  (let [[p0 valley] (traverse valley start end)
        [p1 valley] (traverse valley end start)
        [p2 _] (traverse valley start end)]
    (concat p0 p1 p2)))

(def valleys
  (take (count path) (iterate solution/update-valley valley)))

(defn draw [canvas _ ^long frameno _]
  (c2d/set-background canvas colors/aoc-background)
  (c2d/set-font canvas "Iosevka")
  (c2d/set-font-attributes canvas 20.)
  (let [loop (* 30 60)
        frame (inc (mod frameno loop))
        p (* (count path) (/ frame loop))
        {:keys [right left up down]} (nth valleys p (last valleys))
        path (take (inc p) path)
        cx 9
        cy 12
        w  8
        wb 3
        dx 45
        dy 270]

    (c2d/set-color canvas (color/set-alpha colors/aoc-gold 60))
    (doseq [[x y] (set path)]
      (c2d/ellipse canvas (+ dx (* cx x)) (+ dy (* cy y)) w w))

    (c2d/set-color canvas colors/aoc-light-blue)
    (doseq [[x y] (concat right left up down)]
      (c2d/ellipse canvas (+ dx (* cx x)) (+ dy (* cy y)) wb wb))

    (c2d/set-color canvas colors/aoc-gold)
    (let [[x y] (last path)]
      (c2d/ellipse canvas (+ dx (* cx x)) (+ dy (* cy y)) w w))

    (c2d/set-color canvas colors/aoc-brown)
    (doseq [[x y] [start end]]
      (c2d/crect canvas (+ dx (* cx x)) (+ dy (* cy y)) w w))

    #_(c2d/save-image (c2d/get-image canvas) (format "images/%03d.png" frame))))

(comment
  (c2d/show-window
   {:canvas (c2d/canvas 1000 1000 :highest)
    :window-name (str *ns*)
    :hint :highest
    :fps 30
    :draw-fn #'draw}))
