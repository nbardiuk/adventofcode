(ns day02-sketch
  (:require [clojure.java.io :as io]
    [clojure2d.core :as c2d]
    [day02 :as solution]))


(def games
  (delay (solution/parse-games (slurp (io/resource "input02.txt")))))


(defn draw [canvas _ ^long frameno _]
  (c2d/set-background canvas :white)
  (c2d/set-font canvas "Iosevka")
  (c2d/set-font-attributes canvas 20.)
  (let [w 30
        h 7
        c (/ 255. (->> @games (mapcat :draws) (mapcat vals) (reduce max 0)))]
    (doseq [{:keys [game draws]} @games
            d (range (count draws))
            :let [{:keys [red green blue]} (get draws d)]]

      (c2d/set-color canvas (* c (or red 0)) (* c (or green 0)) (* c (or blue 0)))
      (c2d/rect canvas (* d (inc w)) (* 9 game) w h))

    (doseq [{:keys [game draws]} @games
            :let [mx #(reduce max 0 (keep % draws))]]

      (c2d/set-color canvas (* c (mx :red)) (* c (mx :green)) (* c (mx :blue)))
      (c2d/rect canvas (* 8 (inc w)) (* 9 game) w h)))
  #_(c2d/save-image (c2d/get-image canvas) "renders/day02.png"))


(comment
  (c2d/show-window
    {:canvas (c2d/canvas 1000 1000 :highest)
     :window-name (str *ns*)
     :hint :highest
     :fps 1
     :draw-fn #'draw}))
