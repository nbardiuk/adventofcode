(ns sketches.day05
  (:require [quil.core :as q]
            [clojure.java.io :as io]))

(def input (slurp (io/resource "input5.txt")))

(defn- parse-lines [input]
  (->> input
       (re-seq #"\d+")
       (map parse-long)
       (partition 2)
       (map vec)
       (partition 2)
       (mapv vec)))

(defn- range-inc [start end step]
  (range start (+ end step) step))

(defn- direction [start end]
  (if (<= start end) 1 -1))

(defn- diagonal? [[[ax ay] [bx by]]]
  (and (not= ax bx) (not= ay by)))

(defn- points [[[ax ay] [bx by] :as line]]
  (let [xrange (range-inc ax bx (direction ax bx))
        yrange (range-inc ay by (direction ay by))]
    (if (diagonal? line)
      (map vector xrange yrange)
      (for [x xrange, y yrange] [x y]))))

(def steps
  (->> (parse-lines input)
       (sort-by (fn [[[ax ay] [bx by]]] (if (or (= ax bx) (= ay by)) 0 1)))
       (reductions (fn [{:keys [lines cells]} line]
                     {:cells (->> line points frequencies
                                  (merge-with + cells))
                      :lines (conj lines line)})
                   {:lines [] :cells {}})
       (map #(update % :cells (fn [cells] (keep (fn [[k v]] (when (not= 1 v) k)) cells))))
       vec))

(def step (atom 0))

(defn setup []
  (reset! step 0))

(defn draw []
  (swap! step #(-> % (+ 1)))
  (when (<= (count steps) @step)
    (q/exit))

  (q/smooth)
  (q/color-mode :hsb 360 100 100 1.0)
  (q/background 240 57 13)

  (let [blue (q/color 193 100 100)
        gold (q/color 60 60 100)
        {:keys [cells lines]} (nth steps @step)]

    (doseq [[[ax ay] [bx by]] lines]
      (q/stroke blue)
      (q/line ax ay bx by))

    (doseq [[x y] cells]
      (q/set-pixel x y gold)))

  #_(q/save (format "images/day05-%03d.jpg" @step)))

(comment (q/defsketch day05
           :title "day05"
           :draw draw
           :setup setup
           :size [1000 1000]))
