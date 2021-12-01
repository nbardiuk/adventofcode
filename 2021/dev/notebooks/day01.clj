(ns notebooks.day01
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]
   [nextjournal.clerk :as clerk]))

(def input (->> "input1.txt"
                io/resource
                slurp
                string/split-lines
                (map parse-long)))

(def pairs (->> input (partition 2 1)))

(clerk/plotly {:data [{:name "Changes"
                       :type "candlestick"
                       :low (map #(apply min %) pairs)
                       :high (map #(apply max %) pairs)
                       :open (map first pairs)
                       :close (map second pairs)}]})

(clerk/plotly {:data [{:name "Depth"
                       :type "scatter"
                       :mode "none"
                       :fill "tozeroy"
                       :y (->> pairs (map second))}
                      {:name "Increasing"
                       :type "scatter"
                       :mode "markers"
                       :y (->> pairs
                               (map (fn [[x y]] (when (< x y) y))))}]})
