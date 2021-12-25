(ns day25
  (:require [clojure.string :as string]
            [day00 :refer [take-until-repeat]]))

(defn parse [input]
  (let [lines (string/split-lines input)]
    (->> (for [y (range (count lines))
               :let [line (nth lines y)]
               x (range (count line))
               :let [direction ({\> :right \v :down} (nth line x))]
               :when direction]
           {direction [[x y]]})
         (reduce (partial merge-with into)
                 {:width (count (first lines))
                  :height (count lines)}))))

(defn occupied [{:keys [right down]}]
  (-> #{} (into right) (into down)))

(defn shift-right [{:as m :keys [width right]}]
  (let [occupied? (occupied m)]
    (assoc m :right
           (for [p right
                 :let [p' (update p 0 #(-> % inc (mod width)))]]
             (if (occupied? p') p p')))))

(defn shift-down [{:as m :keys [height down]}]
  (let [occupied? (occupied m)]
    (assoc m :down
           (for [p down
                 :let [p' (update p 1 #(-> % inc (mod height)))]]
             (if (occupied? p') p p')))))

(defn step [m]
  (-> m shift-right shift-down))

(defn part1 [input]
  (->> (parse input)
       (iterate step)
       take-until-repeat
       count))
