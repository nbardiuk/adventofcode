(ns sketches.day14
  (:require [clojure2d.core :as c2d]
            [clojure2d.color :as color]
            [day14 :refer [parse-input]]
            [fastmath.core :as math]
            [day14-test :refer [my-input]]))

(def input my-input)

(def steps
  (let [{:keys [insertion polymer]} (parse-input input)]
    (->> [(for [element polymer]
            {:element element
             :scale 1.0})]
         (iterate (fn [polymers]
                    (let [polymer (last polymers)
                          pairs (partition 2 1 polymer)
                          triples (vec
                                   (for [[a b] pairs
                                         :let [[_ c _] (insertion [(:element a) (:element b)])]]
                                     [a {:element c :scale 1.0} b]))]
                      (vec
                       (for [scale [0.15 0.3 0.45 0.55 0.63 0.72 0.8 0.85 0.9 0.95 1.0]]
                         (->> (map #(assoc-in % [1 :scale] scale) triples)
                              (reduce #(into %1 (rest %2)))))))))
         (mapcat identity)
         (take 98))))

(def colors
  (let [gradient (color/gradient :rainbow2 #_["#ffff66" "#00c8ff"])
        names (apply str (set  (sort (re-seq #"\w" input))))]
    (->> (map #(vector %1 (gradient %2)) names (range 0 1 (/ 1.0 (count names))))
         (into {}))))

(defn draw [canvas _ ^long frameno _]
  (c2d/set-background canvas "#0f0f23")
  (let [index (mod frameno (count steps))
        polymer (nth steps index)
        cx (/ (c2d/width canvas) 2)
        cy cx
        size (/ cx (math/sqrt (->> polymer (map :scale) (reduce +))))]
    (doseq [i (range (count polymer))
            :let [p (->> polymer (take i) (map :scale) (reduce +))
                  {:keys [element scale]} (nth polymer i)
                  th (* math/PI (math/sqrt p))
                  r (* size (math/sqrt p))
                  x (+ cx (* r (math/cos th)))
                  y (+ cy (* r (math/sin th)))]]
      (c2d/set-color canvas (colors element))
      (c2d/ellipse canvas x y (* size scale) (* size scale)))
    #_(c2d/save-image (c2d/get-image canvas) (format "images/day14-%03d.jpg" index))))

(comment
  (c2d/show-window
   {:canvas (c2d/canvas 1500 1500 :highest)
    :window-name "day14"
    :hint :highest
    :fps 5
    :h 700
    :w 700
    :draw-fn #'draw}))

