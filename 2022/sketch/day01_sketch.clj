(ns day01-sketch
  (:require
   [clojure.java.io :as io]
   [clojure2d.core :as c2d]
   [clojure2d.color :as color]
   [fastmath.core :as math]
   [colors :as colors]
   [day01 :refer [parse-inventory]]))

(def my-input
  (slurp (io/resource "input01.txt")))

(def inventory (parse-inventory my-input))

(defn top-vals [n]
  (->> inventory
       (map #(reduce + %))
       (sort >)
       (take n)
       set))

(def item-gradient
  (color/gradient [(color/set-alpha colors/aoc-silver 150) colors/aoc-silver]))

(def max-items (apply max (map count inventory)))

(def steps
  (let [inv (->> inventory
                 (mapv (fn [items]
                         (->> (for [n (range (count items))]
                                {:value (get (vec items) n)
                                 :color (item-gradient (double (/ n max-items)))})
                              (reduce
                               (fn [result item]
                                 (let [{:keys [mx]} (peek result)
                                       mx (or mx 0)]
                                   (conj result (assoc item
                                                       :mx (+ (:value item) mx)
                                                       :mn mx))))
                               [])))))]
    (-> []
        (into (for [n (range (inc max-items))]
                (mapv #(->> % (take n) vec) inv)))
        (into (for [n (range 4)
                    _ (range 5)]
                (let [top? (top-vals n)]
                  (mapv (fn [items]
                          (let [{:keys [mx]} (peek items)
                                mx (or mx 0)]
                            (if (top? mx)
                              (mapv #(assoc % :color (color/set-alpha colors/aoc-gold 200)) items)
                              items)))
                        inv)))))))

(defn draw [canvas _ ^long frameno _]
  (c2d/set-background canvas colors/aoc-background)
  (let [loop (count steps)
        frame (mod frameno loop)
        inv (get steps frame)]
    (doseq [i (range (count inv))
            :let [callories (get inv i)]
            j (range (count callories))]
      (let [ymn (math/ceil (/ (get-in callories [j :mn]) 75.))
            ymx (math/floor (/ (get-in callories [j :mx]) 75.))
            x (* i 4.0)]
        (c2d/set-color canvas (get-in callories [j :color]))
        (c2d/line canvas x (- 1000 ymn) x (- 1000 ymx))))
    #_(c2d/save-image (c2d/get-image canvas) (format "images/%03d.png" frame))))

(comment
  (c2d/show-window
   {:canvas (c2d/canvas 1000 1000 :highest)
    :window-name (str *ns*)
    :hint :highest
    :fps 5
    :draw-fn #'draw}))
