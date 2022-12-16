(ns day16-sketch
  (:require
   [clojure.java.io :as io]
   [clojure.set :as set]
   [clojure2d.core :as c2d]
   [fastmath.core :as math]
   [colors :as colors]
   [day16 :as solution]))

(def graph (solution/parse-graph (slurp (io/resource "input16.txt"))))
(def closed-valves (->> graph (keep (fn [[v {:keys [rate]}]] (when (< 0 rate) v))) set))
(def state (solution/release-valves graph closed-valves [:AA :AA] [26 26]))
(def closed-idx (into {} (map vector closed-valves (range))))
(def open-idx (into {} (map vector (->> graph keys (remove closed-valves)) (range))))

(def path1
  (->> (first (:paths state))
       (drop-last 1)
       (partition 2 1)
       (mapcat #(apply solution/path graph %))))

(def path2
  (->> (second (:paths state))
       (drop-last 1)
       (partition 2 1)
       (mapcat #(apply solution/path graph %))))

(def pressures
  (let [a (->> (first (:paths state))
               (partition 2 1)
               (mapcat #(apply solution/path graph %))
               (take 25)
               (reductions (fn [open valve]
                             (conj open valve))
                           #{}))
        b (->> (second (:paths state))
               (partition 2 1)
               (mapcat #(apply solution/path graph %))
               (take 25)
               (reductions (fn [open valve]
                             (conj open valve))
                           #{}))]
    (reductions (fn [score open]
                  (reduce + score (map #(:rate (graph %)) open)))
                0 (map set/union a b))))

(defn draw [canvas _ ^long frameno _]
  (c2d/set-background canvas colors/aoc-background)
  (c2d/set-font canvas "Iosevka")
  (c2d/set-font-attributes canvas 18.)
  (let [loop (* 30 10)
        frame (inc (mod frameno loop))
        n (min 26 (long (* 26 (/ frame (- loop (* 30 2))))))
        current1 (nth path1 n (last path1))
        current2 (nth path2 n (last path2))
        paths1 (->> (take (inc n) path1)
                    (partition 2 1))
        paths2 (->> (take (inc n) path2)
                    (partition 2 1))
        connected (->> (concat paths1 paths2)
                       (filter (fn [[a b]] (closed-idx b)))
                       (map second)
                       set)
        pressure (nth pressures n (last pressures))

        o (count open-idx)
        i (count closed-idx)
        dopen (/ math/TWO_PI o)
        di (/ math/TWO_PI i)
        s 30
        ty 100
        sa (- (* math/TWO_PI (/ frame loop)))
        open-x #(+ 500 (* 440 (math/cos (+ sa (* dopen %)))))
        open-y #(+ 800 (* 110 (math/sin (+ sa (* dopen %)))))
        closed-x #(+ 500 (* 200 (math/cos (+ sa (* di %)))))
        closed-y #(+ 410 (* 50 (math/sin (+ sa (* di %)))))
        fx #(or (some-> (closed-idx %) closed-x) (some-> (open-idx %) open-x))
        fy #(or (some-> (closed-idx %) closed-y) (some-> (open-idx %) open-y))]

    (c2d/set-stroke canvas 1)
    (doseq [[a b] (concat paths1 paths2)]
      (c2d/set-color canvas colors/aoc-dark-green)
      (c2d/line canvas (fx a) (fy a) (fx b) (fy b)))
    (doseq [[a b] (concat (drop (dec n) paths1) (drop (dec n) paths2))]
      (c2d/set-color canvas colors/aoc-gold)
      (c2d/line canvas (fx a) (fy a) (fx b) (fy b)))

    (c2d/set-color canvas colors/aoc-red)
    (doseq [c connected]
      (c2d/line canvas (fx c) (fy c) 500 ty))
    (c2d/ellipse canvas 500 ty s s)
    (c2d/set-color canvas colors/aoc-red)
    (c2d/text canvas (str "Released pressure: " pressure) 250 (+ ty 5))
    (c2d/set-color canvas colors/aoc-dark-green)
    (c2d/text canvas (str "Time: " n) 250 (- ty 25))

    (doseq [[key id] open-idx]
      (c2d/set-color canvas colors/aoc-brown)
      (c2d/ellipse canvas (open-x id) (open-y id) s s)
      (c2d/set-color canvas colors/aoc-grey)
      (c2d/text canvas (name key) (- (open-x id) 10) (+ (open-y id) 6)))

    (doseq [[key id] closed-idx]
      (c2d/set-color canvas colors/aoc-brown)
      (c2d/ellipse canvas (closed-x id) (closed-y id) s s)
      (c2d/set-color canvas colors/aoc-grey)
      (c2d/text canvas (name key) (- (closed-x id) 10) (+ (closed-y id) 6)))

    (c2d/set-color canvas colors/aoc-gold)
    (c2d/set-stroke canvas 2)
    (doseq [c [current1 current2]]
      (c2d/ellipse canvas (fx c) (fy c) s s true))

    #_(c2d/save-image (c2d/get-image canvas) (format "images/%03d.png" frame))))

(comment
  (c2d/show-window
   {:canvas (c2d/canvas 1000 1000 :highest)
    :window-name (str *ns*)
    :hint :highest
    :fps 30
    :draw-fn #'draw}))
