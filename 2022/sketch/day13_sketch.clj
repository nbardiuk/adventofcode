(ns day13-sketch
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure2d.core :as c2d]
   [colors :as colors]
   [day13 :as solution]))

(defn enumerate [xs]
  (map vector (range) xs))

(def packets
  (solution/parse-packets
   (slurp (io/resource "example13.txt"))))

(defn pad [left right]
  (let [cl (count left)
        cr (count right)]
    [(into left  (repeat (- cr cl) {}))
     (into right (repeat (- cl cr) {}))]))

(defn normalize [left right]
  (cond
    (or (map? left) (map? right))      [left right]
    (and (number? left) (number? right)) [left right]
    (number? left)  (normalize [left] right)
    (number? right) (normalize left [right])
    (and (vector? left) (vector? right))
    (let [[left right] (pad left right)
          pairs (map normalize left right)]
      [(mapv first pairs)
       (mapv second pairs)])))

(defn width [e]
  (cond
    (number? e) 1
    (map? e) (:width e 1)
    :else (reduce + 2 (drop-last (interleave (map width e) (repeat 1))))))

(defn align [left right]
  (cond
    (and (map? left) (map? right))
    [{:width (max (width left) (width right))} {:width (max (width left) (width right))}]

    (map? left)
    [{:width (max (width left) (width right))} right]

    (map? right)
    [left {:width (max (width right) (width left))}]

    (and (number? left) (number? right))
    [left right]

    :else
    (let [pairs (map align left right)]
      [(mapv first pairs)
       (mapv second pairs)])))

(defn align-all [packets]
  (->> packets
       (reduce
        (fn [packets right]
          (let [right (reduce (fn [right left] (first (align right left))) right packets)]
            (conj (mapv (comp first align) packets (repeat right)) right)))
        [])))

(defn normalize-align [xs]
  (->> xs
       (reduce
        (fn [packets right]
          (let [right (reduce (fn [right left] (first (normalize right left))) right packets)]
            (align-all (conj (mapv (comp first normalize) packets (repeat right)) right))))
        [])))

(defn merge-groups [groups]
  (->> groups (partition 2) (map (fn [[a b]] (concat a b)))))

(defn iter-normalize [groups]
  (for [i (range (inc (count groups)))]
    (let [[tn r] (split-at i groups)]
      (concat (map normalize-align tn) r))))

(defn iter-sort [groups]
  (for [i (range (inc (count groups)))]
    (let [[tn r] (split-at i groups)]
      (concat (map #(sort solution/packet-compare %) tn) r))))

(def steps
  (let [s1 (->> packets (partition 2))
        s2 (iter-normalize s1)
        t2 (iter-sort (last s2))
        s3 (merge-groups (last t2))
        s4 (iter-normalize s3)
        t4 (iter-sort (last s4))
        s5 (merge-groups (last t4))
        s6 (iter-normalize s5)
        t6 (iter-sort (last s6))
        s7 (merge-groups (last t6))
        s8 (iter-normalize s7)
        t8 (iter-sort (last s8))]
    (-> [s1]
        (into s2)
        (into t2)
        (conj s3)
        (into s4)
        (into t4)
        (conj s5)
        (into s6)
        (into t6)
        (conj s7)
        (into s8)
        (into t8)
        distinct
        vec)))

(defn draw-packet [canvas packet x y]
  (cond
    (number? packet) (c2d/text canvas packet x y)
    (map? packet) (c2d/text canvas (str/join (repeat (width packet) " ")) x y)
    :else
    (do
      (c2d/text canvas (str "[" (str/join (repeat (- (width packet) 2) " ")) "]") x y)
      (loop [x (+ x (c2d/text-width canvas "["))
             [p & packet] packet]
        (when p
          (draw-packet canvas p x y)
          (recur (+ x (c2d/text-width canvas (str/join (repeat (inc (width p)) " "))))
                 packet))))))

(defn draw [canvas _ ^long frameno _]
  (c2d/set-background canvas colors/aoc-background)
  (c2d/set-font canvas "Iosevka")
  (c2d/set-font-attributes canvas 20.)
  (let [lp (* 30 10)
        frame (inc (mod frameno lp))
        ; frame 0
        lp (- lp (* 30 3))
        groups (get steps (long (* (/ frame lp) (count steps))) (peek steps))]

    (loop [[group & groups] groups
           y 100]
      (when group
        (let [c (count groups)
              ki (+ 28 (* 2 (- 8 c)))
              dv (+ 30 (* 3 (- 8 c)))]
          (doseq [[i p] (enumerate group)]
            (draw-packet canvas p 300 (+ y (* ki i))))
          (recur groups (+ y dv (* ki (count group)))))))

    #_(c2d/save-image (c2d/get-image canvas) (format "images/%03d.png" frame))))

(comment
  (c2d/show-window
   {:canvas (c2d/canvas 1000 1000 :highest)
    :window-name (str *ns*)
    :hint :highest
    :fps 20
    :draw-fn #'draw}))
