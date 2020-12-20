(ns day20
  (:require [clojure.string :as str]))

(defn tiles [input]
  (for [tile (str/split input #"\R\R")
        :let [[title & pixels] (str/split-lines tile)
              id (read-string (first (re-seq #"\d+" title)))]]
    {:id id
     :pixels pixels}))

(defn vflip [pixels]
  (reverse pixels))

(defn rot-cw [pixels]
  (for [i (range (count pixels))]
    (str/join (reverse (map #(get % i) pixels)))))

(defn all-positions [pixels]
  (concat (take 4 (iterate rot-cw pixels))
          (take 4 (iterate rot-cw (vflip pixels)))))

(defn edge [side pixels]
  (case side
    :top (first pixels)
    :right (last (rot-cw pixels))
    :bottom (last pixels)
    :left (first (rot-cw pixels))))

(defn neighbours [[x y]]
  {:top [x (- y 1)]
   :bottom [x (+ y 1)]
   :left [(- x 1) y]
   :right [(+ x 1) y]})

(def opposite-side
  {:top :bottom
   :bottom :top
   :left :right
   :right :left})

(defn open-edges [grid]
  (for [[pos :as cell] grid
        [dir np] (neighbours pos)
        :when (not (grid np))]
    [dir cell]))

(defn try-tile [grid tile]
  (if (empty? grid)
    (assoc grid [0 0] tile)
    (first
     (for [[dir [cell-pos cell-tile]] (open-edges grid)
           :let [open-edge (edge dir (:pixels cell-tile))]
           pixels (all-positions (:pixels tile))
           :let [tile-edge (edge (opposite-side dir) pixels)]
           :when (= open-edge tile-edge)]
       (assoc grid
              ((neighbours cell-pos) dir)
              (assoc tile :pixels pixels))))))

(defn assemble [tiles]
  (loop [[tile & queue] tiles
         grid {}]
    (if (nil? tile)
      grid
      (if-let [new-grid (try-tile grid tile)]
        (recur queue new-grid)
        (recur (conj (vec queue) tile) grid)))))

(defn- glue-image [grid]
  (let [side (int (Math/sqrt (count grid)))
        without-borders (fn [pixels]
                          (->> pixels
                               rest butlast
                               (map #(subs % 1 (dec (count %))))))]
    (->> grid (sort-by first)
         (map (comp without-borders :pixels second))
         (partition side)
         (map (partial reduce concat))
         (reduce (partial map str)))))

(def monster
  {:width 20
   :height 3
   :pixels [[#_.                            18]
            [#_. 0    5 6     11 12      17 18 19]
            [#_.  1  4   7  10     13  16]]})

(defn- has-monster [image [x y]]
  (->> (for [[j mxs] (zipmap (range) (:pixels monster))
             i mxs]
         (-> image (nth (+ y j)) (get (+ x i))))
       (every? #{\#})))

(defn- hightlight-monster [image [x y]]
  (->> image
       (map-indexed
        (fn [j line]
          (if (<= y j (+ y 2))
            (->> (get (:pixels monster) (- j y))
                 (reduce (fn [line mx]
                           (str (subs line 0 (+ x mx)) "O" (subs line (+ x mx 1))))
                         line))
            line)))))

(defn highlight-monsters [image]
  (->> (for [y (range (- (count image) (:height monster)))
             x (range (- (count image) (:width monster)))]
         [x y])
       (reduce
        (fn [image p]
          (if (has-monster image p)
            (hightlight-monster image p)
            image))
        image)))

(defn part1 [input]
  (let [grid (assemble (tiles input))
        side (int (Math/sqrt (count grid)))
        ids (->> grid
                 (sort-by first)
                 (map (comp :id second))
                 (partition side)
                 (mapv vec))]
    (* (first (peek ids))
       (first (first ids))
       (peek (peek ids))
       (peek (first ids)))))

(defn part2 [input]
  (let [grid (assemble (tiles input))
        image (glue-image grid)]
    (first
     (for [image (all-positions image)
           :let [with-monsters (highlight-monsters image)]
           :when (not= image with-monsters)]
       (->> with-monsters (mapcat #(re-seq #"#" %)) count)))))
