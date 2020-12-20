(ns day20
  (:require [clojure.string :as str]))

(defn read-tiles [input]
  (for [tile (str/split input #"\R\R")
        :let [[title & pixels] (str/split-lines tile)
              id (read-string (first (re-seq #"\d+" title)))]]
    {:id id
     :pixels (map vec pixels)}))

(defn vflip [pixels]
  (reverse pixels))

(def rot-cw
  (memoize
   (fn [pixels]
     (vec
      (for [i (range (count pixels))]
        (->> pixels (map #(nth % i)) reverse vec))))))

(defn all-orientations [pixels]
  (->> [pixels (vflip pixels)]
       (mapcat #(take 4 (iterate rot-cw %)))))

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

(def opposite
  {:top :bottom
   :bottom :top
   :left :right
   :right :left})

(defn open-cells [grid]
  (for [[pos cell] grid
        [dir np] (neighbours pos)
        :when (not (grid np))]
    [np (opposite dir) (edge dir (:pixels cell))]))

(defn try-tile [grid tile]
  (if (empty? grid)
    (assoc grid [0 0] tile)
    (first
     (for [[pos dir open-edge] (open-cells grid)
           pixels (all-orientations (:pixels tile))
           :when (= open-edge (edge dir pixels))]
       (assoc grid pos (assoc tile :pixels pixels))))))

(defn assemble [tiles]
  (loop [queue (vec tiles)
         grid {}]
    (if (empty? queue)
      grid
      (let [tile (first queue)
            queue (subvec queue 1)
            new-grid (try-tile grid tile)]
        (if new-grid
          (recur queue new-grid)
          (recur (conj queue tile) grid))))))

(defn- glue-image [grid]
  (let [side (int (Math/sqrt (count grid)))
        without-borders (fn [pixels]
                          (->> pixels
                               rest butlast
                               (map #(->> % rest butlast vec))))]
    (->> grid (sort-by first)
         (map (comp without-borders :pixels second))
         (partition side)
         (map (partial reduce concat))
         (reduce (partial mapv (comp vec concat))))))

(def monster
  (let [pic ["                  # "
             "#    ##    ##    ###"
             " #  #  #  #  #  #   "]]
    {:width (count (first pic))
     :height (count pic)
     :pixels (mapv (partial keep-indexed #(when (= \# %2) %1)) pic)}))

(defn- has-monster [image [x y]]
  (->> (for [[j mxs] (zipmap (range) (:pixels monster))
             i mxs]
         (-> image (nth (+ y j)) (nth (+ x i))))
       (every? #{\#})))

(defn- hightlight-monster [image [x y]]
  (->> image
       (map-indexed
        (fn [j line]
          (if (<= y j (+ y 2))
            (->> (get (:pixels monster) (- j y))
                 (reduce (fn [line mx]
                           (assoc line (+ x mx) \O))
                         line))
            line)))
       vec))

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
  (let [grid (assemble (read-tiles input))
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
  (let [grid (assemble (read-tiles input))
        image (glue-image grid)]
    (first
     (for [image (all-orientations image)
           :let [with-monsters (highlight-monsters image)]
           :when (not= image with-monsters)]
       (->> with-monsters (mapcat #(filter #{\#} %)) count)))))
