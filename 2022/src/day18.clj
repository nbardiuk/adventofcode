(ns day18)

(defn parse-points [input]
  (->> (re-seq #"\d+" input)
       (map parse-long)
       (partition 3)
       (map vec)))

(defn neighbours [[x y z]]
  (for [dx [-1 0 1] dy [-1 0 1] dz [-1 0 1]
        :when (= 1 (+ (abs dx) (abs dy) (abs dz)))]
    [(+ x dx) (+ y dy) (+ z dz)]))

(defn inside-box? [[mn mx] p]
  (every? true? (map <= mn p mx)))

(defn flood [[mn :as box] wall?]
  (loop [[point & queue] [mn]
         water           #{mn}]
    (if (nil? point)
      water
      (let [obstacle?  (some-fn water wall? #(not (inside-box? box %)))
            neighbours (remove obstacle? (neighbours point))]
        (recur (into queue neighbours) (into water neighbours))))))

(defn surface [points]
  (->> points
       (mapcat neighbours)
       (remove (set points))
       count))

(defn bounding-box [points]
  (let [axis (fn [i] (map #(get % i) points))]
    [(mapv #(dec (apply min (axis %))) (range 3))
     (mapv #(inc (apply max (axis %))) (range 3))]))

(defn box-surface [[mn mx]]
  (let [[w h d] (map #(inc (- %1 %2)) mx mn)]
    (* 2 (+ (* w d) (* w h) (* d h)))))

(defn part1 [input]
  (surface (parse-points input)))

(defn part2 [input]
  (let [points (parse-points input)
        box    (bounding-box points)
        water  (flood box (set points))]
    (- (surface water) (box-surface box))))
