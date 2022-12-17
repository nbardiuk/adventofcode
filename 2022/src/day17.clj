(ns day17)

(def shapes
  [[:- [[0 0] [1 0] [2 0] [3 0]]]
   [:+ [#____ [1 2]
        [0 1] [1 1] [2 1]
        #____ [1 0]]]
   [:j [#____ #____ [2 2]
        #____ #____ [2 1]
        [0 0] [1 0] [2 0]]]
   [:i [[0 3]
        [0 2]
        [0 1]
        [0 0]]]
   [:o [[0 1] [1 1]
        [0 0] [1 0]]]])

(defn inside-walls? [[x y]]
  (and (<= 0 x 6)
       (<= 0 y)))

(defn freeze [cup [name points]]
  (into cup (map vector points (repeat name))))

(defn down [cup [name points :as shape]]
  (let [next (map #(update % 1 dec) points)]
    (if (every? (every-pred inside-walls? (comp not cup)) next)
      [cup [name next]]
      [(freeze cup shape) nil])))

(defn side [cup blow [name points :as shape]]
  (let [next (map #(update % 0 + blow) points)]
    (if (every? (every-pred inside-walls? (comp not cup)) next)
      [name next]
      shape)))

(defn initialize [[name points] height]
  [name (for [[x y] points] [(+ 2 x) (+ 4 height y)])])

(defn drop-shape
  [{:keys [cup height wind]} shape]
  (loop [cup cup
         [blow & wind] wind
         shape (initialize shape height)]
    (let [[cup shape] (->> shape (side cup blow) (down cup))]
      (if shape
        (recur cup wind shape)
        {:cup cup
         :height (->> cup keys (map second) (reduce max -1))
         :wind wind}))))

(defn find-cycle [xs]
  (let [cycled (drop 1000 xs)
        length (first
                (for [len (range 5 2000)
                      :let [[a b] (->> cycled (take (* 2 len)) (split-at len))]
                      :when (= a b)]
                  len))
        start (first
               (for [start (range 1000)
                     :let [[a b] (->> xs (drop start) (take (* 2 length)) (split-at length))]
                     :when (= a b)]
                 start))]
    [start length]))

(defn heights [input]
  (let [wind (keep {\> 1 \< -1} input)]
    (->> (cycle shapes)
         (reductions drop-shape {:cup {} :height -1 :wind (cycle wind)})
         (map :height))))

(defn part1 [input]
  (inc (nth (heights input) 2022)))

(defn part2 [input]
  (let [dh           (->> (heights input)
                          (partition 2 1)
                          (map (fn [[a b]] (- b a))))
        [start len]  (find-cycle dh)
        cycled       (- 1000000000000 start)
        rotation     (mod cycled len)
        cycles       (quot cycled len)
        init-height  (->> dh (take (+ rotation start)) (reduce + 0))
        cycle-height (->> dh (drop (+ rotation start)) (take len) (reduce + 0))]
    (+ init-height (* cycle-height cycles))))

