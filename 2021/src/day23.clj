(ns day23
  (:require [clojure.string :as string]
            [clojure.data.priority-map :refer [priority-map]]))

(def folded
  "  #D#C#B#A#
  #D#B#A#C#")

(defn parse [input]
  (let [lines (string/split-lines input)
        points (for [y (range (count lines))
                     :let [line (nth lines y)]
                     x (range (count line))
                     :let [c (keyword (subs line x (inc x)))]]
                 [x y c])]
    {:amphipods (into {} (for [[x y c] points :when (#{:A :B :C :D} c)] [[x y] c]))
     :grid (into #{} (for [[x y c] points :when (#{:. :A :B :C :D} c)] [x y]))}))

(def costs
  {:A 1 :B 10 :C 100 :D 1000})

(def column
  {:A 3 :B 5 :C 7 :D 9})

(def room-entrances
  (->> (for [x (vals column)] [x 1]) (into #{})))

(defn neighbours [grid [x y]]
  (for [[dx dy] [[0 -1] [-1 0] [1 0] [0 1]]
        :let [p [(+ x dx) (+ y dy)]]
        :when (grid p)]
    p))

(def goal
  (memoize
   (fn [grid]
     (let [max-y (->> grid (map second) (apply max))]
       (->> (for [[t c] column
                  r (range 2 (inc max-y))]
              [[c r] t])
            (into {}))))))

(def rooms
  (memoize
   (fn [grid t]
     (let [max-y (->> grid (map second) (apply max))]
       (set (map vector (repeat (column t)) (range 2 (inc max-y))))))))

(defn hallway? [[_x y]]
  (= 1 y))

(defn destinations [grid p occupied]
  ((fn lazy-result [seen? [[d p] & queue]]
     (lazy-seq
      (when p
        (let [ns (->> (neighbours grid p) (remove seen?) vec)
              rs (->> ns (map vector (repeat (inc d))))]
          (concat rs (lazy-result (into seen? ns) (concat queue rs)))))))
   occupied [[0 p]]))

(defn target-room? [grid t p amphipods]
  (let [room (rooms grid t)]
    (and (room p) (->> room (keep amphipods) (every? #{t})))))

(defn push-queue [queue [k v]]
  (let [old-v (get queue k v)]
    (if (neg? (compare old-v v))
      queue
      (assoc queue k v))))

(def est-left
  (memoize
   (fn [amphipods]
     (->> (for [[[x y] t] amphipods]
            (* (costs t) (+ (Math/abs (- x (column t))) (Math/abs (- y 5)))))
          (reduce +)))))

(defn part1 [input]
  (let [{:keys [grid amphipods]} (parse input)]
    (loop [seen? #{}
           queue (priority-map amphipods [0 0])]
      (let [[amphipods [_ cost]] (peek queue)
            queue (pop queue)
            seen? (conj seen? amphipods)]

        (if (= amphipods (goal grid))
          cost
          (let [occupied (set (keys amphipods))]
            (recur seen?
                   (->> (for [[p t] amphipods
                              ;; don't exit target room
                              :when (not (target-room? grid t p amphipods))

                              [d n] (destinations grid p occupied)

                              ;; don't block room entrance
                              :when (not (room-entrances n))

                              :when (or
                                      ;; don't wander around halway
                                     (and (not (hallway? p)) (hallway? n))
                                      ;; enter only target room
                                     (target-room? grid t n amphipods))

                              :let [amphipods (-> amphipods (dissoc p) (assoc n t))
                                    cost (+ cost (* d (costs t)))]
                              :when (not (seen? amphipods))]
                          [amphipods [(+ cost (est-left amphipods)) cost]])
                        (reduce push-queue queue)))))))))

(defn part2 [input]
  (let [[before after] (split-at 3 (string/split-lines input))]
    (part1 (string/join "\n" (concat before [folded] after)))))
