(ns day20
  (:require [clojure.string :as string]))

(defn parse [input]
  (let [[algo image] (string/split input #"\R\R")
        image (string/split-lines image)]
    {:algorithm (->> algo
                     (re-seq #"#|\.")
                     (keep-indexed #(when (= "#" %2) %1))
                     set)
     :image (set (for [y (range (count image))
                       :let [line (get image y)]
                       x (range (count line))
                       :when (= "#" (subs line x (inc x)))]
                   [x y]))}))

(defn range-inc [from to]
  (range from (inc to)))

(defn neighbours [[x y]]
  (for [dy (range-inc -1 1)
        dx (range-inc -1 1)]
    [(+ x dx) (+ y dy)]))

(defn boundaries [image]
  (reduce (fn [{:keys [min-x min-y max-x max-y]} [x y]]
            {:min-x (min min-x x)
             :max-x (max max-x x)
             :min-y (min min-y y)
             :max-y (max max-y y)})
          {:min-x Long/MAX_VALUE
           :min-y Long/MAX_VALUE
           :max-x Long/MIN_VALUE
           :max-y Long/MIN_VALUE}
          image))

(defn flip-infinity? [algorithm]
  (and (algorithm 2r000000000)
       (not (algorithm 2r111111111))))

(defn enhance [algorithm [negative-infinity? image]]
  (let [{:keys [min-x min-y max-x max-y]} (boundaries image)]
    [(if (flip-infinity? algorithm) (not negative-infinity?) negative-infinity?)
     (set (for [y (range-inc (- min-y 1) (+ max-y 1))
                x (range-inc (- min-x 1) (+ max-x 1))
                :when (->> (for [[x' y'] (neighbours [x y])]
                             (or (image [x' y'])
                                 (and (not negative-infinity?)
                                      (not (and (<= min-y y' max-y)
                                                (<= min-x x' max-x))))))
                           (reduce (fn [i b] (cond-> (bit-shift-left i 1) b inc)) 0)
                           algorithm)]
            [x y]))]))

(defn solution [times input]
  (let [{:keys [algorithm image]} (parse input)]
    (-> (iterate #(enhance algorithm %) [true image])
        (nth times)
        second
        count)))

(def part1 (partial solution 2))
(def part2 (partial solution 50))
