(ns day20)

(defn parse-longs [input]
  (map parse-long (re-seq #"-?\d+" input)))

(defn index-of [x xs]
  (first (keep-indexed (fn [i v] (when (= x v) i)) xs)))

(defn move-left [v from to]
  (-> []
      (into (subvec v 0 to))
      (conj (get v from))
      (into (subvec v to from))
      (into (subvec v (inc from)))))

(defn move-right [v from to]
  (-> []
      (into (subvec v 0 from))
      (into (subvec v (inc from) (inc to)))
      (conj (get v from))
      (into (subvec v (inc to)))))

(defn mix [xs [_i n :as x]]
  (let [from (index-of x xs)
        to   (mod (+ from n) (dec (count xs)))
        move (if (< from to) move-right move-left)]
    (move xs from to)))

(defn concat-times [n xs]
  (apply concat (repeat n xs)))

(defn decrypt [decr-key times xs]
  (let [indexed (map-indexed (fn [i n] [i (* decr-key n)]) xs)]
    (->> (concat-times times indexed)
         (reduce mix (vec indexed))
         (mapv second))))

(defn solution [decr-key times input]
  (let [numbers   (parse-longs input)
        decrypted (decrypt decr-key times numbers)
        iz        (index-of 0 decrypted)]
    (->> (for [g [1000 2000 3000]
               :let [i (mod (+ iz g) (count decrypted))]]
           (get decrypted i))
         (reduce +))))

(defn part1 [input]
  (solution 1 1 input))

(defn part2 [input]
  (solution 811589153 10 input))
