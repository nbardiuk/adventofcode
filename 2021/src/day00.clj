(ns day00)

(defn fix-point [f x]
  (let [x' (f x)]
    (if (= x x')
      x
      (recur f x'))))

(defn take-until-repeat [xs]
  (->> (partition 2 1 xs)
       (take-while #(apply not= %))
       (map second)
       (cons (first xs))))

(defn tails [xs]
  (->> (iterate rest xs)
       (take-while seq)))
