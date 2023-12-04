(ns day03
  (:require
    [clojure.string :as str]))


(defn enumerate [xs]
  (map vector (range) xs))


(defn category [c]
  (cond
    (= \. c) :empty
    (Character/isDigit c) :digit
    :else :symbol))


(defn- parse-input [input]
  (for [[y line] (enumerate (str/split-lines input))
        p (->> (enumerate line)
               (partition-by (comp category second))
               (mapv (fn [p]
                       (let [category (category (second (first p)))
                             value (str/join (mapv second p))]
                         {:pos (mapv vector (mapv first p) (repeat y))
                          :value (if (= :digit category) (parse-long value) value)
                          :category category}))))]
    p))


(defn neighbours [[x y]]
  (for [dx [-1 0 1]
        dy [-1 0 1]
        :when (not= 0 dx dy)]
    [(+ x dx) (+ y dy)]))


(defn part1 [input]
  (let [parsed (parse-input input)
        digits (->> parsed
                    (filter (comp #{:digit} :category))
                    (mapv #(assoc % :ns (set (mapcat neighbours (:pos %))))))
        symbol-pos (->> parsed
                        (filter (comp #{:symbol} :category))
                        (mapv (comp first :pos)))]
    (apply + (for [d digits
                   :when (some #(contains? (:ns d) %) symbol-pos)]
               (:value d)))))


(defn part2 [input]
  (let [parsed (parse-input input)
        digits (->> parsed
                    (filter (comp #{:digit} :category))
                    (mapv #(assoc % :ns (set (mapcat neighbours (:pos %))))))
        symbol-pos (->> parsed
                        (filter (comp #{:symbol} :category))
                        (filter (comp #{"*"} :value))
                        (mapv (comp first :pos)))]
    (apply + (for [pos symbol-pos
                   :let [digits (filterv #((:ns %) pos) digits)]
                   :when (= 2 (count digits))]
               (->> digits (map :value) (reduce *))))))
