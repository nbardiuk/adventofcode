(ns day16
  (:require [clojure.string :as string]))

(defn hex->bin [input]
  (-> (for [c (string/trim input)]
        (format "%4s" (-> c str (Long/parseLong 16) Long/toBinaryString)))
      string/join
      (string/replace " " "0")))

(defn take-bits [bits length]
  [(subs bits 0 length)
   (subs bits length)])

(defn parse-bin [bits]
  (Long/parseLong bits 2))

(defn parse-bits [bits length]
  (let [[a bits] (take-bits bits length)]
    [(parse-bin a) bits]))

(declare parse-packet)

(defn parse-count-packets [n bits]
  (loop [result []
         bits bits]
    (if (= n (count result))
      [result bits]
      (let [[r bits] (parse-packet bits)]
        (recur (conj result r) bits)))))

(defn parse-length-packets [length bits]
  (let [keep-bits (- (count bits) length)]
    (loop [result []
           bits bits]
      (if (= keep-bits (count bits))
        [result bits]
        (let [[r bits] (parse-packet bits)]
          (recur (conj result r) bits))))))

(defn parse-packets [bits]
  (let [[length-type-id bits] (parse-bits bits 1)
        [parser length] (case length-type-id
                          0 [parse-length-packets 15]
                          1 [parse-count-packets 11])
        [n bits] (parse-bits bits length)]
    (parser n bits)))

(defn parse-literal [bits]
  (loop [groups ""
         bits bits]
    (let [[type bits] (parse-bits bits 1)
          [group bits] (take-bits bits 4)
          groups (str groups group)]
      (case type
        0 [(parse-bin groups) bits]
        1 (recur groups bits)))))

(defn parse-packet [bits]
  (let [[version bits] (parse-bits bits 3)
        [type-id bits] (parse-bits bits 3)
        [body bits]    (if (= 4 type-id)
                         (parse-literal bits)
                         (parse-packets bits))]
    [{:version version
      :type-id type-id
      :body body}
     bits]))

(defn sum-versions [{:keys [type-id version body]}]
  (case type-id
    4 version
    (->> body (map sum-versions) (reduce + version))))

(def bool->int {true 1 false 0})

(defn evaluate [{:keys [type-id body]}]
  (case type-id
    4 body
    0 (->> body (map evaluate) (reduce +))
    1 (->> body (map evaluate) (reduce *))
    2 (->> body (map evaluate) (reduce min))
    3 (->> body (map evaluate) (reduce max))
    5 (->> body (map evaluate) (reduce >) bool->int)
    6 (->> body (map evaluate) (reduce <) bool->int)
    7 (->> body (map evaluate) (reduce =) bool->int)))

(defn solution [evaluation input]
  (-> input hex->bin parse-packet first evaluation))

(def part1 (partial solution sum-versions))
(def part2 (partial solution evaluate))
