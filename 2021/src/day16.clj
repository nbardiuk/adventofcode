(ns day16
  (:require [clojure.string :as string]))

(defn hex->bin [input]
  (-> (for [c (string/trim input)]
        (format "%4s" (-> c str (Long/parseLong 16) Long/toBinaryString)))
      string/join
      (string/replace " " "0")))

(defn take-bits [bits length]
  [(subs bits 0 length) (subs bits length)])

(defn parse-bits [bits length]
  (let [[a bits] (take-bits bits length)]
    [(Long/parseLong a 2) bits]))

(declare parse-packet)

(defn parse-count-packets [n bits]
  (loop [packets []
         bits bits]
    (if (= n (count packets))
      [packets bits]
      (let [[packet bits] (parse-packet bits)]
        (recur (conj packets packet) bits)))))

(defn parse-length-packets [length bits]
  (let [[bits rest] (take-bits bits length)]
    (loop [packets []
           bits bits]
      (if (zero? (count bits))
        [packets rest]
        (let [[packet bits] (parse-packet bits)]
          (recur (conj packets packet) bits))))))

(defn parse-packets [bits]
  (let [[length-type-id bits] (parse-bits bits 1)
        [packets-parser length] (case length-type-id
                                  0 [parse-length-packets 15]
                                  1 [parse-count-packets 11])
        [n bits] (parse-bits bits length)]
    (packets-parser n bits)))

(defn parse-literal [bits]
  (loop [value 0
         bits bits]
    (let [[flag bits] (parse-bits bits 1)
          [chunk bits] (parse-bits bits 4)
          value (+ chunk (bit-shift-left value 4))]
      (case flag
        0 [value bits]
        1 (recur value bits)))))

(defn parse-packet [bits]
  (let [[version bits] (parse-bits bits 3)
        [type-id bits] (parse-bits bits 3)
        body-parser (if (= 4 type-id) parse-literal parse-packets)
        [body bits] (body-parser bits)]
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
