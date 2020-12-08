(ns day08
  (:require [clojure.string :as str]))

(defn read-instructions [input]
  (->> (str/split-lines input)
       (map #(str/split % #" "))
       (map #(update % 1 read-string))
       vec))

(defn flip [[op arg :as instruction]]
  (case op
    "acc" instruction
    "jmp" ["nop" arg]
    "nop" ["jmp" arg]))

(defn flip-seq [instructions]
  (->> (range)
       (map #(update instructions % flip))
       (take (count instructions))
       distinct))

(defn execute [instructions]
  (loop [acc 0
         i 0
         seen #{}]
    (cond
      (seen i) {:loops acc}
      (<= (count instructions) i) {:halts acc}
      :else (let [[op arg] (get instructions i)]
              (case op
                "acc" (recur (+ arg acc) (inc i) (conj seen i))
                "jmp" (recur acc (+ arg i) (conj seen i))
                "nop" (recur acc (inc i) (conj seen i)))))))

(defn part1 [input]
  (->> (read-instructions input)
       execute
       :loops))

(defn part2 [input]
  (->> (read-instructions input)
       flip-seq
       (map execute)
       (some :halts)))
