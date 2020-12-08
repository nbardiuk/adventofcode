(ns day08
  (:require [clojure.string :as str]))

(defn read-program [input]
  (->> (str/split-lines input)
       (map #(str/split % #" "))
       (mapv (fn [[op arg]]
               [(keyword op) (read-string arg)]))))

(defn flip-seq [program]
  (letfn [(flip [i [op]]
            (when-let [fop ({:nop :jmp :jmp :nop} op)]
              (assoc-in program [i 0] fop)))]
    (keep-indexed flip program)))

(defn execute [program]
  (loop [accumulator 0
         counter 0
         seen #{}]
    (cond
      (seen counter) {:loops accumulator}
      (= counter (count program)) {:halts accumulator}
      :else (let [[op arg] (get program counter)
                  seen (conj seen counter)]
              (case op
                :acc (recur (+ arg accumulator) (inc counter) seen)
                :jmp (recur accumulator (+ arg counter) seen)
                :nop (recur accumulator (inc counter) seen))))))

(defn part1 [input]
  (->> (read-program input)
       execute
       :loops))

(defn part2 [input]
  (->> (read-program input)
       flip-seq
       (map execute)
       (some :halts)))
