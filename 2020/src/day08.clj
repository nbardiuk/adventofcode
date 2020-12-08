(ns day08
  (:require [clojure.string :as str]))

(defn read-instructions [input]
  (->> (str/split-lines input)
       (map #(str/split % #" "))
       (map #(update % 1 read-string))
       vec))

(defn loops [cmds]
  (loop [acc 0
         i 0
         seen #{}]
    (if (seen i)
      acc
      (let [[op v] (get cmds i)]
        (case op
          "acc" (recur (+ acc v) (inc i) (conj seen i))
          "jmp" (recur acc (+ i v) (conj seen i))
          "nop" (recur acc (inc i) (conj seen i)))))))

(defn part1 [input]
  (->> (read-instructions input)
       loops))

(defn flip [[op v :as cmd]]
  (case op
    "acc" cmd
    "jmp" ["nop" v]
    "nop" ["jmp" v]))

(defn all-flips [cmds]
  (->> (range)
       (map #(update cmds % flip))
       (take (count cmds))
       distinct))

(defn halts [cmds]
  (loop [acc 0
         i 0
         seen #{}]
    (cond
      (seen i) nil
      (< i (count cmds)) (let [[op v] (get cmds i)]
                           (case op
                             "acc" (recur (+ acc v) (inc i) (conj seen i))
                             "jmp" (recur acc (+ i v) (conj seen i))
                             "nop" (recur acc (inc i) (conj seen i))))
      :else acc)))

(defn part2 [input]
  (->> (read-instructions input)
       all-flips
       (some halts)))
