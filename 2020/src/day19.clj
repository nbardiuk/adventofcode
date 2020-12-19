(ns day19
  (:require [clojure.string :as str]))

(defn parse-input [input]
  (let [[rules messages] (->> (str/split input #"\R\R") (map str/split-lines))]
    {:messages messages
     :rules (->> (for [rule rules
                       :let [[index rule] (str/split rule #": ")
                             index (read-string index)
                             rule (if (str/starts-with? rule "\"")
                                    (read-string rule)
                                    (mapv #(->> % (re-seq #"\d+") (mapv read-string))
                                          (str/split rule #"\|")))]]
                   [index rule])
                 (into {}))}))

(defn examples [rules i]
  (let [rule (get rules i)]
    (if (string? rule)
      [rule]
      (mapcat #(reduce
                (fn [result i]
                  (mapcat (fn [e] (for [r result] (str r e)))
                          (examples rules i)))
                [""] %)
              rule))))

(defn part1 [input]
  (let [{:keys [rules messages]} (parse-input input)
        valid? (set (examples rules 0))]
    (count (filter valid? messages))))

(defn part2 [input]
  (let [{:keys [rules messages]} (parse-input input)
        valid42? (set (examples rules 42))
        valid31? (set (examples rules 31))
        len (count (first valid42?))]
    (count
     (for [message messages
           :let [parts (->> message (partition len) (map str/join))
                 [matches42 matches31] (split-with valid42? parts)]
           :when (and (every? valid31? matches31)
                      (< 0 (count matches31) (count matches42)))]
       message))))
