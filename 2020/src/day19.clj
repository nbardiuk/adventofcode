(ns day19
  (:require [clojure.string :as str]))

(defn- parse-input [input]
  (let [[grammar messages] (->> (str/split input #"\R\R") (map str/split-lines))]
    {:messages messages
     :grammar (->> (for [rule grammar
                         :let [[index rule] (str/split rule #": ")
                               index (read-string index)
                               rule (if (str/starts-with? rule "\"")
                                      (read-string rule)
                                      (mapv #(->> % (re-seq #"\d+") (mapv read-string))
                                            (str/split rule #"\|")))]]
                     [index rule])
                   (into {}))}))

(defn- match [grammar i s]
  (letfn [(one [i s]
            (let [rule (get grammar i)]
              (if (string? rule)
                (if (= (subs s 0 1) rule)
                  [true (subs s 1)]
                  [false s])
                (any-of rule s))))

          (any-of [alternation s]
            (or (->> alternation (map #(all-of % s)) (filter first) first)
                [false s]))

          (all-of [[i & concatenation] srest]
            (cond
              (nil? i) [true srest]
              (empty? srest) [false s]
              :else (let [[ok srest] (one i srest)]
                      (if ok
                        (recur concatenation srest)
                        [false s]))))]
    (one i s)))

(defn- full-match [grammar i s]
  (let [[ok srest]  (match grammar i s)]
    (and ok (empty? srest))))

(defn part1 [input]
  (let [{:keys [grammar messages]} (parse-input input)]
    (count (filter #(full-match grammar 0 %) messages))))

(defn part2 [input]
  (let [{:keys [grammar messages]} (parse-input input)
        matches (fn [matcher s]
                  (loop [n 0 s s]
                    (let [[ok srest] (matcher s)]
                      (if ok
                        (recur (inc n) srest)
                        [n srest]))))]
    (->> messages
         (filter
          #(let [[n42 srest] (matches (partial match grammar 42) %)
                 [n31 srest] (matches (partial match grammar 31) srest)]
             (and (< 0 n31 n42) (empty? srest))))
         count)))
