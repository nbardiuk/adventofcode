(ns day10
  (:require [clojure.string :as string]))

(def config
  {\( {:closing \)}
   \{ {:closing \}}
   \[ {:closing \]}
   \< {:closing \>}
   \) {:open \( :unexpected 3     :completion 1}
   \] {:open \[ :unexpected 57    :completion 2}
   \} {:open \{ :unexpected 1197  :completion 3}
   \> {:open \< :unexpected 25137 :completion 4}})

(defn analyze [brackets]
  (loop [[bracket & brackets] brackets
         open []]
    (if bracket
      (if-let [open-bracket ((comp :open config) bracket)]
        (if (= open-bracket (peek open))
          (recur brackets (pop open))
          {:unexpected [bracket]})
        (recur brackets (conj open bracket)))
      {:completion (->> open rseq (map (comp :closing config)))})))

(defn score [score-name brackets]
  (reduce (fn [result bracket]
            (+ (* result 5)
               ((comp score-name config) bracket)))
          0 brackets))

(defn- middle [xs]
  (nth (sort xs) (/ (count xs) 2)))

(defn part1 [input]
  (->> (string/split-lines input)
       (keep (comp :unexpected analyze))
       (map (partial score :unexpected))
       (reduce +)))

(defn part2 [input]
  (->> (string/split-lines input)
       (keep (comp :completion analyze))
       (map (partial score :completion))
       middle))
