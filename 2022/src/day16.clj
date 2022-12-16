(ns day16
  (:require [clojure.string :as str])
  (:import [java.util PriorityQueue]))

(defn parse-graph [input]
  (->> (for [line (str/split-lines input)]
         (let [rate (parse-long (re-find #"\d+" line))
               [valve & neighbours] (map keyword (re-seq #"[A-Z]{2}" line))]
           [valve {:rate rate
                   :neighbours (vec neighbours)}]))
       (into {})))

(defn path [graph a b]
  (loop [queue [[a [a]]]
         seen  #{a}]
    (let [[[c path] & queue] queue]
      (cond
        (not c) []
        (= b c) path
        :else   (let [ns (->> graph c :neighbours (remove seen))
                      nx (for [n ns] [n (conj path n)])]
                  (recur (concat queue nx)
                         (into seen ns)))))))

(defn distances-between [graph valves]
  (->> (for [a valves
             b valves
             :let [p (path graph a b)]
             :when (seq p)]
         {a {b (count p)}})
       (reduce (partial merge-with merge) {})))

(defn index-of-max [xs]
  (->> (map vector xs (range))
       (reduce (partial max-key first))
       second))

(defn release-valves [graph closed start times]
  (let [distances (distances-between graph (into closed start))
        queue (new PriorityQueue 10000 (comparator (fn [a b] (> (:priority a) (:priority b)))))]
    (.add queue {:positions start
                 :paths     (mapv vector start)
                 :times     times
                 :closed    closed
                 :preasure  0
                 :priority  0})
    (loop [seen #{}]
      (let [{:as state :keys [positions times closed preasure paths]} (.poll queue)]
        (cond
          (seen [(set positions) preasure])
          (recur seen)

          (or (empty? closed) (= 0 (apply max times)))
          state

          :else
          (do
            (.addAll queue
                     (let [index (index-of-max times)
                           valve (positions index)
                           time  (times index)]
                       (for [[valve dist] (distances valve)
                             :when (closed valve)
                             :let [positions (assoc positions index valve)
                                   time      (max 0 (- time dist))
                                   times     (assoc times index time)
                                   closed    (disj closed valve)
                                   preasure  (+ preasure (* time (:rate (graph valve))))]
                             :when (not (seen [(set positions) preasure]))]
                         {:positions positions
                          :paths     (update paths index #(conj % valve))
                          :times     times
                          :closed    closed
                          :preasure  preasure
                          :priority  (+ preasure (* 10 (count closed) (apply max times)))})))
            (recur (conj seen [(set positions) preasure]))))))))

(defn solution [input start times]
  (let [graph         (parse-graph input)
        closed-valves (->> graph (keep (fn [[v {:keys [rate]}]] (when (< 0 rate) v))) set)
        state         (release-valves graph closed-valves start times)]
    (:preasure state)))

(defn part1 [input]
  (solution input [:AA] [30]))

(defn part2 [input]
  (solution input [:AA :AA] [26 26]))
