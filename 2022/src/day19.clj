(ns day19
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn parse-blueprints [input]
  (->> (for [blueprint (str/split-lines input)
             :let [[name & robots] (str/split blueprint #"(:|\.)\s*")]]
         [(parse-long (re-find #"\d+" name))
          (->> (for [robot robots
                     :let [[name & resources] (str/split robot #" costs | and ")]]
                 [(keyword (second (str/split name #"\s")))
                  (->> (for [r resources
                             :let [[amount name] (str/split r #"\s")]]
                         [(keyword name) (parse-long amount)])
                       (into {}))])
               (into {}))])))

(defn submap? [a b]
  (every? (fn [[k v]] (<= v (get b k 0))) a))

(defn keep-best [states]
  (let [maxg (transduce (map #(-> % :robots :geode)) max 0 states)
        maxo (transduce (map #(-> % :robots :obsidian)) max 0 states)
        xf (filter #(and (<= (- maxg 1) (-> % :robots :geode))
                         (<= (- maxo 3) (-> % :robots :obsidian))))]
    (into #{} xf states)))

(defn next-states [blueprint {:as state :keys [robots resources]}]
  (let [next (for [[robot requirements] blueprint
                   :when (submap? requirements resources)]
               {:robots    (update robots robot inc)
                :resources (merge-with - resources requirements)})]
    (->> (if (= 4 (count next)) next (cons state next))
         (map #(update % :resources (partial merge-with +) robots)))))

(defn crack-geodes [time blueprint]
  (loop [time time
         states #{{:robots    {:ore 1 :clay 0 :obsidian 0 :geode 0}
                   :resources {:ore 0 :clay 0 :obsidian 0 :geode 0}}}
         seen #{}]
    (if (= 0 time)
      (transduce (map #(-> % :resources :geode)) max 0 states)
      (let [states (->> states (mapcat #(next-states  blueprint %)) keep-best)]
        (recur (dec time) (set/difference states seen) (set/union states seen))))))

(defn part1 [input]
  (->> (parse-blueprints input)
       (map (fn [[id blueprint]] (* id (crack-geodes 24 blueprint))))
       (reduce +)))

(defn part2 [input]
  (->> (parse-blueprints input)
       (take 3)
       (map (fn [[_id blueprint]] (crack-geodes 32 blueprint)))
       (reduce *)))
