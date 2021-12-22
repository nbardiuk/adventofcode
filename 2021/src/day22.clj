(ns day22
  (:require [clojure.string :as string]))

(defn parse [input]
  (for [line (string/split-lines input)
        :let [step {:flag (keyword (re-find #"\w+" line))}]]
    (->> (re-seq #"-?\d+" line)
         (map parse-long)
         (partition 2)
         (map vector [:x :y :z])
         (into step))))

(defn sub-1d [[amn amx] [bmn bmx]]
  (cond
    (or (< amx bmn) (< bmx amn))    [{:range [amn amx] :in? true}]
    (and (<= bmn amn) (<= amx bmx)) [{:range [amn amx] :in? false}]
    (<= bmn amx bmx)                [{:range [amn (dec bmn)] :in? true}
                                     {:range [bmn amx] :in? false}]
    (<= bmn amn bmx)                [{:range [amn bmx] :in? false}
                                     {:range [(inc bmx) amx] :in? true}]
    (and (< amn bmn) (< bmx amx))   [{:range [amn (dec bmn)] :in? true}
                                     {:range [bmn bmx] :in? false}
                                     {:range [(inc bmx) amx] :in? true}]))

(defn sub-3d [a b]
  (for [{in-x :in? x :range} (sub-1d (:x a) (:x b))
        {in-y :in? y :range} (sub-1d (:y a) (:y b))
        {in-z :in? z :range} (sub-1d (:z a) (:z b))
        :when (or in-x in-y in-z)]
    {:x x :y y :z z}))

(defn merge-along [i j k steps]
  (->> (group-by (juxt i j) steps)
       vals
       (mapcat
        (fn [group]
          (->> (sort-by k group)
               (reduce
                (fn [group b]
                  (let [updated (for [a group]
                                  (cond
                                    (= (inc (get-in b [k 1])) (get-in a [k 0]))
                                    (-> a (assoc-in [k 0] (get-in b [k 0])))

                                    (= (inc (get-in a [k 1])) (get-in b [k 0]))
                                    (-> b (assoc-in [k 0] (get-in a [k 0])))

                                    :else a))]
                    (cond-> updated
                      (= updated group) (conj b))))
                []))))
       vec))

(defn len [[a b]] (inc (- b a)))

(defn volume [{:keys [x y z]}]
  (->> [x y z] (map len) (reduce *)))

(defn total-volume [cuboid]
  (->> cuboid
       (map volume)
       (reduce +)))

(defn sub-all [cuboids cuboid]
  (->> cuboids
       (mapcat #(sub-3d % cuboid))
       (merge-along :x :y :z)
       (merge-along :x :z :y)
       (merge-along :z :y :x)))

(defn step [on step]
  (case (:flag step)
    :on (into on (reduce sub-all [step] on))
    :off (sub-all on step)))

(defn part2 [input]
  (->> (parse input)
       (reduce step [])
       total-volume))

(defn part1 [input]
  (->> (parse input)
       (filter (fn [{:keys [x y z]}] (every? #(<= -50 % 50) (flatten [x y z]))))
       (reduce step [])
       total-volume))
