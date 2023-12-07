(ns day07
  (:require
    [clojure.string :as str]
    [clojure.set :as set]))


(defn face->index [faces]
  (->> faces
       (map-indexed (fn [i f] [f i]))
       (into {})))


(defn parse-hands [input]
  (for [line (str/split-lines input)]
    (let [[hand score] (str/split line #" ")]
      {:hand hand
       :score (parse-long score)})))


(defn- hand-type [f]
  (let [fi (set/map-invert f)]
    (cond
      (fi 5) 6 ; five
      (fi 4) 5 ; four
      (and (fi 3) (fi 2)) 4 ; full house
      (fi 3) 3 ; three
      (and (fi 2) (= 3 (count f))) 2 ; two pairs
      (and (fi 2) (= 4 (count f))) 1 ; pair
      :else 0 ; high
    )))


(defn part1 [input]
  (->> (parse-hands input)
       (mapv (fn [{:keys [hand] :as h}]
               (assoc h
                 :face-indexes (mapv (face->index "23456789TJQKA") hand)
                 :type (hand-type (frequencies hand)))))
       (sort-by (juxt :type :face-indexes))
       (map-indexed (fn [i {:keys [score]}] (* (inc i) score)))
       (reduce +)))


(defn part2 [input]
  (->> (parse-hands input)
       (mapv (fn [{:keys [hand] :as h}]
               (let [most-frequent-face (->> (frequencies hand)
                                             (sort-by second >)
                                             (mapv first)
                                             (remove #{\J})
                                             first)
                     best-hand (str/escape hand {\J most-frequent-face})]
                 (assoc h
                        :face-indexes (mapv (face->index "J23456789TQKA") hand)
                        :type (hand-type (frequencies best-hand))))))
       (sort-by (juxt :type :face-indexes))
       (map-indexed (fn [i {:keys [score]}] (* (inc i) score)))
       (reduce +)))
