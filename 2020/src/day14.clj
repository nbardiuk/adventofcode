(ns day14
  (:require [clojure.string :as str]))

(defn- read-program [input]
  (for [line (str/split-lines input)
        :let [[k v] (str/split line #" = ")]]
    (case k
      "mask" [:mask (->> (reverse v)
                         (map-indexed
                          (fn [index ch]
                            [index ({\0 0 \1 1 \X :floating} ch)])))]
      (let [[_ index] (re-find #"(\d+)" k)]
        [:mem [(read-string index) (read-string v)]]))))

(defn masked [mask value]
  (->> mask
       (reduce
        (fn [value [i v]]
          (case v
            1 (bit-set value i)
            0 (bit-clear value i)
            value))
        value)))

(defn fluctuations [floats]
  (for [variant (range (Math/pow 2 (count floats)))]
    (->> floats
         (map-indexed
          (fn [n [i _]]
            [i (if (bit-test variant n) 1 0)])))))

(defn masks [mask]
  (let [floats (filter (comp #{:floating} second) mask)
        ones (filter (comp #{1} second) mask)]
    (for [fluc (fluctuations floats)]
      (concat ones fluc))))

(defn- memory-value-decoder [memory mask [address value]]
  (assoc memory address (masked mask value)))

(defn- memory-address-decoder [memory mask [address value]]
  (->> (masks mask)
       (reduce
        (fn [memory mask]
          (assoc memory (masked mask address) value))
        memory)))

(defn- init-memory [decoder instructions]
  (->> instructions
       (reduce
        (fn [{:keys [mask] :as result} [op arg]]
          (case op
            :mask (assoc result :mask arg)
            :mem (update result :memory decoder mask arg)))
        {:memory {} :mask []})))

(defn solution [decoder input]
  (->> (read-program input)
       (init-memory decoder)
       :memory vals
       (apply +)))

(defn part1 [input]
  (solution memory-value-decoder input))

(defn part2 [input]
  (solution memory-address-decoder input))
