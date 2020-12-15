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

(defn- masked [mask value]
  (reduce
   (fn [value [i v]]
     (case v
       1 (bit-set value i)
       0 (bit-clear value i)
       value))
   value mask))

(defn fluctuations [floats]
  (for [variant (range (Math/pow 2 (count floats)))]
    (->> floats
         (map-indexed
          (fn [n [i _]]
            [i (if (bit-test variant n) 1 0)])))))

(defn- floating-masks [mask]
  (let [floats (filter (comp #{:floating} second) mask)
        ones (filter (comp #{1} second) mask)]
    (for [fluc (fluctuations floats)]
      (concat ones fluc))))

(defn- memory-value-decoder [memory masks [address value]]
  (reduce
   (fn [memory mask]
     (assoc memory address (masked mask value)))
   memory masks))

(defn- memory-address-decoder [memory masks [address value]]
  (reduce
   (fn [memory mask]
     (assoc memory (masked mask address) value))
   memory masks))

(defn- init-memory [decoder mask-fun instructions]
  (reduce
   (fn [{:keys [masks] :as result} [op arg]]
     (case op
       :mask (assoc result :masks (mask-fun arg))
       :mem (update result :memory decoder masks arg)))
   {:memory {} :masks []} instructions))

(defn- solution [decoder mask-fun input]
  (->> (read-program input)
       (init-memory decoder mask-fun)
       :memory vals
       (reduce +)))

(defn part1 [input]
  (solution memory-value-decoder vector input))

(defn part2 [input]
  (solution memory-address-decoder floating-masks input))
