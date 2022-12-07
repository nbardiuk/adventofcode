(ns day07
  (:require [clojure.string :as str]))

(defn parse-tree [input]
  (->> (str/split input #"\$\s")
       (reduce (fn [[tree pwd] interaction]
                 (let [[command & output-lines] (str/split-lines interaction)
                       [exe arg] (str/split command #"\s")]
                   (case exe
                     "cd" (case arg
                            "/"  [tree ["/"]]
                            ".." [tree (pop pwd)]
                            #__  [tree (conj pwd arg)])
                     "ls" [(reduce (fn [tree output-line]
                                     (let [[size name] (str/split output-line #"\s")
                                           path (conj pwd name)]
                                       (case size
                                         "dir" (assoc-in tree path {})
                                         #__   (assoc-in tree path (parse-long size)))))
                                   tree
                                   output-lines)
                           pwd]
                     #__  [tree pwd])))
               [{} []])
       first))

(defn file-sizes [directory tree]
  (->> tree
       (mapcat
        (fn [[name v]]
          (let [path (conj directory name)]
            (cond
              (map? v)    (file-sizes path v)
              (number? v) [[path v]]))))))

(defn parents [path]
  (let [parent (pop path)]
    (when (seq parent)
      (into [parent] (parents parent)))))

(defn directory-sizes [tree]
  (->> tree
       (file-sizes [])
       (mapcat (fn [[file size]]
                 (for [directory (parents file)]
                   [directory size])))
       (reduce (fn [sizes [directory size]]
                 (update sizes directory (fnil #(+ % size) 0)))
               {})))

(defn part1 [input]
  (let [sizes (vals (directory-sizes (parse-tree input)))]
    (->> sizes
         (filter #(<= % 100000))
         (reduce +))))

(defn part2 [input]
  (let [sizes   (vals (directory-sizes (parse-tree input)))
        free    (- 70000000 (apply max sizes))
        to-free (- 30000000 free)]
    (->> (sort sizes)
         (drop-while #(< % to-free))
         first)))

