(ns day21
  (:require [clojure.string :as str]))

(defn parse-tree [input]
  (->> (for [line (str/split-lines input)
             :let [[monkey expr] (str/split line #": ")
                   monkey (keyword monkey)
                   expr (or (parse-long expr) (mapv keyword (str/split expr #"\s")))]]
         [monkey expr])
       (into {})))

(defn evl [tree k]
  (let [e (tree k)]
    (cond
      (number? e)  e
      (keyword? e) (evl tree e)
      :else
      (let [[a op b] e]
        (({:+ + :- - :/ / :* *} op) (evl tree a) (evl tree b))))))

(defn path-between [tree from to]
  (loop [[path & queue] [[from]]]
    (if (= (peek path) to)
      path
      (let [e (tree (peek path))]
        (if (vector? e)
          (let [[a _ b] e]
            (recur (concat queue [(conj path a) (conj path b)])))
          (recur queue))))))

(defn reroot [tree new-root]
  (let [[a _ b] (tree :root)
        tree    (assoc tree :root [a := b])
        path    (reverse (path-between tree :root new-root))]
    (-> (reduce
         (fn [tree [unknown known]]
           (let [[a op b] (tree known)
                 inverted (cond
                            (and (= op :*) (= unknown a)) [known :/ b]
                            (and (= op :*) (= unknown b)) [known :/ a]
                            (and (= op :+) (= unknown a)) [known :- b]
                            (and (= op :+) (= unknown b)) [known :- a]
                            (and (= op :-) (= unknown a)) [known :+ b]
                            (and (= op :-) (= unknown b)) [a :- known]
                            (and (= op :/) (= unknown a)) [known :* b]
                            (and (= op :/) (= unknown b)) [a :/ known]
                            (and (= op :=) (= unknown a)) b
                            (and (= op :=) (= unknown b)) a)]
             (assoc tree unknown inverted)))
         tree (partition 2 1 path))
        (assoc :root new-root))))

(defn part1 [input]
  (-> (parse-tree input)
      (evl :root)))

(defn part2 [input]
  (-> (parse-tree input)
      (reroot :humn)
      (evl :root)))
