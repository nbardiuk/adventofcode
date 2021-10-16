(ns aoc.day07
  (:require [clojure.string :as str :refer [split-lines]]))

(defn- hypernets [ipv7]
  (re-seq #"\[[^\]]*\]" ipv7))

(defn- supernets [ipv7]
  (re-seq #"(?:^|\])[^\[]*(?:\[|$)" ipv7))

(defn- abba? [s]
  (let [[match a b] (re-find #"(\w)(\w)\2\1" s)]
    (and match (not= a b))))

(defn- aba [s]
  (for [[match a b] (re-seq #"(?=(\w)(\w)\1)" s)
        :when (and match (not= a b))]
    [a b a]))

(defn- bab? [[a b _] s]
  (str/includes? s (str b a b)))

(defn- supports-tls? [ipv7]
  (and (some abba? (supernets ipv7))
       (not-any? abba? (hypernets ipv7))))

(defn- supports-ssl? [ipv7]
  (->> (mapcat aba (supernets ipv7))
       (some #(some (partial bab? %) (hypernets ipv7)))))

(defn part1 [input]
  (->> (split-lines input)
       (filter supports-tls?)
       count))

(defn part2 [input]
  (->> (split-lines input)
       (filter supports-ssl?)
       count))
