(ns day04
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.walk :refer [keywordize-keys]]))

(defn- parse-docs [input]
  (for [doc (str/split input #"\n\n")]
    (->> (str/split doc #"\n| ")
         (map #(str/split % #":"))
         (into {})
         keywordize-keys)))

(defn- parse-int [s]
  (try
    (Integer/parseInt s)
    (catch Exception _ nil)))

(defn- int-range [s min max]
  (when-let [val (parse-int s)]
    (when (<= min val max)
      val)))

(defn- re-match [s re]
  (when s
    (->> s (re-matches re) second)))

(defn- drop-nils [m]
  (->> m (filter second) (into {})))

(defn- parse-passport [passport]
  (-> passport
      (update :byr int-range 1920 2002)
      (update :iyr int-range 2010 2020)
      (update :eyr int-range 2020 2030)
      (update :hgt #(or (-> % (re-match #"^(\d+)cm$") (int-range 150 193))
                        (-> % (re-match #"^(\d+)in$") (int-range 59 76))))
      (update :hcl re-match #"^(#[0-9a-f]{6})$")
      (update :ecl re-match #"^(amb|blu|brn|gry|grn|hzl|oth)$")
      (update :pid re-match #"^([0-9]{9})$")
      drop-nils))

(defn- full-doc? [doc]
  (->> doc keys set
       (set/subset? #{:byr :iyr :eyr :hgt :hcl :ecl :pid})))

(defn- count-full-docs [parser input]
  (->> (parse-docs input)
       (map parser)
       (filter full-doc?)
       count))

(def part1 #(count-full-docs identity %))
(def part2 #(count-full-docs parse-passport %))
