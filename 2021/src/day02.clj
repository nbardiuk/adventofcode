(ns day02)

(defn- parse-commands [input]
  (for [[_ name amount] (re-seq #"(\w+)\s+(\d+)" input)]
    {:name (keyword name)
     :amount (parse-long amount)}))

(def start-position
  {:aim 0 :horizontal 0 :depth 0})

(defn- checksum [{:keys [horizontal depth]}]
  (* horizontal depth))

(defn- step1 [position {:keys [name amount]}]
  (case name
    :forward (update position :horizontal + amount)
    :down    (update position :depth + amount)
    :up      (update position :depth - amount)))

(defn- step2 [position {:keys [name amount]}]
  (case name
    :forward (-> position
                 (update :horizontal + amount)
                 (update :depth + (* (:aim position) amount)))
    :down    (update position :aim  + amount)
    :up      (update position :aim  - amount)))

(defn- solution [step input]
  (->> (parse-commands input)
       (reduce step start-position)
       checksum))

(def part1 (partial solution step1))
(def part2 (partial solution step2))
