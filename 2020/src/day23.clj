(ns day23)

(defn- dec-modulo [m n]
  (mod (dec n) (inc m)))

(defn circle [xs]
  (let [prev (int-array (inc (count xs)) 0)
        next (int-array (inc (count xs)) 0)]
    (doseq [[p v n] (take (count xs) (partition 3 1 (cycle xs)))]
      (aset prev v ^int p)
      (aset next v ^int n))
    {:cursor (first xs)
     :prev prev
     :next next}))

(defn rotate-to [m dest]
  (assoc m :cursor dest))

(defn shrink [{:keys [cursor next prev] :as circle}]
  (let [next (ints next)
        prev (ints prev)
        n (aget next cursor)
        p (aget prev cursor)]
    (aset prev n p)
    (aset next p n)
    (assoc circle :cursor n)))

(defn add [{:keys [cursor next prev] :as circle} v]
  (let [next (ints next)
        prev (ints prev)
        n (aget next cursor)]
    (doto prev (aset v ^int cursor) (aset n ^int v))
    (doto next (aset cursor ^int v) (aset v n))
    (assoc circle :cursor v)))

(defn append [{:keys [cursor prev] :as circle} v]
  (let [p (aget (ints prev) ^int cursor)]
    (-> circle
        (rotate-to p)
        (add v)
        (rotate-to cursor))))

(defn cut [{:keys [cursor next]}]
  (letfn [(go [c]
              (let [n (aget (ints next) c)]
                (when (not= n cursor)
                  (cons n (lazy-seq (go n))))))]
    (cons cursor (go cursor))))

(defn- move [m all]
  (let [[head rest] ((juxt :cursor shrink) all)
        [a rest] ((juxt :cursor shrink) rest)
        [b rest] ((juxt :cursor shrink) rest)
        [c rest] ((juxt :cursor shrink) rest)
        next (:cursor rest)
        destination (->> head
                         (iterate #(dec-modulo m %))
                         (drop-while #{0 a b c head})
                         first)]
    (-> rest
        (rotate-to destination)
        (add a)
        (add b)
        (add c)
        (rotate-to next)
        (append head))))

(defn- after [destination xs]
  (->> (rotate-to xs destination) shrink cut))

(defn part1 [input]
  (apply str (after 1 (nth (iterate (partial move (apply max input)) (circle input)) 100))))

(defn part2 [input]
  (let [m 1000000
        input (concat input (range 10 (inc m)))]
    (apply * (take 2 (after 1 (nth (iterate (partial move m) (circle input)) 10000000))))))
