(ns day23)

(defn- dec-modulo [m n]
  (mod (dec n) (inc m)))

(defn circle [xs]
  {:cursor (first xs)
   :items (->> (for [[prev v next] (take (count xs) (partition 3 1 (cycle xs)))]
                 [v {:prev prev :next next}])
               (into {}))})

(defn rotate-to [m dest]
  (assoc m :cursor dest))

(defn shrink [{:keys [cursor items]}]
  (let [{:keys [next prev]} (get items cursor)]
    {:cursor next
     :items (-> items
                (assoc-in [next :prev] prev)
                (assoc-in [prev :next] next))}))

(defn add [{:keys [cursor items]} v]
  (let [next (get-in items [cursor :next])]
    {:cursor v
     :items (-> items
                (assoc-in [cursor :next] v)
                (assoc v {:prev cursor :next next})
                (assoc-in [next :prev] v))}))

(defn append [{:keys [cursor] :as circle} v]
  (let [prev (get-in circle [:items cursor :prev])]
    (-> circle
        (rotate-to prev)
        (add v)
        (rotate-to cursor))))

(defn cut [{:keys [cursor items]}]
  (letfn [(go [items c]
              (let [next (get-in items [c :next])]
                (when (not= next cursor)
                  (cons next (lazy-seq (go items next))))))]
    (cons cursor (go items cursor))))

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
