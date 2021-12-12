(ns day12)

(defn graph [input]
  (-> (for [[_ a b] (re-seq #"(\w+)-(\w+)" input)]
        {a [b]
         b [a]})
      (#(apply merge-with concat %))
      (update-vals #(->> % (remove #{"start"}) vec))))

(defn small? [cave]
  (re-find #"^[a-z]" cave))

(defn lazy-paths [extra-permision connections paths]
  (lazy-seq
   (when (seq paths)
     (let [paths (for [{:keys [caves small-freq]} paths
                       cave (connections (peek caves))
                       :let [small-cave? (small? cave)]
                       :when (or (not small-cave?)
                                 (not (small-freq cave))
                                 (extra-permision small-freq))]
                   {:caves (conj caves cave)
                    :status (if (= "end" cave) :complete :incomplete)
                    :small-freq (cond-> small-freq
                                  small-cave? (update cave (fnil inc 0)))})
           {:keys [complete incomplete]} (group-by :status paths)]
       (concat complete
               (lazy-paths extra-permision connections incomplete))))))

(defn solution [extra-permision input]
  (count (lazy-paths extra-permision
                     (graph input)
                     [{:caves ["start"] :small-freq {}}])))

(def part1 (partial solution (constantly false)))
(def part2 (partial solution #(every? #{1} (vals %))))
