(ns day24)

(def input
  [[1   15 15]
   [1   12  5]
   [1   16  6]
   [26 -14  7]
   [1   15  9]
   [26  -7  9]
   [1   14 14]
   [1   15  3]
   [1   15  1]
   [26  -7  3]
   [26  -8  4]
   [26  -7  6]
   [26  -5  7]
   [26 -10  1]])

(defn step [z input q a b]
  (if (= input (+ a (mod z 26)))
    (quot z q)
    (+ input b (* 26 (quot z q)))))

(defn solution [criteria]
  (->
   (reduce
    (fn [result [q a b]]
      (->> (for [[z inputs] result
                 input (range 1 10)]
             [(step z input q a b) (+ input (* 10 inputs))])
           (reduce
            (fn [result [z inputs]]
              (update result z (fnil criteria inputs) inputs))
            {})))
    {0 0} input)
   (get 0)))

(def part1 #(solution max)) ; 49917929934999
(def part2 #(solution min)) ; 11911316711816
