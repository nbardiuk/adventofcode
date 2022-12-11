(ns day11-sketch
  (:require
   [clojure.java.io :as io]
   [clojure2d.core :as c2d]
   [fastmath.core :as math]
   [colors :as colors]
   [day11 :as solution]))

(def monkeys
  (solution/parse-notes (slurp (io/resource "input11.txt"))))

(def divisor
  (->> monkeys (map :divisor) (reduce *)))

(defn item-norm [i]
  (mod i divisor))

(def steps
  (->> {:items (->> monkeys (map (juxt :id :items)) (into {}))
        :steps []}
       (iterate
        (fn [state]
          (reduce
           (fn [{:keys [items steps]} {:keys [id] :as monkey}]
             (let [monkey-items (items id)
                   [iterations [items]] (->> items
                                             (iterate (partial solution/throw-item item-norm monkey))
                                             (split-at (count monkey-items)))]
               {:items items
                :steps (->> (map (fn [items item]
                                   {:items items
                                    :current-id id
                                    :to-id      (->> item
                                                     (solution/update-item item-norm monkey)
                                                     (solution/destination monkey))})
                                 iterations monkey-items)
                            (into steps))}))
           state
           monkeys)))
       (#(nth % 20))
       :steps))
(def stats (mapv frequencies (reductions conj [] (map :current-id steps))))

(defn draw-items [canvas x y items]
  (doseq [id (range (count items))]
    (let [s 23
          id (+ 2 id)
          p (math/sqrt id)
          r (* 15 p)
          th (+ 3 (* math/PI p))
          item-x (+ x (* r (math/cos th)))
          item-y (+ y (* r (math/sin th)))]
      (c2d/ellipse canvas item-x item-y s s))))

(defn draw [canvas _ ^long frameno _]
  (c2d/set-background canvas colors/aoc-background)
  (c2d/set-font canvas "Iosevka")
  (c2d/set-font-attributes canvas 20.)
  (let [loop (* 30 10)
        frame (inc (mod frameno loop))
        {:keys [items current-id to-id]} (get steps (long (* (count steps) (/ frame (- loop 20)))) (last steps))
        stats (get stats (long (* (count stats) (/ frame (- loop 20)))) (last stats))
        m (count monkeys)
        monkeys-rx 350
        monkeys-ry 150
        da (/ math/TWO_PI m)
        sa (- (* math/TWO_PI (/ frame loop)))
        cx 500
        cy 750
        s 50
        monkey-x (fn [id] (+ cx (* monkeys-rx (math/cos (+ sa (* da id))))))
        monkey-y (fn [id] (+ cy (* monkeys-ry (math/sin (+ sa (* da id))))))]

    (when to-id
      (c2d/set-color canvas colors/aoc-dark-grey)
      (c2d/set-stroke canvas 6)
      (c2d/line canvas
                (monkey-x current-id) (monkey-y current-id)
                (monkey-x to-id) (monkey-y to-id)))

    (doseq [{:keys [id]} monkeys]
      (c2d/set-color canvas colors/aoc-brown)
      (c2d/ellipse canvas (monkey-x id) (monkey-y id) s s)
      (c2d/set-color canvas colors/aoc-grey)
      (c2d/text canvas id (- (monkey-x id) 5) (+ (monkey-y id) 6)))

    (doseq [[id items] items]
      (c2d/set-color canvas colors/aoc-grey)
      (draw-items canvas (monkey-x id) (monkey-y id) items))

    (doseq [[id fr] stats]
      (c2d/set-color canvas colors/aoc-gold)
      (c2d/text canvas fr (+ 135 (* id 100)) (- 345 fr))
      (c2d/set-color canvas colors/aoc-grey)
      (c2d/rect canvas (+ 100 (* id 100)) (- 350 fr) 97 fr)
      (c2d/set-color canvas colors/aoc-brown)
      (c2d/ellipse canvas (+ 150 (* id 100)) 380 s s)
      (c2d/set-color canvas colors/aoc-grey)
      (c2d/text canvas id (+ 145 (* id 100)) 386))

    #_(c2d/save-image (c2d/get-image canvas) (format "images/%03d.png" frame))))

(comment
  (c2d/show-window
   {:canvas (c2d/canvas 1000 1000 :highest)
    :window-name (str *ns*)
    :hint :highest
    :fps 30
    :draw-fn #'draw}))
