(ns sketches.day16
  (:require [clojure2d.core :as c2d]
            [fastmath.core :as math]
            [day00 :refer [take-until-repeat]]
            [day16 :refer [hex->bin parse-packet]]
            [day16-test :refer [my-input]]))

(def input my-input)
(def bits (hex->bin input))

(def packet (first (parse-packet bits)))

(defn tree [{:keys [type-id body]}]
  (case type-id
    4 {:label body :done? false}
    0 {:label '+ :nodes (mapv tree body)}
    1 {:label '* :nodes (mapv tree body)}
    2 {:label 'min :nodes (mapv tree body)}
    3 {:label 'max :nodes (mapv tree body)}
    5 {:label '> :nodes (mapv tree body)}
    6 {:label '< :nodes (mapv tree body)}
    7 {:label '= :nodes (mapv tree body)}))

(def expression-tree (tree packet))

(defn bool->int [b]
  (get {true 1 false 0} b b))

(def steps
  (let [r (vec
           (concat
            (->> expression-tree
                 (iterate
                  (fn dissoc-once [{:keys [nodes] :as n}]
                    (let [c (count nodes)]
                      (cond
                        (not (seq nodes)) n

                        (seq (:nodes (last nodes)))
                        (update-in n [:nodes (dec c)] dissoc-once)

                        (not (seq (:nodes (last nodes))))
                        (update n :nodes (comp vec butlast))))))
                 (take-until-repeat)
                 reverse)
            (->> expression-tree
                 (iterate
                  (fn eval-once [{:keys [done? nodes label] :as n}]
                    (let [c (count nodes)]
                      (cond
                        done? n

                        (not (seq nodes))
                        (assoc n :done? true)

                        (not (:done? (last nodes)))
                        (update-in n [:nodes (dec c)] eval-once)

                        (and (< 1 c) (not (:done? (last (butlast (:nodes n))))))
                        (update-in n [:nodes (- c 2)] eval-once)

                        (and (< 2 c) (every? :done? (take-last 2 nodes)))
                        (let [[kp rd] (split-at (- c 2) nodes)]
                          {:nodes (conj (vec kp)
                                        {:done? true
                                         :label (->> rd (map :label) (reduce (eval label)) bool->int)})
                           :label label})

                        (every? :done? nodes)
                        {:done? true
                         :label (->> nodes (map :label) (reduce (eval label)) bool->int)}))))
                 (take-until-repeat))))]
    (into r (repeat 10 (last r)))))

(defn draw-branches [canvas depth [{:keys [done? label nodes]} & rest] angle sign x y]
  (let [angle (+ 5 angle)
        next-angle (+ (* sign (- 55 (* depth 3))) angle)
        w (c2d/text-width canvas (str label "   "))
        pad (c2d/text-width canvas "  ")
        [_ _ w' h'] (c2d/text-bounding-box canvas label)]
    (c2d/push-matrix canvas)
    (c2d/translate canvas x y)
    (c2d/rotate canvas (math/radians (if (seq nodes) next-angle angle)))
    (c2d/translate canvas (- 0. x) (- 0. y))
    (c2d/set-color canvas "#0f0f23" 100)
    (c2d/ellipse canvas
                 (+ x (* w' 0.8)) (- y (* 0.3 h'))
                 (+ (* 1.5 w') (* 2 pad)) (* h' 2.5))
    (c2d/set-color canvas (cond
                            done? "#ffff00"
                            (seq nodes) "#a47a4d"
                            :else "#0000dd"))
    (c2d/ellipse canvas (+ x (/ w' 2)) (- y (* 0.3 h'))
                 (+ w' (* 2 pad)) (* h' (if (seq nodes) 2 1.5)))

    (c2d/set-color canvas (if done? "#0f0f23" "#ffff00"))
    (c2d/text canvas label x y)
    (c2d/pop-matrix canvas)

    (when nodes
      (let [x' (+ x (* w (math/cos (math/radians next-angle))))
            y' (+ y (* w (math/sin (math/radians next-angle))))]
        (draw-branches canvas (inc depth) nodes next-angle sign x' y')))

    (when (seq rest)
      (let [x' (+ x (* w (math/cos (math/radians angle))))
            y' (+ y (* w (math/sin (math/radians angle))))]
        (draw-branches canvas depth rest angle (* -1. sign) x' y')))))

(defn draw [canvas _ ^long frameno _]
  (c2d/set-background canvas "#0f0f23")
  (c2d/set-font canvas "Iosevka")
  (c2d/set-font-attributes canvas 20.)
  (let [index (mod frameno (count steps))
        tree (nth steps index)]
    (draw-branches canvas 1 [tree] -90.0 1. 250. 250.)
    #_(c2d/save-image (c2d/get-image canvas) (format "images/day16-%03d.jpg" index))))

(comment
  (c2d/show-window
   {:canvas (c2d/canvas 1500 1500 :highest)
    :window-name "day16"
    :hint :highest
    :fps 30
    :h 1000
    :w 1000
    :draw-fn #'draw}))
