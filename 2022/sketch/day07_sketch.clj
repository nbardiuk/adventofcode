(ns day07-sketch
  (:require
   [clojure.java.io :as io]
   [clojure2d.core :as c2d]
   [clojure2d.color :as color]
   [fastmath.core :as math]
   [clojure.string :as str]
   [colors :as colors]
   [day07 :as solution]))

(def input (slurp (io/resource "input07.txt")))

(def instructions-progress
  (->> (str/split input #"\$\s")
       (reductions #(str/join "$ " %&))
       vec))

(def arcs-perlevel-progress
  (vec (for [input instructions-progress]
         (let [tree (solution/parse-tree input)
               sizes (-> (solution/directory-sizes tree)
                         (assoc [] 70000000))]
           (->
            (->> (range 1 13)
                 (reduce (fn [arcs level]
                           (->> sizes
                                (filter (fn [[path _]] (= level (count path))))
                                (group-by (fn [[path _]] (pop path)))
                                (reduce (fn [arcs [parent children]]
                                          (let [[parent-start parent-len parent-size] (arcs parent)]
                                            (->> children
                                                 (reduce (fn [[arcs start] [path size]]
                                                           (let [len (double (* (/ size parent-size) parent-len))]
                                                             [(assoc arcs path [start len size]) (+ start len)]))
                                                         [arcs parent-start])
                                                 first)))
                                        arcs)))
                         {[] [0.0 math/TWO_PI 70000000]})
                 (group-by (fn [[path]] (count path))))
            (update-vals #(into {} %)))))))

(defn draw [canvas _ ^long frameno _]
  (c2d/set-background canvas colors/aoc-background)
  (c2d/set-font canvas "Iosevka")
  (c2d/set-font-attributes canvas 20.)
  (let [loop (* 30 10)
        frame (mod frameno loop)
        cursor (* (count arcs-perlevel-progress) (/ frame (* loop 0.9)))
        arcs-perlevel (nth arcs-perlevel-progress cursor (last arcs-perlevel-progress))
        arc-x 420
        arc-y 550
        legend-x 520
        legend-y 650
        rk 50
        total (get-in arcs-perlevel [0 [] 2] 0)
        used (get-in arcs-perlevel [1 ["/"] 2] 0)
        small (->> arcs-perlevel
                   (mapcat second)
                   (map #(get-in % [1 2] 0))
                   (filter #(< % 100000))
                   (reduce + 0))
        to-free (max (- 30000000 (- total used)) 0)
        to-del (if (zero? to-free)
                 0
                 (->>
                  arcs-perlevel
                  (mapcat second)
                  (map #(get-in % [1 2] 0))
                  (sort)
                  (drop-while #(< % to-free))
                  first))]

    (doseq [level (range 12 0 -1)
            [_path [start length size]] (arcs-perlevel level)]
      (c2d/set-color canvas colors/aoc-dark-grey)
      (c2d/rarc canvas arc-x arc-y (* (inc level) rk) start (- length) :pie true)
      (c2d/set-color canvas (cond
                              (< size 100000) colors/aoc-silver
                              (= size to-del) colors/aoc-gold
                              :else (color/brighten colors/aoc-dark-green (/ level 5))))
      (c2d/rarc canvas arc-x arc-y (* (inc level) rk) start (- length) :pie false))

    (c2d/set-color canvas colors/aoc-brown)
    (c2d/rarc canvas arc-x arc-y rk 0 math/TWO_PI :pie false)

    (c2d/set-color canvas colors/aoc-brown)
    (c2d/text canvas (format "Disk size:  %.2f G" (/ total 1000000.0)) legend-x legend-y)
    (c2d/set-color canvas colors/aoc-light-green)
    (c2d/text canvas (format "Total used: %.2f G" (/ used 1000000.0)) legend-x (+ 25 legend-y))
    (c2d/set-color canvas colors/aoc-silver)
    (c2d/text canvas (format "Small dirs: %.2f G" (/ small 1000000.0)) legend-x (+ 60 legend-y))
    (c2d/set-color canvas colors/aoc-gold)
    (c2d/text canvas (format "Delete dir: %.2f G" (/ to-del 1000000.0)) legend-x (+ 85 legend-y))

    #_(c2d/save-image (c2d/get-image canvas) (format "images/%03d.png" frame))))

(comment
  (c2d/show-window
   {:canvas (c2d/canvas 1000 1000 :highest)
    :window-name (str *ns*)
    :hint :highest
    :fps 30
    :draw-fn #'draw}))

